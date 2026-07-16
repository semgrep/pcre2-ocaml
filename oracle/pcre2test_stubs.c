/* Dev-only stubs backing the pcre2test-compatible harness's C-oracle driver.
 *
 * Unlike the production bindings (pcre2_c_stubs.c), these expose everything
 * the harness needs to reproduce pcre2test output byte-for-byte: compile
 * error offsets, a compile context (newline / BSR / extra options), the full
 * ovector, MARK values, and start-char. Self-contained: own custom block.
 */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

/* --- Dev-only oracle overrun guard (pattern + subject slack buffers) ------
 *
 * PCRE2 10.44's matcher and compiler contain bounded out-of-bounds reads a
 * few bytes before/after the pattern/subject that are undefined behaviour but
 * which the pure OCaml engine pins to a defined "reads-as-zero" model:
 *
 *   - Forward GETCHAR-past-end family: an invalid/truncated trailing UTF-8
 *     sequence makes GETUTF8INC read up to 5 code units past the subject end
 *     (ASan: pcre2_match.c:2221; engine pins via Utf.peek -> 0).
 *   - Backward was_newline/BACKCHAR family: under PCRE2_MATCH_INVALID_UTF the
 *     bad-start skip can walk BACKCHAR below start_subject, reading the code
 *     unit BEFORE the subject (fuzz repro 204387 / commit e8eeaa8; engine pins
 *     stop-at-(-1), read 0).
 *
 * Handed an OCaml string directly (String_val), those overruns land in the
 * adjacent OCaml block: the header word before, and NUL padding after -- so
 * they are USUALLY mapped, but (a) occasionally the string abuts an unmapped
 * page -> a rare real SIGSEGV, and (b) the last padding byte of an OCaml string
 * is the padding COUNT (1..word size), not always 0x00, so a forward overrun is
 * not even reliably zero. To make every bounded overrun read deterministic
 * 0x00 -- exactly the engine's pinned model -- copy the pattern and subject
 * into malloc'd buffers with SLACK zero bytes on BOTH sides and hand pcre2 the
 * interior pointer. The byte immediately before the subject stays 0x00, which
 * keeps the 204387 backward-walk analysis valid.
 *
 * (The unbounded GET_UCD out-of-range read that a huge decoded code point can
 * trigger is a separate C UB handled by the dev-only libpcre2 build patch in
 * oracle/patches/, not by this buffer slack.) */
#define ORACLE_OVERRUN_SLACK 8

/* Copy [len] bytes from [src] into a fresh buffer with ORACLE_OVERRUN_SLACK
 * zero bytes on each side; return the interior pointer (caller frees the base
 * via [oracle_slack_free], which recovers the base from the same offset).
 * Dev-only: on allocation failure abort() rather than return NULL -- a NULL
 * flowing into pcre2 would SIGSEGV (10.44 skips arg sanity in jit paths) and a
 * NULL-checked path would masquerade as a spurious oracle-error divergence. */
static unsigned char *oracle_slack_dup(const unsigned char *src, size_t len) {
        unsigned char *base = (unsigned char *)calloc(len + 2 * ORACLE_OVERRUN_SLACK, 1);
        if (base == NULL) {
                fprintf(stderr, "oracle_slack_dup: out of memory (len=%zu)\n", len);
                abort();
        }
        if (len != 0) memcpy(base + ORACLE_OVERRUN_SLACK, src, len);
        return base + ORACLE_OVERRUN_SLACK;
}

static void oracle_slack_free(unsigned char *interior) {
        if (interior != NULL) free(interior - ORACLE_OVERRUN_SLACK);
}

struct test_regex {
        pcre2_code *regex;
};

static void test_regex_free(value v) {
        struct test_regex *re = Data_custom_val(v);
        pcre2_code_free(re->regex);
}

static struct custom_operations test_regex_ops = {.identifier = "pcre2_ocaml_test_regexp",
                                                  .finalize = test_regex_free,
                                                  .compare = NULL,
                                                  .hash = NULL,
                                                  .serialize = NULL,
                                                  .deserialize = NULL,
                                                  .compare_ext = NULL,
                                                  .fixed_length = NULL};

static inline pcre2_code *test_code_of_value(value v) {
        return ((struct test_regex *)Data_custom_val(v))->regex;
}

/* compile : string -> int (options) -> int (newline; 0 = default)
 *         -> int (bsr; 0 = default) -> int (extra options)
 *         -> (code, int * int) result        [Error (errcode, erroroffset)] */
CAMLprim value oracle_test_compile(value pattern, value voptions, value vnewline, value vbsr,
                                   value vextra) {
        CAMLparam5(pattern, voptions, vnewline, vbsr, vextra);
        CAMLlocal3(result, regex_value, err);

        int error_code;
        PCRE2_SIZE error_offset;
        size_t pattern_len = caml_string_length(pattern);
        uint32_t options = (uint32_t)Long_val(voptions);

        pcre2_compile_context *ccontext = pcre2_compile_context_create(NULL);
        if (Long_val(vnewline) != 0)
                pcre2_set_newline(ccontext, (uint32_t)Long_val(vnewline));
        if (Long_val(vbsr) != 0)
                pcre2_set_bsr(ccontext, (uint32_t)Long_val(vbsr));
        if (Long_val(vextra) != 0)
                pcre2_set_compile_extra_options(ccontext, (uint32_t)Long_val(vextra));

        /* Slack-padded copy so any bounded compile-time overrun reads 0x00. */
        unsigned char *pat_buf =
            oracle_slack_dup((const unsigned char *)String_val(pattern), pattern_len);
        pcre2_code *regex = pcre2_compile((PCRE2_SPTR)pat_buf, pattern_len, options, &error_code,
                                          &error_offset, ccontext);
        oracle_slack_free(pat_buf);
        pcre2_compile_context_free(ccontext);

        if (!regex) {
                err = caml_alloc_small(2, 0);
                Field(err, 0) = Val_long(error_code);
                Field(err, 1) = Val_long((long)error_offset);
                result = caml_alloc_small(1, 1); /* Error */
                Field(result, 0) = err;
                CAMLreturn(result);
        }

        size_t pcre2_allocated_mem;
        pcre2_pattern_info(regex, PCRE2_INFO_SIZE, &pcre2_allocated_mem);
        regex_value =
            caml_alloc_custom_mem(&test_regex_ops, sizeof(struct test_regex), pcre2_allocated_mem);
        ((struct test_regex *)Data_custom_val(regex_value))->regex = regex;

        result = caml_alloc_small(1, 0); /* Ok */
        Field(result, 0) = regex_value;
        CAMLreturn(result);
}

/* Shared body of [oracle_test_exec] (padded, the default) and
 * [oracle_test_exec_nopad] (bench-only). One body so the mark-read guard
 * below (commit 5c790c2 -- about UNINITIALIZED match_data->mark on
 * early-return rcs, orthogonal to subject padding) provably applies on both
 * paths and cannot drift. [use_slack] selects the subject buffer:
 *   1 -> slack-padded copy (see ORACLE_OVERRUN_SLACK above; commit 13454e2);
 *   0 -> String_val(subject) handed to pcre2 directly, as the stub did
 *        before 13454e2. TIMING-ONLY -- see oracle_test_exec_nopad. */
static value test_exec_common(value vcode, value subject, value voffset, value voptions,
                              int use_slack, long match_limit, long depth_limit,
                              long heap_limit) {
        CAMLparam4(vcode, subject, voffset, voptions);
        CAMLlocal4(res, ovec_arr, mark_opt, mark_str);

        pcre2_code *regex = test_code_of_value(vcode);
        pcre2_match_data *md = pcre2_match_data_create_from_pattern(regex, NULL);
        PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(md);
        uint32_t oveccount = pcre2_get_ovector_count(md);

        /* Match pcre2test: make unset groups deterministic. */
        for (uint32_t i = 0; i < 2 * oveccount; i++) ovector[i] = PCRE2_UNSET;

        /* Padded: slack copy so any bounded subject overrun reads 0x00.
         * Unpadded: SAFETY: passing String_val(subject) directly is fine --
         * nothing between the read and pcre2_match returning allocates on the
         * OCaml heap (match_data / slack buffers use C malloc), so no GC can
         * move the string while pcre2 holds the pointer. */
        size_t subject_len = caml_string_length(subject);
        unsigned char *subj_buf =
            use_slack ? oracle_slack_dup((const unsigned char *)String_val(subject), subject_len)
                      : (unsigned char *)String_val(subject);

        /* Per-call match-context limits (pcre2test MOD_CTM modifiers). Any
         * limit >= 0 means "set"; build a context and apply the set ones
         * (pcre2_context.c:435-452), else pass NULL exactly as before. */
        pcre2_match_context *mcontext = NULL;
        if (match_limit >= 0 || depth_limit >= 0 || heap_limit >= 0) {
                mcontext = pcre2_match_context_create(NULL);
                if (mcontext == NULL) {
                        /* dev-only: abort on alloc failure, as oracle_slack_dup */
                        fprintf(stderr, "test_exec_common: out of memory (match context)\n");
                        abort();
                }
                if (match_limit >= 0) pcre2_set_match_limit(mcontext, (uint32_t)match_limit);
                if (depth_limit >= 0) pcre2_set_depth_limit(mcontext, (uint32_t)depth_limit);
                if (heap_limit >= 0) pcre2_set_heap_limit(mcontext, (uint32_t)heap_limit);
        }

        int rc = pcre2_match(regex, (PCRE2_SPTR)subj_buf, subject_len,
                             (PCRE2_SIZE)Long_val(voffset), (uint32_t)Long_val(voptions), md,
                             mcontext);
        if (use_slack) oracle_slack_free(subj_buf);
        if (mcontext != NULL) pcre2_match_context_free(mcontext);

        ovec_arr = caml_alloc(2 * oveccount, 0);
        for (uint32_t i = 0; i < 2 * oveccount; i++) {
                long v = (ovector[i] == PCRE2_UNSET) ? -1 : (long)ovector[i];
                Store_field(ovec_arr, i, Val_long(v));
        }

        /* match_data->mark is only DEFINED when pcre2_match reached the
         * post-match-loop epilogue that assigns it -- match_data->mark = mb->mark
         * for a full match and = mb->nomatch_mark otherwise (pcre2_match.c:7706,
         * 7741), reached for rc >= 0, PCRE2_ERROR_NOMATCH (-1),
         * PCRE2_ERROR_PARTIAL (-2), and the resource-limit / internal errors
         * that flow through `default: goto ENDLOOP` (pcre2_match.c:7575-7576).
         * The EARLY-return errors -- input/plausibility checks (BADOPTION/NULL/
         * BADOFFSET/BADMAGIC/BADMODE/BADOFFSETLIMIT, pcre2_match.c:6597-6619,
         * 6666,6673), BADUTFOFFSET (6839), and the UTF-validity errors
         * (6841/6902; this stub never JIT-compiles, so the SUPPORT_JIT copies at
         * 6716/6760 are unreachable here) -- return BEFORE that, leaving
         * match_data->mark exactly as
         * pcre2_match_data_create left it: UNINITIALIZED (create sets oveccount/
         * flags/heapframes only, pcre2_match_data.c:57-72). Usually that garbage
         * reads as NULL (harmless), occasionally a wild pointer -> reading
         * mark[-1] is a non-canonical dereference = general protection fault
         * (heap-state-dependent, so a fresh seed found it: ip=oracle_test_exec
         * +0x229, pcre2test_stubs.c:178, seed 20260708 @200k; an earlier GPF of
         * the same shape predates today's work). pcre2test itself reads MARK
         * only in its match / partial / nomatch output paths (pcre2test.c:
         * 8128-8136, 8143, 8246) -- a subset of the mark-defined rcs -- so read
         * mark for exactly rc >= 0 / -1 / -2 and return None otherwise, matching
         * pcre2test and the engine driver (engine_driver.ml). */
        if (rc >= 0 || rc == PCRE2_ERROR_NOMATCH || rc == PCRE2_ERROR_PARTIAL) {
                PCRE2_SPTR mark = pcre2_get_mark(md);
                if (mark == NULL) {
                        mark_opt = Val_long(0); /* None */
                } else {
                        /* MARK names may contain NULs; the length is stored in
                         * the code unit preceding the name (pcre2test.c:8131
                         * prints via PCHARSV(mark, -1, -1, ...) -> pchars8
                         * length = *p++). */
                        size_t mark_len = mark[-1];
                        mark_str = caml_alloc_initialized_string(mark_len, (const char *)mark);
                        mark_opt = caml_alloc_small(1, 0); /* Some */
                        Field(mark_opt, 0) = mark_str;
                }
        } else {
                mark_opt = Val_long(0); /* None -- mark undefined for early-return rcs */
        }

        /* startchar (an integer, not a pointer -> reading it can never fault)
         * is zeroed at pcre2_match.c:6688 before the UTF check and match loop,
         * so it is DEFINED for every rc except the early returns that precede
         * the zeroing (6597-6619, 6666, 6673); those rcs are never consumed for
         * startchar by the comparisons (fuzz_diff cmp_exec / harness read
         * startchar only for a match, PARTIAL, and the UTF-validity errors --
         * all defined: 7719/7758, 6892/6901 on the non-JIT path this stub
         * takes (JIT copies 6756/6759 unreachable here); the direct
         * BADUTFOFFSET/UTF8_ERR20 returns at 6839/6841 leave it at the 6688
         * zero), so it is left read unconditionally. */
        long startchar = (long)pcre2_get_startchar(md);
        pcre2_match_data_free(md);

        res = caml_alloc_small(4, 0);
        Field(res, 0) = Val_long(rc);
        Field(res, 1) = ovec_arr;
        Field(res, 2) = mark_opt;
        Field(res, 3) = Val_long(startchar);
        CAMLreturn(res);
}

/* exec : code -> string -> int (offset) -> int (options)
 *      -> int (match_limit) -> int (depth_limit) -> int (heap_limit)
 *      -> int (rc) * int array (ovector, unset = -1) * string option (mark)
 *      * int (startchar)
 * DEFAULT entry point (oracle/test_driver.ml): subject slack-padded, so the
 * fuzzer and the conformance runner always get the pinned reads-as-zero
 * overrun model. The three trailing limits are the match-context knobs
 * (-1 = unset -> NULL context, exactly the pre-limit behavior). 7 args (>5),
 * so OCaml needs the bytecode/native external pair below. */
CAMLprim value oracle_test_exec(value vcode, value subject, value voffset, value voptions,
                                value vmatch_limit, value vdepth_limit, value vheap_limit) {
        return test_exec_common(vcode, subject, voffset, voptions, /*use_slack=*/1,
                                Long_val(vmatch_limit), Long_val(vdepth_limit),
                                Long_val(vheap_limit));
}

CAMLprim value oracle_test_exec_byte(value *argv, int argn) {
        (void)argn; /* fixed arity 7 */
        return oracle_test_exec(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

/* exec, UNPADDED subject -- TIMING ONLY. Same signature as oracle_test_exec.
 *
 * Exists solely so the M10 perf bench (bench/bench.ml Oracle_driver) can
 * measure the C matcher instead of memcpy: the gate metric engine/oracle
 * <= 2.0x assumes oracle ~= raw-C (35c6fb6 log row), but the per-call
 * oracle_slack_dup added by 13454e2 costs O(calls x subject_len) -- measured
 * at e233089 it inflated repeat_bounded to oracle=105,349ms vs raw-C=96ms
 * (~1000x) and email to 2481ms vs 137ms, while single-call pathological
 * stayed 156ms ~= 157ms (padding-copy cost, not matcher cost). 13454e2's
 * padding itself is correct and REQUIRED for fuzz/conformance, which pin
 * PCRE2 10.44's bounded UB overruns to deterministic 0x00; this variant
 * reintroduces that UB by design, so it must only ever see the bench's
 * fixed, benign corpora (valid UTF-8 where UTF is on; no crafted invalid
 * trailing sequences).
 *
 * Containment: this symbol is deliberately NOT declared in
 * oracle/test_driver.ml -- the ONLY OCaml `external` for it lives in
 * bench/bench.ml, so the fuzzer / conformance runner cannot reach it without
 * writing a new external declaration. The mark-read guard (5c790c2) is in
 * test_exec_common and therefore applies here too. */
CAMLprim value oracle_test_exec_nopad(value vcode, value subject, value voffset, value voptions) {
        /* Bench-only: keeps its 4-arg shape; forwards unset (-1) limits so it
         * always uses the NULL/default match context. */
        return test_exec_common(vcode, subject, voffset, voptions, /*use_slack=*/0,
                                /*match_limit=*/-1, /*depth_limit=*/-1, /*heap_limit=*/-1);
}

/* info : code -> int (argoptions) * int (alloptions) * int (newline)
 *      * int (bsr) * int (capture_count) */
CAMLprim value oracle_test_info(value vcode) {
        CAMLparam1(vcode);
        CAMLlocal1(res);

        pcre2_code *regex = test_code_of_value(vcode);
        uint32_t argoptions, alloptions, newline, bsr, capture_count;
        pcre2_pattern_info(regex, PCRE2_INFO_ARGOPTIONS, &argoptions);
        pcre2_pattern_info(regex, PCRE2_INFO_ALLOPTIONS, &alloptions);
        pcre2_pattern_info(regex, PCRE2_INFO_NEWLINE, &newline);
        pcre2_pattern_info(regex, PCRE2_INFO_BSR, &bsr);
        pcre2_pattern_info(regex, PCRE2_INFO_CAPTURECOUNT, &capture_count);

        res = caml_alloc_small(5, 0);
        Field(res, 0) = Val_long((long)argoptions);
        Field(res, 1) = Val_long((long)alloptions);
        Field(res, 2) = Val_long((long)newline);
        Field(res, 3) = Val_long((long)bsr);
        Field(res, 4) = Val_long((long)capture_count);
        CAMLreturn(res);
}

/* error_message : int -> string, exactly pcre2_get_error_message. */
CAMLprim value oracle_test_error_message(value verrcode) {
        CAMLparam1(verrcode);
        PCRE2_UCHAR buf[256];
        int rc = pcre2_get_error_message((int)Long_val(verrcode), buf, sizeof(buf));
        if (rc < 0) buf[0] = 0; /* unknown code: empty, caller decides */
        CAMLreturn(caml_copy_string((const char *)buf));
}

/* name_table : code -> (string * int) array, in PCRE2 name-table order. */
CAMLprim value oracle_test_name_table(value vcode) {
        CAMLparam1(vcode);
        CAMLlocal3(arr, entry, name);

        pcre2_code *regex = test_code_of_value(vcode);
        uint32_t name_count, entry_size;
        PCRE2_SPTR table;
        pcre2_pattern_info(regex, PCRE2_INFO_NAMECOUNT, &name_count);
        pcre2_pattern_info(regex, PCRE2_INFO_NAMEENTRYSIZE, &entry_size);
        pcre2_pattern_info(regex, PCRE2_INFO_NAMETABLE, &table);

        arr = caml_alloc(name_count, 0);
        for (uint32_t i = 0; i < name_count; i++) {
                PCRE2_SPTR e = table + i * entry_size;
                /* Entry: 2-byte big-endian group number, then NUL-terminated name. */
                int group = (e[0] << 8) | e[1];
                name = caml_copy_string((const char *)(e + 2));
                entry = caml_alloc_small(2, 0);
                Field(entry, 0) = name;
                Field(entry, 1) = Val_long(group);
                Store_field(arr, i, entry);
        }
        CAMLreturn(arr);
}
