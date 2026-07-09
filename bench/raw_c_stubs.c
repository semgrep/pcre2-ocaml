/* Raw-C reference numbers for bench.ml — the "honesty number" the M10 plan
 * requires: what libpcre2-8 costs when the ENTIRE scan loop runs inside C
 * with a single OCaml->C crossing per repetition (no per-match FFI, no
 * OCaml API shape on top). The oracle side of the gate is measured through
 * Pcre2_test_driver instead, so per-call FFI overhead is included there;
 * this file exists so the gap between the two is recorded, not hidden.
 *
 * The scan loops mirror bench.ml's Runner functor EXACTLY (same empty-match
 * retry with NOTEMPTY_ATSTART|ANCHORED and same one-code-unit UTF-aware
 * bumpalong, pcre2test.c:8293-8443 / pcre2demo.c shape) so that the match
 * counts cross-check against both OCaml drivers. */

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static void bench_raw_code_finalize(value v)
{
        pcre2_code *code = *(pcre2_code **)Data_custom_val(v);
        if (code != NULL) pcre2_code_free(code);
}

static struct custom_operations bench_raw_code_ops = {
        "pcre2-ocaml.bench.raw_code",
        bench_raw_code_finalize,
        custom_compare_default,
        custom_hash_default,
        custom_serialize_default,
        custom_deserialize_default,
        custom_compare_ext_default,
        custom_fixed_length_default,
};

#define Raw_code_val(v) (*(pcre2_code **)Data_custom_val(v))

/* bench_raw_compile : string -> int -> (code, string) result */
CAMLprim value bench_raw_compile(value vpattern, value voptions)
{
        CAMLparam2(vpattern, voptions);
        CAMLlocal2(res, payload);

        int errcode = 0;
        PCRE2_SIZE erroffset = 0;
        pcre2_code *code =
            pcre2_compile((PCRE2_SPTR)String_val(vpattern),
                          caml_string_length(vpattern),
                          (uint32_t)Long_val(voptions), &errcode, &erroffset,
                          NULL);
        if (code == NULL) {
                PCRE2_UCHAR msg[256];
                if (pcre2_get_error_message(errcode, msg, sizeof(msg)) < 0)
                        msg[0] = 0;
                payload = caml_copy_string((const char *)msg);
                res = caml_alloc(1, 1); /* Error */
                Store_field(res, 0, payload);
        } else {
                payload = caml_alloc_custom(&bench_raw_code_ops,
                                            sizeof(pcre2_code *), 0, 1);
                Raw_code_val(payload) = code;
                res = caml_alloc(1, 0); /* Ok */
                Store_field(res, 0, payload);
        }
        CAMLreturn(res);
}

/* Find-all scan with pcre2test/pcre2demo bump semantics. Returns the match
 * count, or the (negative) pcre2 error code on a hard error. When [utf] is
 * nonzero, calls after the first pass PCRE2_NO_UTF_CHECK — the subject was
 * validated by the first call; bench.ml's OCaml loops do the identical
 * thing so all three drivers run the same call sequence. */
static long scan_count_all(pcre2_code *code, const char *subj, size_t len,
                           long utf, int extract)
{
        pcre2_match_data *md =
            pcre2_match_data_create_from_pattern(code, NULL);
        char *scratch = NULL;
        long count = 0;
        long result = 0;
        size_t pos = 0;
        uint32_t opts = 0;
        uint32_t uck = 0;

        if (md == NULL) return PCRE2_ERROR_NOMEMORY;
        if (extract) {
                scratch = malloc(len > 0 ? len : 1);
                if (scratch == NULL) {
                        pcre2_match_data_free(md);
                        return PCRE2_ERROR_NOMEMORY;
                }
        }
        for (;;) {
                int rc = pcre2_match(code, (PCRE2_SPTR)subj, len, pos,
                                     opts | uck, md, NULL);
                if (utf) uck = PCRE2_NO_UTF_CHECK;
                if (rc > 0) {
                        PCRE2_SIZE *ov = pcre2_get_ovector_pointer(md);
                        size_t s = (size_t)ov[0], e = (size_t)ov[1];
                        count++;
                        if (extract && e > s) memcpy(scratch, subj + s, e - s);
                        if (s == e) { /* empty match */
                                if (e >= len) {
                                        result = count;
                                        break;
                                }
                                opts = PCRE2_NOTEMPTY_ATSTART | PCRE2_ANCHORED;
                                pos = e;
                        } else {
                                opts = 0;
                                pos = e;
                        }
                } else if (rc == PCRE2_ERROR_NOMATCH ||
                           rc == PCRE2_ERROR_PARTIAL) {
                        if (opts == 0) {
                                result = count;
                                break;
                        }
                        /* failed NOTEMPTY_ATSTART retry: bump one code unit
                         * (UTF-aware) — pcre2test.c:8362-8377 */
                        pos++;
                        if (utf)
                                while (pos < len &&
                                       (((unsigned char)subj[pos]) & 0xc0) ==
                                           0x80)
                                        pos++;
                        opts = 0;
                        if (pos > len) {
                                result = count;
                                break;
                        }
                } else {
                        result = rc;
                        break;
                }
        }
        free(scratch);
        pcre2_match_data_free(md);
        return result;
}

/* One pcre2_match per '\n'-separated line of [subj] (the joined form of the
 * OCaml drivers' line array — lines never contain '\n', so the two shapes
 * see identical subjects). Returns the count of matching lines. */
static long scan_lines(pcre2_code *code, const char *subj, size_t len)
{
        pcre2_match_data *md =
            pcre2_match_data_create_from_pattern(code, NULL);
        long count = 0;
        size_t p = 0;

        if (md == NULL) return PCRE2_ERROR_NOMEMORY;
        for (;;) {
                const char *nl = memchr(subj + p, '\n', len - p);
                size_t linelen = nl ? (size_t)(nl - (subj + p)) : len - p;
                int rc = pcre2_match(code, (PCRE2_SPTR)(subj + p), linelen, 0,
                                     0, md, NULL);
                if (rc > 0)
                        count++;
                else if (rc != PCRE2_ERROR_NOMATCH &&
                         rc != PCRE2_ERROR_PARTIAL) {
                        count = rc;
                        break;
                }
                if (nl == NULL) break;
                p += linelen + 1;
        }
        pcre2_match_data_free(md);
        return count;
}

/* Single exec (the pathological MATCHLIMIT benchmark): 1 = matched,
 * 0 = no match, negative = the pcre2 error code (expected: -47). */
static long scan_single(pcre2_code *code, const char *subj, size_t len)
{
        pcre2_match_data *md =
            pcre2_match_data_create_from_pattern(code, NULL);
        int rc;

        if (md == NULL) return PCRE2_ERROR_NOMEMORY;
        rc = pcre2_match(code, (PCRE2_SPTR)subj, len, 0, 0, md, NULL);
        pcre2_match_data_free(md);
        if (rc > 0) return 1;
        if (rc == PCRE2_ERROR_NOMATCH || rc == PCRE2_ERROR_PARTIAL) return 0;
        return rc;
}

/* bench_raw_run : code -> string -> int (mode) -> int (utf) -> int
 * mode: 0 = count_all, 1 = single, 2 = per_line, 3 = count_all + extract.
 * No OCaml allocation happens between reading the subject pointer and the
 * last use, and pcre2 never touches the OCaml heap, so the naked pointers
 * below are safe. */
CAMLprim value bench_raw_run(value vcode, value vsubj, value vmode,
                             value vutf)
{
        CAMLparam4(vcode, vsubj, vmode, vutf);
        pcre2_code *code = Raw_code_val(vcode);
        const char *subj = String_val(vsubj);
        size_t len = caml_string_length(vsubj);
        long mode = Long_val(vmode);
        long utf = Long_val(vutf);
        long r;

        switch (mode) {
        case 0:
                r = scan_count_all(code, subj, len, utf, 0);
                break;
        case 1:
                r = scan_single(code, subj, len);
                break;
        case 2:
                r = scan_lines(code, subj, len);
                break;
        case 3:
                r = scan_count_all(code, subj, len, utf, 1);
                break;
        default:
                caml_invalid_argument("bench_raw_run: bad mode");
        }
        CAMLreturn(Val_long(r));
}

/* bench_raw_version : unit -> string  (PCRE2_CONFIG_VERSION, e.g. "10.44 ...") */
CAMLprim value bench_raw_version(value vunit)
{
        CAMLparam1(vunit);
        char buf[64];
        if (pcre2_config(PCRE2_CONFIG_VERSION, buf) < 0) buf[0] = 0;
        CAMLreturn(caml_copy_string(buf));
}
