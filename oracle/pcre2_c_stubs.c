#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

// NOTE: Currently these bindings support only 8-bit code units. Below we use
// the generically named functions. Future versions could include support for
// non-8-bit code units.
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#if __STDC_VERSION__ >= 202311L
#define UNUSED [[maybe_unused]]
#else
#define UNUSED __attribute__((unused))
#endif

/* --- Dev-only oracle overrun guard (pattern + subject slack buffers) ------
 *
 * PCRE2 10.44 has bounded out-of-bounds reads a few bytes before/after the
 * pattern/subject (undefined behaviour) that the pure OCaml engine pins to a
 * defined "reads-as-zero" model:
 *   - forward GETCHAR-past-end (invalid trailing UTF-8 -> GETUTF8INC reads up
 *     to 5 code units past the subject end; ASan pcre2_match.c:2221), and
 *   - backward was_newline/BACKCHAR under PCRE2_MATCH_INVALID_UTF (reads the
 *     code unit before the subject; fuzz repro 204387 / commit e8eeaa8).
 * Passed String_val directly these land in the adjacent OCaml block -- usually
 * mapped (header before, NUL padding after) but occasionally an unmapped page
 * (rare SIGSEGV), and the final OCaml padding byte is the padding COUNT, not
 * always 0x00. Copying pattern/subject into buffers with SLACK zero bytes on
 * BOTH sides makes every bounded overrun read deterministic 0x00 -- the same
 * value the engine reads -- and keeps the byte before the subject 0x00 so the
 * 204387 backward-walk analysis stays valid. (The separate unbounded GET_UCD
 * out-of-range read is handled by oracle/patches/, not by this slack.) */
#define ORACLE_OVERRUN_SLACK 8

/* Dev-only: on allocation failure abort() rather than return NULL -- a NULL
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

const int oracle_OPTION_SOME_TAG = 0;
const int oracle_RESULT_OK_TAG = 0;
const int oracle_RESULT_ERROR_TAG = 1;
const int oracle_TUPLE_TAG = 0;
const int oracle_ARRAY_TAG = 0;

struct ocaml_regex {
        pcre2_code *regex;
};

static inline struct ocaml_regex *regex_of_value(value v) {
        CAMLparam1(v);
        CAMLreturnT(struct ocaml_regex *, Data_custom_val(v));
}

static void ocaml_regex_free(value ocaml_regex) {
        struct ocaml_regex *re = Data_custom_val(ocaml_regex);
        pcre2_code_free(re->regex);
}

static struct custom_operations regex_ops = {.identifier = "pcre2_ocaml_regexp",
                                             .finalize = ocaml_regex_free,
                                             .compare = NULL,
                                             .hash = NULL,
                                             .serialize = NULL,
                                             .deserialize = NULL,
                                             .compare_ext = NULL,
                                             .fixed_length = NULL};

CAMLprim void oracle_pcre2_ocaml_init(void) {
        CAMLparam0();
        CAMLreturn0;
}

/// Returns the PCRE2 version the library was compiled with.
CAMLprim value oracle_get_version(void) /* -> int * int */ {
        CAMLparam0();
        CAMLlocal1(version);
        // SAFETY: This allocation is immediately filled with well-formed
        // values prior to returning.
        version = caml_alloc_small(2, oracle_TUPLE_TAG);
        Field(version, 0) = Val_int(PCRE2_MAJOR);
        Field(version, 1) = Val_int(PCRE2_MINOR);
        CAMLreturn(version);
}

/// error_message : int -> string, exactly pcre2_get_error_message; empty
/// string for unknown codes (caller decides).
CAMLprim value oracle_error_message(value verrcode) {
        CAMLparam1(verrcode);
        PCRE2_UCHAR buf[256];
        int rc = pcre2_get_error_message((int)Long_val(verrcode), buf, sizeof(buf));
        if (rc < 0) buf[0] = 0;
        CAMLreturn(caml_copy_string((const char *)buf));
}

/// Compiles the provided pattern.
///
/// Note that the options from OCaml are not split between those which can
/// directly be provided and those which are specified in the extra word passed
/// via the oracle_compile context.
///
/// @param[in] pattern The pattern to oracle_compile. See `pcre2pattern(3)` for
/// details.
/// @param[in] options The options, specified via a bitvector. See
/// `pcre2_compile(3)`.
/// @return A result comprising the compiled pattern or a structured error.
CAMLprim value oracle_compile_unboxed(value pattern /* : string */,
                               uint32_t options /* : int32 [@unboxed] */
                               /* another arg for oracle_compile context options? */
                               ) /* : -> (regex, int) Result.t */ {
        CAMLparam1(pattern);
        CAMLlocal3(result, regex_value, err_pair);

        size_t ocaml_regexp_size = sizeof(struct ocaml_regex);
        int error_code;
        size_t error_offset;
        size_t pattern_len = caml_string_length(pattern);

        pcre2_compile_context *ccontext = NULL;
        // TODO(cooper): allocated compile_context for the "extra" options if need be.
        // SAFETY: Passing in the value of String_val(subject) here is fine
        // since a GC cannot occur (and the resulting value, which is held across GC, does not refer
        // to the string).
        // Slack-padded copy so any bounded compile-time overrun reads 0x00.
        unsigned char *pat_buf =
            oracle_slack_dup((const unsigned char *)String_val(pattern), pattern_len);
        pcre2_code *regex = pcre2_compile((PCRE2_SPTR)pat_buf, pattern_len, options, &error_code,
                                          &error_offset, ccontext);
        oracle_slack_free(pat_buf);
        pcre2_compile_context_free(ccontext);

        if (!regex) {
                // Returns [Error (code, offset)] since the pattern could not be
                // compiled; the offset mirrors the pure engine's raw seam.
                err_pair = caml_alloc_tuple(2);
                Field(err_pair, 0) = Val_int(error_code);
                Field(err_pair, 1) = Val_int((int)error_offset);
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = err_pair;
                CAMLreturn(result);
        }

        // caml_alloc_custom_mem wants a size estimate of the allocated
        size_t pcre2_allocated_mem;
        pcre2_pattern_info(regex, PCRE2_INFO_SIZE, &pcre2_allocated_mem);
        // TODO(cooper): used mem amount needs increased later if we jit?
        regex_value = caml_alloc_custom_mem(&regex_ops, ocaml_regexp_size, pcre2_allocated_mem);
        regex_of_value(regex_value)->regex = regex;

        // Return [Ok regex]
        // SAFETY: This allocation is immediately filled with well-formed
        // values prior to returning.
        result = caml_alloc_small(1, oracle_RESULT_OK_TAG);
        Field(result, 0) = regex_value;

        CAMLreturn(result);
}

/// Boxed argument version of [oracle_compile_unboxed] (for bytecode).
CAMLprim value oracle_compile(value *argv, int argc UNUSED) {
        return oracle_compile_unboxed(argv[0], Int32_val(argv[1]));
}

/// Match with the provided pattern.
///
/// @param[in] ocaml_re The compiled regex to use for matching.
/// @param[in] subject The string to be searched.
/// @param[in] subject_offset The byte index in the subject at which to begin.
/// @param[in] options Matching options, specified via a bitvector . See `pcre2_match(3)`.
// TODO: allow reusing the pcre2_match_data struct so that exec_all / find_iter
// / captures_iter can avoid a bunch of allocations.
// Ideally this function doesn't allocate except for some `caml_alloc_small`s.
CAMLprim value oracle_match_unboxed(value ocaml_re /* : _ regex */, value subject /* : string */,
                             intnat subject_offset /* : int [@untagged] */,
                             uint32_t options /* : int32 */
                             ) /* : -> ((int * int) option, int) Result.t */ {
        CAMLparam2(ocaml_re, subject);
        CAMLlocal3(result, range, oracle_match);

        // Need to handle this case manually since PCRE2 takes an unsigned value.
        if (subject_offset < 0) {
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(PCRE2_ERROR_BADOFFSET);
                CAMLreturn(result);
        }
        size_t offset = subject_offset;
        size_t subject_length = caml_string_length(subject);

        const pcre2_code *re = regex_of_value(ocaml_re)->regex;
        // TODO: support oracle_match/depth limits. Or callouts. May need to be
        // bundled with the compiled regex.
        pcre2_match_context *mcontext = NULL;
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        // Slack-padded copy so any bounded subject overrun reads 0x00.
        unsigned char *subj_buf =
            oracle_slack_dup((const unsigned char *)String_val(subject), subject_length);
        int ret = pcre2_match(re, (PCRE2_SPTR)subj_buf, subject_length, offset, options, match_data,
                              mcontext);
        oracle_slack_free(subj_buf);
        PCRE2_SIZE *ovec = pcre2_get_ovector_pointer(match_data);

        if (ret == PCRE2_ERROR_NOMATCH || ret == PCRE2_ERROR_PARTIAL) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_OK_TAG);
                Field(result, 0) = Val_none;
                CAMLreturn(result);
        } else if (ret <= 0) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(ret);
                CAMLreturn(result);
        }

        // SAFETY: This allocation is immediately filled with well-formed values.
        range = caml_alloc_small(2, oracle_TUPLE_TAG);
        Field(range, 0) = Val_int(ovec[0]);
        Field(range, 1) = Val_int(ovec[1]);

        pcre2_match_data_free(match_data);

        // SAFETY: This allocation is immediately filled with well-formed values.
        oracle_match = caml_alloc_small(1, oracle_OPTION_SOME_TAG);
        Field(oracle_match, 0) = range;

        // SAFETY: This allocation is immediately filled with
        // well-formed values prior to returning.
        result = caml_alloc_small(1, oracle_RESULT_OK_TAG);
        Field(result, 0) = oracle_match;

        CAMLreturn(result);
}

/// Boxed argument version of [oracle_jit_match_unboxed] (for bytecode).
CAMLprim value oracle_match(value *argv, int argc UNUSED) {
        return oracle_match_unboxed(argv[0], argv[1], Nativeint_val(argv[2]), Int32_val(argv[3]));
}

/// Requests JIT compilation for a processed regex.
///
/// @param[in] regex The already processed regex.
/// @param[in] options JIT compilation options, specified via a bitvector . See
/// `pcre2_jit_compile(3)`.
CAMLprim value oracle_jit_compile_unboxed(
    value ocaml_re /* : interp regex */,
    uint32_t options /* : int32 [@unboxed] */) /* : -> (jit regex, int) Result.t */ {
        // NOTE: The return value of the function is 0 for success, or a
        // negative error code otherwise. In particular,
        // PCRE2_ERROR_JIT_BADOPTION is returned if JIT is not supported or if
        // an unknown bit is set in options. The function can also return
        // PCRE2_ERROR_NOMEMORY if JIT is unable to allocate executable memory
        // for the compiler, even if it was because of a system security
        // restriction.

        // How to handle allocation failure (can be due to security policy) or
        // lack of jit support? Result seems fine but a bit annoying maybe
        CAMLparam1(ocaml_re);
        CAMLlocal1(result);

        int res = pcre2_jit_compile(regex_of_value(ocaml_re)->regex, options);
        if (res < 0) {
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(res);
                CAMLreturn(result);
        }

        // SAFETY: This allocation is immediately filled with well-formed
        // values prior to returning.
        result = caml_alloc_small(1, oracle_RESULT_OK_TAG);
        // TODO: this seems bad, since in OCaml we could then have
        // {x : interp regex} and {y : jit regex}, pointers to the same regex.
        //
        // This is maybe actually fine here, and mostly depends on if you can
        // call pcre2_jit_compile multiple times on the same pcer2_code*
        // safely. Since pcre2_match permits jit, and while {interp regex} is
        // "really" interpreted OR JIT, {jit regex} is definitely JIT.
        Field(result, 0) = ocaml_re;
        CAMLreturn(result);
}

/// Boxed argument version of [oracle_jit_compile_unboxed] (for bytecode).
CAMLprim value oracle_jit_compile(value *argv, int argc UNUSED) {
        return oracle_jit_compile_unboxed(argv[0], Int32_val(argv[1]));
}

/// Match with the provided JIT compiled regex.
///
/// @param[in] ocaml_re The JIT regex to use for matching.
/// @param[in] subject The string to be searched.
/// @param[in] subject_offset The byte index in the subject at which to begin.
/// @param[in] options Matching options, specified via a bitvector. See
/// `pcre2_match(3)`. NOTE: PCRE2_ZERO_TERMINATED is not supported, but this
/// isn't much of an issue since we are dealing with OCaml strings.
CAMLprim value oracle_jit_match_unboxed(value ocaml_re /* : jit regex */, value subject /* : string */,
                                 int subject_offset /* : int [@untagged] */,
                                 uint32_t options /* : int32 */
                                 ) /* : -> ((int * int) option, int) Result.t */ {
        // TODO: Mostly copied from match_stub impl
        //
        //
        // NOTE: In UTF mode, the subject string is not checked for UTF
        // validity. Unless PCRE2_MATCH_INVALID_UTF was set when the pattern
        // was compiled, passing an invalid UTF string results in undefined
        // behaviour.Your program may crash or loop or give wrong results .In
        // the absence of PCRE2_MATCH_INVALID_UTF you should only call
        // pcre2_jit_match() in UTF mode if you are sure the subject is valid.
        //
        // TODO: either implement a UTF check or force INVALID_UTF in UTF mode.
        //
        //
        // NOTE: restricted option set. Use polymoprhic variants on the OCaml side.
        // The supported options are PCRE2_NOTBOL, PCRE2_NOTEOL,
        // PCRE2_NOTEMPTY, PCRE2_NOTEMPTY_ATSTART, PCRE2_PARTIAL_HARD, and
        // PCRE2_PARTIAL_SOFT. Unsupported options are ignored.
        CAMLparam2(ocaml_re, subject);
        CAMLlocal3(result, range, oracle_match);

        // For lower bound, need to handle this case manually since PCRE2 takes
        // an unsigned value.
        //
        // NOTE: need upper bound check here since we use pcre2_jit_match later and we want to
        // ensure we consistently return BADOFFSET rather than just failing to
        // find for JIT only.
        if (!(0 <= subject_offset && (mlsize_t)subject_offset <= caml_string_length(subject))) {
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(PCRE2_ERROR_BADOFFSET);
                CAMLreturn(result);
        }
        size_t offset = subject_offset;
        size_t subject_length = caml_string_length(subject);

        const pcre2_code *re = regex_of_value(ocaml_re)->regex;
        // TODO: support oracle_match/depth limits. Or callouts. May need to be
        // bundled with the compiled regex.
        pcre2_match_context *mcontext = NULL;
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        // SAFETY: Passing in the value of String_val(subject) here is fine
        // since a GC cannot occur.
        // Slack-padded copy so any bounded subject overrun reads 0x00.
        unsigned char *subj_buf =
            oracle_slack_dup((const unsigned char *)String_val(subject), subject_length);
        int ret = pcre2_jit_match(re, (PCRE2_SPTR)subj_buf, subject_length, offset, options,
                                  match_data, mcontext);
        oracle_slack_free(subj_buf);
        PCRE2_SIZE *ovec = pcre2_get_ovector_pointer(match_data);

        if (ret == PCRE2_ERROR_NOMATCH || ret == PCRE2_ERROR_PARTIAL) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_OK_TAG);
                Field(result, 0) = Val_none;
                CAMLreturn(result);
        } else if (ret <= 0) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(ret);
                CAMLreturn(result);
        }

        // SAFETY: This allocation is immediately filled with well-formed values.
        range = caml_alloc_small(2, oracle_TUPLE_TAG);
        Field(range, 0) = Val_int(ovec[0]);
        Field(range, 1) = Val_int(ovec[1]);

        pcre2_match_data_free(match_data);

        // SAFETY: This allocation is immediately filled with well-formed values.
        oracle_match = caml_alloc_small(1, oracle_OPTION_SOME_TAG);
        Field(oracle_match, 0) = range;

        // SAFETY: This allocation is immediately filled with
        // well-formed values prior to returning.
        result = caml_alloc_small(1, oracle_RESULT_OK_TAG);
        Field(result, 0) = oracle_match;

        CAMLreturn(result);
}

/// Boxed argument version of [oracle_jit_match_unboxed] (for bytecode).
CAMLprim value oracle_jit_match(value *argv, int argc UNUSED) {
        return oracle_jit_match_unboxed(argv[0], argv[1], Int_val(argv[2]), Int32_val(argv[3]));
}

/// Returns the name table associated with a given regex.
///
/// @param[in] regex The regex to retrieve the name table of.
/// @param[out] name_count The number of names.
/// @param[out] entry_size The size of each entry (string length of the names)
/// in code points.
/// @return The name table, an array of length name_count, comprising strings
/// of (maximum) length entry_size and the corresponding parentheses number of
/// them (packed as specified by pcre2_pattern_info(3)).
PCRE2_SPTR oracle_names_of_regex(const pcre2_code *regex, uint32_t *name_count, uint32_t *entry_size) {
        PCRE2_SPTR name_table;

        if (pcre2_pattern_info(regex, PCRE2_INFO_NAMECOUNT, name_count) < 0) {
                return NULL;
        }

        if (pcre2_pattern_info(regex, PCRE2_INFO_NAMEENTRYSIZE, entry_size) < 0) {
                return NULL;
        }

        if (pcre2_pattern_info(regex, PCRE2_INFO_NAMETABLE, &name_table) < 0) {
                return NULL;
        }

        return name_table;
}

/// Returns the named oracle_capture groups and their corresponding numbering for a
/// given regex.
///
/// @param[in] regex The regex to return the named oracle_capture groups of.
/// @return An array of OCaml values comprising tuples `(n, i)` of the name `n`
/// and numbering `i` of the named oracle_capture groups present in the regex.
value oracle_make_capture_group_name_table(const pcre2_code *regex) /* -> (string * int) array */ {
        CAMLparam0();
        CAMLlocal3(name, pair, array);

        uint32_t name_count;
        uint32_t entry_size;
        PCRE2_SPTR names = oracle_names_of_regex(regex, &name_count, &entry_size);

        if (!names) {
                array = caml_alloc_small(0, oracle_ARRAY_TAG);
                // SAFETY(caml_alloc_small): There are no fields in the
                // allocation, so it is trivially well-formed.
                CAMLreturn(array);
        }

        if (name_count < Max_young_wosize) { /* likely */
                // SAFETY: This array is fully initialized with well-formed
                // values by the following loop before
                array = caml_alloc_small(name_count, oracle_ARRAY_TAG);
                for (size_t i = 0; i < name_count; ++i) {
                        Field(array, i) = Val_unit;
                }
        } else {
                array = caml_alloc_tuple(name_count);
        }

        for (size_t i = 0; i < name_count; ++i) {
                const size_t j = i * entry_size;
                uint32_t group_number = (names[j] << 8) | names[j + 1];
                name = caml_copy_string((const char *)&names[j + 2]);
                pair = caml_alloc_small(2, oracle_TUPLE_TAG);
                Field(pair, 0) = name;
                Field(pair, 1) = Val_int(group_number);
                caml_modify(&Field(array, i), pair);
        }

        CAMLreturn(array);
}

/// Wrapper for [oracle_make_capture_group_name_table] which takes a regex as an OCaml
/// value, instead of directly.
CAMLprim value oracle_get_capture_groups(value ocaml_regex /* : regex */) /* -> (string * int) array */ {
        CAMLparam1(ocaml_regex);
        CAMLreturn(oracle_make_capture_group_name_table(regex_of_value(ocaml_regex)->regex));
}

/// Match, with oracle_capture groups, the provided pattern.
///
/// @param[in] ocaml_re The compiled regex to use for matching.
/// @param[in] subject The string to be searched.
/// @param[in] subject_offset The byte index in the subject at which to begin.
/// @param[in] options Matching options, specified via a bitvector . See `pcre2_match(3)`.
// TODO: allow reusing the pcre2_match_data struct so that captures_iter can avoid a bunch of
// allocations. Ideally this function doesn't allocate (except maybe a result).
CAMLprim value oracle_capture_unboxed(
    value ocaml_re /* : _ regex */, value subject /* : string */,
    intnat subject_offset /* : int [@untagged] */, uint32_t options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        CAMLparam2(ocaml_re, subject);
        CAMLlocal5(result, matches, oracle_match, name_table, matches_and_table);
        CAMLlocal1(match_opt);

        if (subject_offset < 0) {
                // Need to handle this case manually since PCRE2 takes an unsigned value.
                // FIXME: result or option from this function? need to see if meaningful errors can
                // occur
                // SAFETY: This allocation is immediately filled with well-formed values prior to
                // returning.
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(PCRE2_ERROR_BADOFFSET);
                CAMLreturn(result);
        }
        size_t offset = subject_offset;
        size_t subject_length = caml_string_length(subject);

        const pcre2_code *re = regex_of_value(ocaml_re)->regex;
        // TODO: support oracle_match/depth limits. Or callouts. May need to be
        // bundled with the compiled regex.
        pcre2_match_context *mcontext = NULL;
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        // NOTE: Really one more than number of captures since it includes the
        // full oracle_match.
        // SAFETY: Passing in the value of String_val(subject) here is fine
        // since a GC cannot occur.
        // Slack-padded copy so any bounded subject overrun reads 0x00.
        unsigned char *subj_buf =
            oracle_slack_dup((const unsigned char *)String_val(subject), subject_length);
        int num_captures =
            pcre2_match(re, (PCRE2_SPTR)subj_buf, subject_length, offset, options, match_data,
                        mcontext);
        oracle_slack_free(subj_buf);
        PCRE2_SIZE *ovec = pcre2_get_ovector_pointer(match_data);

        if (num_captures == PCRE2_ERROR_NOMATCH || num_captures == PCRE2_ERROR_PARTIAL) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_OK_TAG);
                Field(result, 0) = Val_none;
                CAMLreturn(result);
        } else if (num_captures <= 0) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(num_captures);
                CAMLreturn(result);
        }

        // Marshal the FULL ovector (capturecount + 1 pairs), padding pairs
        // beyond the highest-set one (num_captures) with (-1, -1). The pure
        // engine returns full-length capture arrays; truncating here made the
        // oracle drift from that seam (visible to full_split's NoGroup
        // emission and $n template validation in the convenience layer).
        uint32_t ovec_count = pcre2_get_ovector_count(match_data);
        matches /* : (int * int) array */ = caml_alloc_tuple(ovec_count);
        for (uint32_t i = 0; i < ovec_count; ++i) {
                // SAFETY: This block must be filled with well-formed values
                // before the next allocation. The next allocation is no
                // earlier than the end of this loop iteration. All fields of
                // this tuple are assigned to by the end of the loop.
                oracle_match /* : int * int */ = caml_alloc_small(2, oracle_TUPLE_TAG);
                // The i-th oracle_capture group (the 0-th being the full oracle_match) is at
                // [2i, 2i+1] in ovec.
                if (i < (uint32_t)num_captures) {
                        Field(oracle_match, 0) = Val_int(ovec[2 * i]);
                        Field(oracle_match, 1) = Val_int(ovec[2 * i + 1]);
                } else {
                        Field(oracle_match, 0) = Val_int(-1);
                        Field(oracle_match, 1) = Val_int(-1);
                }
                caml_modify(&Field(matches, i), oracle_match);
        }

        // TODO: cache this? May not be that expensive, but if it is then probably worth since we'll
        // oracle_match w/ oracle_capture many times.
        name_table = oracle_make_capture_group_name_table(re);

        pcre2_match_data_free(match_data);

        // SAFETY: This allocation is immediately filled with well-formed
        // values.
        matches_and_table /* : (int * int) array * (string * int) array */ =
            caml_alloc_small(2, oracle_TUPLE_TAG);
        Field(matches_and_table, 0) = matches;
        Field(matches_and_table, 1) = name_table;

        // SAFETY: This allocation is immediately filled with well-formed
        // values.
        match_opt /* : ((int * int) array * (string * int) array) option */ =
            caml_alloc_small(1, oracle_OPTION_SOME_TAG);
        Field(match_opt, 0) = matches_and_table;

        // SAFETY: This allocation is immediately filled with well-formed
        // values prior to returning.
        result /* : (((int * int) array * (string * int) array) option, _) Result.t */ =
            caml_alloc_small(1, oracle_RESULT_OK_TAG);
        Field(result, 0) = match_opt;

        CAMLreturn(result);
}

/// Boxed argument version of [oracle_capture_unboxed] (for bytecode).
CAMLprim value oracle_capture(value *argv, int argc UNUSED) {
        return oracle_capture_unboxed(argv[0], argv[1], Nativeint_val(argv[2]), Int32_val(argv[3]));
}

/// Match, with oracle_capture groups, the provided JIT-enabled pattern.
///
/// @param[in] ocaml_re The compiled regex to use for matching.
/// @param[in] subject The string to be searched.
/// @param[in] subject_offset The byte index in the subject at which to begin.
/// @param[in] options Matching options, specified via a bitvector . See `pcre2_match(3)`.
// TODO: allow reusing the pcre2_match_data struct so that captures_iter can avoid a bunch of
// allocations. Ideally this function doesn't allocate (except maybe a result).
CAMLprim value oracle_jit_capture_unboxed(
    value ocaml_re /* : _ regex */, value subject /* : string */,
    intnat subject_offset /* : int [@untagged] */, uint32_t options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        CAMLparam2(ocaml_re, subject);
        CAMLlocal5(result, matches, oracle_match, name_table, matches_and_table);
        CAMLlocal1(match_opt);

        // NOTE: need upper bound check here since we use pcre2_jit_match later and we want to
        // ensure we consistently return BADOFFSET rather than just failing to
        // find for JIT only.
        if (!(0 <= subject_offset && (mlsize_t)subject_offset <= caml_string_length(subject))) {
                // Need to handle this case manually since PCRE2 takes an unsigned value.
                // FIXME: result or option from this function? need to see if meaningful errors can
                // occur
                // SAFETY: This allocation is immediately filled with well-formed values prior to
                // returning.
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(PCRE2_ERROR_BADOFFSET);
                CAMLreturn(result);
        }
        size_t offset = subject_offset;
        size_t subject_length = caml_string_length(subject);

        const pcre2_code *re = regex_of_value(ocaml_re)->regex;
        // TODO: support oracle_match/depth limits. Or callouts. May need to be
        // bundled with the compiled regex.
        pcre2_match_context *mcontext = NULL;
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        // NOTE: Really one more than number of captures since it includes the
        // full oracle_match.
        // SAFETY: Passing in the value of String_val(subject) here is fine
        // since a GC cannot occur.
        // TODO: Compare notes on using pcre2_jit_match in oracle_jit_match_unboxed.
        // Slack-padded copy so any bounded subject overrun reads 0x00.
        unsigned char *subj_buf =
            oracle_slack_dup((const unsigned char *)String_val(subject), subject_length);
        int num_captures = pcre2_jit_match(re, (PCRE2_SPTR)subj_buf, subject_length, offset, options,
                                           match_data, mcontext);
        oracle_slack_free(subj_buf);
        PCRE2_SIZE *ovec = pcre2_get_ovector_pointer(match_data);

        if (num_captures == PCRE2_ERROR_NOMATCH || num_captures == PCRE2_ERROR_PARTIAL) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_OK_TAG);
                Field(result, 0) = Val_none;
                CAMLreturn(result);
        } else if (num_captures <= 0) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, oracle_RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(num_captures);
                CAMLreturn(result);
        }

        // Marshal the FULL ovector (capturecount + 1 pairs), padding pairs
        // beyond the highest-set one (num_captures) with (-1, -1). The pure
        // engine returns full-length capture arrays; truncating here made the
        // oracle drift from that seam (visible to full_split's NoGroup
        // emission and $n template validation in the convenience layer).
        uint32_t ovec_count = pcre2_get_ovector_count(match_data);
        matches /* : (int * int) array */ = caml_alloc_tuple(ovec_count);
        for (uint32_t i = 0; i < ovec_count; ++i) {
                // SAFETY: This block must be filled with well-formed values
                // before the next allocation. The next allocation is no
                // earlier than the end of this loop iteration. All fields of
                // this tuple are assigned to by the end of the loop.
                oracle_match /* : int * int */ = caml_alloc_small(2, oracle_TUPLE_TAG);
                // The i-th oracle_capture group (the 0-th being the full oracle_match) is at
                // [2i, 2i+1] in ovec.
                if (i < (uint32_t)num_captures) {
                        Field(oracle_match, 0) = Val_int(ovec[2 * i]);
                        Field(oracle_match, 1) = Val_int(ovec[2 * i + 1]);
                } else {
                        Field(oracle_match, 0) = Val_int(-1);
                        Field(oracle_match, 1) = Val_int(-1);
                }
                caml_modify(&Field(matches, i), oracle_match);
        }

        // TODO: cache this? May not be that expensive, but if it is then probably worth since we'll
        // oracle_match w/ oracle_capture many times.
        name_table = oracle_make_capture_group_name_table(re);

        pcre2_match_data_free(match_data);

        // SAFETY: This allocation is immediately filled with well-formed
        // values.
        matches_and_table /* : (int * int) array * (string * int) array */ =
            caml_alloc_small(2, oracle_TUPLE_TAG);
        Field(matches_and_table, 0) = matches;
        Field(matches_and_table, 1) = name_table;

        // SAFETY: This allocation is immediately filled with well-formed
        // values.
        match_opt /* : ((int * int) array * (string * int) array) option */ =
            caml_alloc_small(1, oracle_OPTION_SOME_TAG);
        Field(match_opt, 0) = matches_and_table;

        // SAFETY: This allocation is immediately filled with well-formed
        // values prior to returning.
        result /* : (((int * int) array * (string * int) array) option, _) Result.t */ =
            caml_alloc_small(1, oracle_RESULT_OK_TAG);
        Field(result, 0) = match_opt;

        CAMLreturn(result);
}

/// Boxed argument version of [oracle_capture_unboxed] (for bytecode).
CAMLprim value oracle_jit_capture(value *argv, int argc UNUSED) {
        return oracle_jit_capture_unboxed(argv[0], argv[1], Nativeint_val(argv[2]), Int32_val(argv[3]));
}
