#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"

// NOTE: Currently these bindings support only 8-bit code units. Below we use
// the generically named functions. Future versions could include support for
// non-8-bit code units.
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

const int OPTION_SOME_TAG = 0;
const int RESULT_OK_TAG = 0;
const int RESULT_ERROR_TAG = 1;
const int TUPLE_TAG = 0;
const int ARRAY_TAG = 0;

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

/// A copy of a subject string held outside the OCaml heap, so that repeated
/// matches against the same subject (e.g. find_iter) can release the runtime
/// lock without re-copying the subject on every call.
struct pinned_subject {
        char *buf;
        size_t len;
};

static inline struct pinned_subject *pinned_of_value(value v) {
        return Data_custom_val(v);
}

static void pinned_subject_free(value pinned) {
        caml_stat_free(pinned_of_value(pinned)->buf);
}

static struct custom_operations pinned_subject_ops = {.identifier = "pcre2_ocaml_pinned_subject",
                                                      .finalize = pinned_subject_free,
                                                      .compare = NULL,
                                                      .hash = NULL,
                                                      .serialize = NULL,
                                                      .deserialize = NULL,
                                                      .compare_ext = NULL,
                                                      .fixed_length = NULL};

/// Copies a subject string out of the OCaml heap. The copy is freed when the
/// returned value is collected.
CAMLprim value pin_subject(value subject /* : string */) /* : -> pinned_subject */ {
        CAMLparam1(subject);
        CAMLlocal1(pinned);

        size_t len = caml_string_length(subject);
        pinned = caml_alloc_custom_mem(&pinned_subject_ops, sizeof(struct pinned_subject), len);
        // The finalizer may run on this block even if caml_stat_alloc raises
        // below, so buf must never hold garbage.
        pinned_of_value(pinned)->buf = NULL;
        pinned_of_value(pinned)->len = len;
        // PCRE2 rejects NULL subjects, so allocate at least one byte.
        pinned_of_value(pinned)->buf = caml_stat_alloc(len == 0 ? 1 : len);
        memcpy(pinned_of_value(pinned)->buf, String_val(subject), len);

        CAMLreturn(pinned);
}

CAMLprim void pcre2_ocaml_init(void) {
        CAMLparam0();
        CAMLreturn0;
}

/// Returns the PCRE2 version the library was compiled with.
CAMLprim value get_version(void) /* -> int * int */ {
        CAMLparam0();
        CAMLlocal1(version);
        // SAFETY: This allocation is immediately filled with well-formed
        // values prior to returning.
        version = caml_alloc_small(2, TUPLE_TAG);
        Field(version, 0) = Val_int(PCRE2_MAJOR);
        Field(version, 1) = Val_int(PCRE2_MINOR);
        CAMLreturn(version);
}

/// Compiles the provided pattern.
///
/// Note that the options from OCaml are not split between those which can
/// directly be provided and those which are specified in the extra word passed
/// via the compile context.
///
/// @param[in] pattern The pattern to compile. See `pcre2pattern(3)` for
/// details.
/// @param[in] options The options, specified via a bitvector. See
/// `pcre2_compile(3)`.
/// @return A result comprising the compiled pattern or a structured error.
CAMLprim value compile_unboxed(value pattern /* : string */,
                               uint32_t options /* : int32 [@unboxed] */
                               /* another arg for compile context options? */
                               ) /* : -> (regex, int) Result.t */ {
        CAMLparam1(pattern);
        CAMLlocal2(result, regex_value);

        size_t ocaml_regexp_size = sizeof(struct ocaml_regex);
        int error_code;
        size_t error_offset;
        size_t pattern_len = caml_string_length(pattern);

        pcre2_compile_context *ccontext = NULL;
        // TODO(cooper): allocated compile_context for the "extra" options if need be.
        // SAFETY: Passing in the value of String_val(subject) here is fine
        // since a GC cannot occur (and the resulting value, which is held across GC, does not refer
        // to the string).
        pcre2_code *regex = pcre2_compile((PCRE2_SPTR)String_val(pattern), pattern_len, options,
                                          &error_code, &error_offset, ccontext);
        pcre2_compile_context_free(ccontext);

        if (!regex) {
                // Returns [Error e] since the pattern could not be compiled.
                result = caml_alloc_small(1, RESULT_ERROR_TAG);
                // TODO(cooper): mapping between error codes here and datatype
                // above.
                Field(result, 0) = Val_int(error_code);
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
        result = caml_alloc_small(1, RESULT_OK_TAG);
        Field(result, 0) = regex_value;

        CAMLreturn(result);
}

/// Boxed argument version of [compile_unboxed] (for bytecode).
CAMLprim value compile(value pattern /* : string */,
                       value options /* : int32 */) /* : -> (regex, int) Result.t */ {
        return compile_unboxed(pattern, Int32_val(options));
}

/// Matches on subjects at least this long run with the runtime lock released
/// so that other domains can proceed (including collecting garbage) during the
/// match. That requires copying the subject, so shorter subjects skip it.
#define LOCK_RELEASE_SUBJECT_THRESHOLD 1024

/// The length of `subject`, which is either an OCaml string or a pinned
/// subject as indicated by `pinned`.
static size_t subject_length_of(value subject /* : string or pinned_subject */, bool pinned) {
        return pinned ? pinned_of_value(subject)->len : caml_string_length(subject);
}

static int do_match(bool use_jit, const pcre2_code *re, PCRE2_SPTR subject_ptr,
                    size_t subject_length, size_t offset, uint32_t options,
                    pcre2_match_data *match_data, pcre2_match_context *mcontext) {
        return use_jit ? pcre2_jit_match(re, subject_ptr, subject_length, offset, options,
                                         match_data, mcontext)
                       : pcre2_match(re, subject_ptr, subject_length, offset, options, match_data,
                                     mcontext);
}

/// Runs `pcre2_match` (or `pcre2_jit_match` if `use_jit`), releasing the
/// runtime lock during the match for sufficiently long subjects.
///
/// The caller must have registered `subject` with the GC and must not reuse
/// pointers derived from it (e.g. via `String_val`) after this returns, since
/// the GC may move it while the lock is released.
///
/// The lock is released with caml_enter_blocking_section_no_pending rather
/// than caml_release_runtime_system: the latter polls pending actions and so
/// can raise an asynchronous exception (e.g. a signal-based timeout), which
/// would leak the subject copy and the caller's match_data. Neither function
/// used here polls (runtime/signals.c); pending actions instead run at the
/// next poll point, after this stub has returned and freed its resources. See
/// https://ocaml.org/manual/5.5/intfc.html#ss:parallel-execution-long-running-c-code
static int run_match(bool use_jit, const pcre2_code *re,
                     value subject /* : string or pinned_subject */, bool pinned, size_t offset,
                     uint32_t options, pcre2_match_data *match_data,
                     pcre2_match_context *mcontext) {
        size_t subject_length = subject_length_of(subject, pinned);

        if (pinned) {
                // The pinned copy lives outside the OCaml heap, so the pointer
                // stays valid while the lock is released.
                PCRE2_SPTR subject_ptr = (PCRE2_SPTR)pinned_of_value(subject)->buf;
                if (subject_length < LOCK_RELEASE_SUBJECT_THRESHOLD) {
                        return do_match(use_jit, re, subject_ptr, subject_length, offset, options,
                                        match_data, mcontext);
                }
                caml_enter_blocking_section_no_pending();
                int ret = do_match(use_jit, re, subject_ptr, subject_length, offset, options,
                                   match_data, mcontext);
                caml_leave_blocking_section();
                return ret;
        }

        if (subject_length < LOCK_RELEASE_SUBJECT_THRESHOLD) {
                // SAFETY: Passing in the value of String_val(subject) here is
                // fine since the runtime lock is held and no OCaml allocation
                // occurs during the match, so a GC cannot occur.
                return do_match(use_jit, re, (PCRE2_SPTR)String_val(subject), subject_length,
                                offset, options, match_data, mcontext);
        }

        // Copy the whole subject, not just from `offset`: lookbehinds, \b,
        // etc. can inspect text preceding the offset.
        char *subject_copy = caml_stat_alloc_noexc(subject_length);
        if (!subject_copy) {
                return PCRE2_ERROR_NOMEMORY;
        }
        memcpy(subject_copy, String_val(subject), subject_length);

        caml_enter_blocking_section_no_pending();
        int ret = do_match(use_jit, re, (PCRE2_SPTR)subject_copy, subject_length, offset, options,
                           match_data, mcontext);
        caml_leave_blocking_section();

        caml_stat_free(subject_copy);
        return ret;
}

/// Match with the provided pattern. Shared implementation for the match stubs.
///
/// @param[in] ocaml_re The compiled regex to use for matching.
/// @param[in] subject The string to be searched (an OCaml string, or a pinned
/// subject if `pinned`).
/// @param[in] use_jit Whether to match with `pcre2_jit_match`.
/// @param[in] subject_offset The byte index in the subject at which to begin.
/// @param[in] options Matching options, specified via a bitvector . See `pcre2_match(3)`.
// TODO: allow reusing the pcre2_match_data struct so that exec_all / find_iter
// / captures_iter can avoid a bunch of allocations.
// Ideally this function doesn't allocate except for some `caml_alloc_small`s.
static value match_general(value ocaml_re /* : _ regex */,
                           value subject /* : string or pinned_subject */, bool pinned,
                           bool use_jit, intnat subject_offset,
                           uint32_t options) /* : -> ((int * int) option, int) Result.t */ {
        CAMLparam2(ocaml_re, subject);
        CAMLlocal3(result, range, match);

        // Need to handle the lower bound manually since PCRE2 takes an
        // unsigned value. The upper bound must be checked for JIT matching
        // since pcre2_jit_match does not, and we want to consistently return
        // BADOFFSET rather than just failing to find for JIT only.
        if (subject_offset < 0
            || (use_jit && (size_t)subject_offset > subject_length_of(subject, pinned))) {
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(PCRE2_ERROR_BADOFFSET);
                CAMLreturn(result);
        }
        size_t offset = subject_offset;

        const pcre2_code *re = regex_of_value(ocaml_re)->regex;
        // TODO: support match/depth limits. Or callouts. May need to be
        // bundled with the compiled regex.
        pcre2_match_context *mcontext = NULL;
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        int ret = run_match(use_jit, re, subject, pinned, offset, options, match_data, mcontext);
        PCRE2_SIZE *ovec = pcre2_get_ovector_pointer(match_data);

        if (ret == PCRE2_ERROR_NOMATCH || ret == PCRE2_ERROR_PARTIAL) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, RESULT_OK_TAG);
                Field(result, 0) = Val_none;
                CAMLreturn(result);
        } else if (ret <= 0) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(ret);
                CAMLreturn(result);
        }

        // SAFETY: This allocation is immediately filled with well-formed values.
        range = caml_alloc_small(2, TUPLE_TAG);
        Field(range, 0) = Val_int(ovec[0]);
        Field(range, 1) = Val_int(ovec[1]);

        pcre2_match_data_free(match_data);

        // SAFETY: This allocation is immediately filled with well-formed values.
        match = caml_alloc_small(1, OPTION_SOME_TAG);
        Field(match, 0) = range;

        // SAFETY: This allocation is immediately filled with
        // well-formed values prior to returning.
        result = caml_alloc_small(1, RESULT_OK_TAG);
        Field(result, 0) = match;

        CAMLreturn(result);
}

/// Match with the provided pattern.
CAMLprim value match_unboxed(value ocaml_re /* : _ regex */, value subject /* : string */,
                             intnat subject_offset /* : int [@untagged] */,
                             uint32_t options /* : int32 */
                             ) /* : -> ((int * int) option, int) Result.t */ {
        return match_general(ocaml_re, subject, false, false, subject_offset, options);
}

/// Boxed argument version of [match_unboxed] (for bytecode).
CAMLprim value match(value ocaml_re /* : _ regex */, value subject /* : string */,
                     value subject_offset /* : int */,
                     value options /* : int32 */) /* : -> ((int * int) option, int) Result.t */ {
        return match_unboxed(ocaml_re, subject, Int_val(subject_offset), Int32_val(options));
}

/// Variant of [match_unboxed] which takes a pinned subject.
CAMLprim value match_pinned_unboxed(value ocaml_re /* : _ regex */,
                                    value subject /* : pinned_subject */,
                                    intnat subject_offset /* : int [@untagged] */,
                                    uint32_t options /* : int32 */
                                    ) /* : -> ((int * int) option, int) Result.t */ {
        return match_general(ocaml_re, subject, true, false, subject_offset, options);
}

/// Boxed argument version of [match_pinned_unboxed] (for bytecode).
CAMLprim value match_pinned(value ocaml_re /* : _ regex */, value subject /* : pinned_subject */,
                            value subject_offset /* : int */, value options /* : int32 */
                            ) /* : -> ((int * int) option, int) Result.t */ {
        return match_pinned_unboxed(ocaml_re, subject, Int_val(subject_offset), Int32_val(options));
}

/// Requests JIT compilation for a processed regex.
///
/// @param[in] regex The already processed regex.
/// @param[in] options JIT compilation options, specified via a bitvector . See
/// `pcre2_jit_compile(3)`.
CAMLprim value jit_compile_unboxed(
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
                result = caml_alloc_small(1, RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(res);
                CAMLreturn(result);
        }

        // SAFETY: This allocation is immediately filled with well-formed
        // values prior to returning.
        result = caml_alloc_small(1, RESULT_OK_TAG);
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

/// Boxed argument version of [jit_compile_unboxed] (for bytecode).
CAMLprim value jit_compile(value ocaml_re /* : interp regex */,
                           value options /* : int32 */) /* : -> (jit regex, int) Result.t */ {
        return jit_compile_unboxed(ocaml_re, Int32_val(options));
}

/// Match with the provided JIT compiled regex.
///
/// @param[in] ocaml_re The JIT regex to use for matching.
/// @param[in] subject The string to be searched.
/// @param[in] subject_offset The byte index in the subject at which to begin.
/// @param[in] options Matching options, specified via a bitvector. See
/// `pcre2_match(3)`. NOTE: PCRE2_ZERO_TERMINATED is not supported, but this
/// isn't much of an issue since we are dealing with OCaml strings.
///
/// NOTE: In UTF mode, the subject string is not checked for UTF
/// validity. Unless PCRE2_MATCH_INVALID_UTF was set when the pattern
/// was compiled, passing an invalid UTF string results in undefined
/// behaviour. Your program may crash or loop or give wrong results. In
/// the absence of PCRE2_MATCH_INVALID_UTF you should only call
/// pcre2_jit_match() in UTF mode if you are sure the subject is valid.
///
/// TODO: either implement a UTF check or force INVALID_UTF in UTF mode.
///
/// NOTE: restricted option set. Use polymorphic variants on the OCaml side.
/// The supported options are PCRE2_NOTBOL, PCRE2_NOTEOL,
/// PCRE2_NOTEMPTY, PCRE2_NOTEMPTY_ATSTART, PCRE2_PARTIAL_HARD, and
/// PCRE2_PARTIAL_SOFT. Unsupported options are ignored.
CAMLprim value jit_match_unboxed(value ocaml_re /* : jit regex */, value subject /* : string */,
                                 intnat subject_offset /* : int [@untagged] */,
                                 uint32_t options /* : int32 */
                                 ) /* : -> ((int * int) option, int) Result.t */ {
        return match_general(ocaml_re, subject, false, true, subject_offset, options);
}

/// Boxed argument version of [jit_match_unboxed] (for bytecode).
CAMLprim value jit_match(value ocaml_re /* : jit regex */, value subject /* : string */,
                         value subject_offset /* : int */, value options /* : int32 */
                         ) /* : -> ((int * int) option, int) Result.t */ {
        return jit_match_unboxed(ocaml_re, subject, Int_val(subject_offset), Int32_val(options));
}

/// Variant of [jit_match_unboxed] which takes a pinned subject.
CAMLprim value jit_match_pinned_unboxed(value ocaml_re /* : jit regex */,
                                        value subject /* : pinned_subject */,
                                        intnat subject_offset /* : int [@untagged] */,
                                        uint32_t options /* : int32 */
                                        ) /* : -> ((int * int) option, int) Result.t */ {
        return match_general(ocaml_re, subject, true, true, subject_offset, options);
}

/// Boxed argument version of [jit_match_pinned_unboxed] (for bytecode).
CAMLprim value jit_match_pinned(value ocaml_re /* : jit regex */,
                                value subject /* : pinned_subject */,
                                value subject_offset /* : int */, value options /* : int32 */
                                ) /* : -> ((int * int) option, int) Result.t */ {
        return jit_match_pinned_unboxed(ocaml_re, subject, Int_val(subject_offset),
                                        Int32_val(options));
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
PCRE2_SPTR names_of_regex(const pcre2_code *regex, uint32_t *name_count, uint32_t *entry_size) {
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

/// Returns the named capture groups and their corresponding numbering for a
/// given regex.
///
/// @param[in] regex The regex to return the named capture groups of.
/// @return An array of OCaml values comprising tuples `(n, i)` of the name `n`
/// and numbering `i` of the named capture groups present in the regex.
value make_capture_group_name_table(const pcre2_code *regex) /* -> (string * int) array */ {
        CAMLparam0();
        CAMLlocal3(name, pair, array);

        uint32_t name_count;
        uint32_t entry_size;
        PCRE2_SPTR names = names_of_regex(regex, &name_count, &entry_size);

        if (!names || name_count == 0) {
                CAMLreturn(Atom(ARRAY_TAG));
        }

        if (name_count < Max_young_wosize) { /* likely */
                // SAFETY: This array is fully initialized with well-formed
                // values by the following loop before
                array = caml_alloc_small(name_count, ARRAY_TAG);
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
                pair = caml_alloc_small(2, TUPLE_TAG);
                Field(pair, 0) = name;
                Field(pair, 1) = Val_int(group_number);
                caml_modify(&Field(array, i), pair);
        }

        CAMLreturn(array);
}

/// Wrapper for [make_capture_group_name_table] which takes a regex as an OCaml
/// value, instead of directly.
CAMLprim value get_capture_groups(value ocaml_regex /* : regex */) /* -> (string * int) array */ {
        CAMLparam1(ocaml_regex);
        CAMLreturn(make_capture_group_name_table(regex_of_value(ocaml_regex)->regex));
}

/// Match, with capture groups, the provided pattern. Shared implementation
/// for the capture stubs.
///
/// @param[in] ocaml_re The compiled regex to use for matching.
/// @param[in] subject The string to be searched (an OCaml string, or a pinned
/// subject if `pinned`).
/// @param[in] use_jit Whether to match with `pcre2_jit_match`.
/// @param[in] subject_offset The byte index in the subject at which to begin.
/// @param[in] options Matching options, specified via a bitvector . See `pcre2_match(3)`.
// TODO: allow reusing the pcre2_match_data struct so that captures_iter can avoid a bunch of
// allocations. Ideally this function doesn't allocate (except maybe a result).
static value capture_general(value ocaml_re /* : _ regex */,
                             value subject /* : string or pinned_subject */, bool pinned,
                             bool use_jit, intnat subject_offset,
                             uint32_t options) /* : -> (((int * int) array * (string * int) array)
                                                  option, match_error) Result.t */
{
        CAMLparam2(ocaml_re, subject);
        CAMLlocal5(result, matches, match, name_table, matches_and_table);
        CAMLlocal1(match_opt);

        // Need to handle the lower bound manually since PCRE2 takes an
        // unsigned value. The upper bound must be checked for JIT matching
        // since pcre2_jit_match does not, and we want to consistently return
        // BADOFFSET rather than just failing to find for JIT only.
        if (subject_offset < 0
            || (use_jit && (size_t)subject_offset > subject_length_of(subject, pinned))) {
                // FIXME: result or option from this function? need to see if meaningful errors can
                // occur
                // SAFETY: This allocation is immediately filled with well-formed values prior to
                // returning.
                result = caml_alloc_small(1, RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(PCRE2_ERROR_BADOFFSET);
                CAMLreturn(result);
        }
        size_t offset = subject_offset;

        const pcre2_code *re = regex_of_value(ocaml_re)->regex;
        // TODO: support match/depth limits. Or callouts. May need to be
        // bundled with the compiled regex.
        pcre2_match_context *mcontext = NULL;
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        // NOTE: Really one more than number of captures since it includes the
        // full match.
        int num_captures =
            run_match(use_jit, re, subject, pinned, offset, options, match_data, mcontext);
        PCRE2_SIZE *ovec = pcre2_get_ovector_pointer(match_data);

        if (num_captures == PCRE2_ERROR_NOMATCH || num_captures == PCRE2_ERROR_PARTIAL) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, RESULT_OK_TAG);
                Field(result, 0) = Val_none;
                CAMLreturn(result);
        } else if (num_captures <= 0) {
                pcre2_match_data_free(match_data);
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(num_captures);
                CAMLreturn(result);
        }

        matches /* : (int * int) array */ = caml_alloc_tuple(num_captures);
        for (int i = 0; i < num_captures; ++i) {
                // SAFETY: This block must be filled with well-formed values
                // before the next allocation. The next allocation is no
                // earlier than the end of this loop iteration. All fields of
                // this tuple are assigned to by the end of the loop.
                match /* : int * int */ = caml_alloc_small(2, TUPLE_TAG);
                // The i-th capture group (the 0-th being the full match) is at
                // [2i, 2i+1] in ovec.
                int start = 2 * i;
                int end = 2 * i + 1;
                Field(match, 0) = Val_int(ovec[start]);
                Field(match, 1) = Val_int(ovec[end]);
                caml_modify(&Field(matches, i), match);
        }

        // TODO: cache this? May not be that expensive, but if it is then probably worth since we'll
        // match w/ capture many times.
        name_table = make_capture_group_name_table(re);

        pcre2_match_data_free(match_data);

        // SAFETY: This allocation is immediately filled with well-formed
        // values.
        matches_and_table /* : (int * int) array * (string * int) array */ =
            caml_alloc_small(2, TUPLE_TAG);
        Field(matches_and_table, 0) = matches;
        Field(matches_and_table, 1) = name_table;

        // SAFETY: This allocation is immediately filled with well-formed
        // values.
        match_opt /* : ((int * int) array * (string * int) array) option */ =
            caml_alloc_small(1, OPTION_SOME_TAG);
        Field(match_opt, 0) = matches_and_table;

        // SAFETY: This allocation is immediately filled with well-formed
        // values prior to returning.
        result /* : (((int * int) array * (string * int) array) option, _) Result.t */ =
            caml_alloc_small(1, RESULT_OK_TAG);
        Field(result, 0) = match_opt;

        CAMLreturn(result);
}

/// Match, with capture groups, the provided pattern.
CAMLprim value capture_unboxed(
    value ocaml_re /* : _ regex */, value subject /* : string */,
    intnat subject_offset /* : int [@untagged] */, uint32_t options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        return capture_general(ocaml_re, subject, false, false, subject_offset, options);
}

/// Boxed argument version of [capture_unboxed] (for bytecode).
CAMLprim value capture(
    value ocaml_re /* : _ regex */, value subject /* : string */, value subject_offset /* : int */,
    value options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        return capture_unboxed(ocaml_re, subject, Int_val(subject_offset), Int32_val(options));
}

/// Variant of [capture_unboxed] which takes a pinned subject.
CAMLprim value capture_pinned_unboxed(
    value ocaml_re /* : _ regex */, value subject /* : pinned_subject */,
    intnat subject_offset /* : int [@untagged] */, uint32_t options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        return capture_general(ocaml_re, subject, true, false, subject_offset, options);
}

/// Boxed argument version of [capture_pinned_unboxed] (for bytecode).
CAMLprim value capture_pinned(
    value ocaml_re /* : _ regex */, value subject /* : pinned_subject */,
    value subject_offset /* : int */, value options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        return capture_pinned_unboxed(ocaml_re, subject, Int_val(subject_offset),
                                      Int32_val(options));
}

/// Match, with capture groups, the provided JIT-enabled pattern.
CAMLprim value jit_capture_unboxed(
    value ocaml_re /* : jit regex */, value subject /* : string */,
    intnat subject_offset /* : int [@untagged] */, uint32_t options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        return capture_general(ocaml_re, subject, false, true, subject_offset, options);
}

/// Boxed argument version of [jit_capture_unboxed] (for bytecode).
CAMLprim value jit_capture(
    value ocaml_re /* : jit regex */, value subject /* : string */,
    value subject_offset /* : int */, value options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        return jit_capture_unboxed(ocaml_re, subject, Int_val(subject_offset), Int32_val(options));
}

/// Variant of [jit_capture_unboxed] which takes a pinned subject.
CAMLprim value jit_capture_pinned_unboxed(
    value ocaml_re /* : jit regex */, value subject /* : pinned_subject */,
    intnat subject_offset /* : int [@untagged] */, uint32_t options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        return capture_general(ocaml_re, subject, true, true, subject_offset, options);
}

/// Boxed argument version of [jit_capture_pinned_unboxed] (for bytecode).
CAMLprim value jit_capture_pinned(
    value ocaml_re /* : jit regex */, value subject /* : pinned_subject */,
    value subject_offset /* : int */, value options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        return jit_capture_pinned_unboxed(ocaml_re, subject, Int_val(subject_offset),
                                          Int32_val(options));
}
