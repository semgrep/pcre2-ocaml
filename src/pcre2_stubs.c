// NOTE: Currently these bindings support only 8-bit code units. Below we use the generically named
// functions. Future versions could include support for non-8-bit code units.
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "caml/alloc.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#if __STDC_VERSION__ >= 202311L
#define UNUSED [[maybe_unused]]
#else
#define UNUSED __attribute__((unused))
#endif

const int OPTION_SOME_TAG = 0;
const int RESULT_OK_TAG = 0;
const int RESULT_ERROR_TAG = 1;
const int TUPLE_TAG = 0;
const int ARRAY_TAG = 0;

/// The maximum size which is used for a temporary array to count the number of strings
/// corresponding to each numbered capture group.
///
/// This should be large enough that most (reasonable) patterns have fewer
/// numbered capture groups than this value, but small enough it is reasonable
/// to have an array of this size for each capturing match.
#define MAX_SMALL_NAMES_ARRAY_LEN (64)

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
CAMLprim value compile(value *argv, int argc UNUSED) {
        return compile_unboxed(argv[0], Int32_val(argv[1]));
}

/// Match with the provided pattern.
///
/// @param[in] regex The compiled regex to use for matching.
/// @param[in] options Matching options, specified via a bitvector . See `pcre2_match(3)`.
// TODO: allow reusing the pcre2_match_data struct so that exec_all / find_iter
// / captures_iter can avoid a bunch of allocations. Ideally this function doesn't allocate
// (except maybe a result).
CAMLprim value match_unboxed(value ocaml_re /* : _ regex */, value subject /* : string */,
                             intnat subject_offset /* : int [@untagged] */,
                             uint32_t options /* : int32 */
                             ) /* : -> ((int * int) option, int) Result.t */ {
        CAMLparam2(ocaml_re, subject);
        CAMLlocal3(result, range, match);

        // Need to handle this case manually since PCRE2 takes an unsigned value.
        if (subject_offset < 0) {
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(PCRE2_ERROR_BADOFFSET);
                CAMLreturn(result);
        }
        size_t offset = subject_offset;
        size_t subject_length = caml_string_length(subject);

        const pcre2_code *re = regex_of_value(ocaml_re)->regex;
        // TODO: support match/depth limits. Or callouts. May need to be
        // bundled with the compiled regex.
        pcre2_match_context *mcontext = NULL;
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        int ret = pcre2_match(re, (PCRE2_SPTR)String_val(subject), subject_length, offset, options,
                              match_data, mcontext);
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

/// Boxed argument version of [jit_match_unboxed] (for bytecode).
CAMLprim value match(value *argv, int argc UNUSED) {
        return match_unboxed(argv[0], argv[1], Nativeint_val(argv[2]), Int32_val(argv[3]));
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
                Field(0, result) = Val_int(res);
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
        Field(0, result) = ocaml_re;
        CAMLreturn(result);
}

/// Boxed argument version of [jit_compile_unboxed] (for bytecode).
CAMLprim value jit_compile(value *argv, int argc UNUSED) {
        return jit_compile_unboxed(argv[0], Int32_val(argv[1]));
}

/// Match with the provided JIT compiled regex.
///
/// @param[in] regex The JIT-enabled regex.
/// @param[in] options Matching options, specified via a bitvector . See
/// `pcre2_match(3)`. NOTE: PCRE2_ZERO_TERMINATED is not supported.
// TODO: Just drop PCRE2_ZERO_TERMINATED? I don't think it much makes sense if we take ocaml
// strings.
CAMLprim value jit_match_unboxed(value ocaml_re /* : jit regex */, value subject /* : string */,
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
        CAMLlocal3(result, range, match);

        // Need to handle this case manually since PCRE2 takes an unsigned value.
        if (subject_offset < 0) {
                // SAFETY: This allocation is immediately filled with
                // well-formed values prior to returning.
                result = caml_alloc_small(1, RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(PCRE2_ERROR_BADOFFSET);
                CAMLreturn(result);
        }
        size_t offset = subject_offset;
        size_t subject_length = caml_string_length(subject);

        const pcre2_code *re = regex_of_value(ocaml_re)->regex;
        // TODO: support match/depth limits. Or callouts. May need to be
        // bundled with the compiled regex.
        pcre2_match_context *mcontext = NULL;
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        int ret = pcre2_jit_match(re, (PCRE2_SPTR)String_val(subject), subject_length, offset,
                                  options, match_data, mcontext);
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
        Field(result, 0) = range;

        CAMLreturn(result);
}

/// Boxed argument version of [jit_match_unboxed] (for bytecode).
CAMLprim value jit_match(value *argv, int argc UNUSED) {
        return jit_match_unboxed(argv[0], argv[1], Int_val(argv[2]), Int32_val(argv[3]));
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

value make_capture_group_name_table(const pcre2_code *re) /* -> (string * int) array */ {
        CAMLparam0();
        CAMLlocal3(name, pair, array);

        uint32_t name_count;
        uint32_t entry_size;
        PCRE2_SPTR names = names_of_regex(re, &name_count, &entry_size);

        if (!names) {
                array = caml_alloc_small(0, ARRAY_TAG);
                // SAFETY(caml_alloc_small): There are no fields in the
                // allocation, so it is trivially well-formed.
                CAMLreturn(array);
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

/// Match with the provided pattern.
///
/// @param[in] regex The compiled regex to use for matching.
/// @param[in] options Matching options, specified via a bitvector . See `pcre2_match(3)`.
// TODO: allow reusing the pcre2_match_data struct so that exec_all / find_iter
// / captures_iter can avoid a bunch of allocations. Ideally this function doesn't allocate
// (except maybe a result).
CAMLprim value capture_unboxed(
    value ocaml_re /* : _ regex */, value subject /* : string */,
    intnat subject_offset /* : int [@untagged] */, uint32_t options /* : int32 */
    ) /* : -> (((int * int) array * (string * int) array) option, match_error) Result.t */ {
        CAMLparam2(ocaml_re, subject);
        CAMLlocal5(result, matches, match, name, name_table);
        CAMLlocal2(matches_and_table, match_opt);

        if (subject_offset < 0) {
                // Need to handle this case manually since PCRE2 takes an unsigned value.
                // FIXME: result or option from this function? need to see if meaningful errors can
                // occur
                // SAFETY: This allocation is immediately filled with well-formed values prior to
                // returning.
                result = caml_alloc_small(1, RESULT_ERROR_TAG);
                Field(result, 0) = Val_int(PCRE2_ERROR_BADOFFSET);
                CAMLreturn(result);
        }
        size_t offset = subject_offset;
        size_t subject_length = caml_string_length(subject);

        const pcre2_code *re = regex_of_value(ocaml_re)->regex;
        // TODO: support match/depth limits. Or callouts. May need to be
        // bundled with the compiled regex.
        pcre2_match_context *mcontext = NULL;
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        // NOTE: Really one more than number of captures since it includes the
        // full match.
        int num_captures = pcre2_match(re, (PCRE2_SPTR)String_val(subject), subject_length, offset,
                                       options, match_data, mcontext);
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

/// Boxed argument version of [capture_unboxed] (for bytecode).
CAMLprim value capture(value *argv, int argc UNUSED) {
        return match_unboxed(argv[0], argv[1], Nativeint_val(argv[2]), Int32_val(argv[3]));
}

///
CAMLprim value get_capture_groups(value ocaml_regex /* : regex */) /* -> (string * int) array */ {
        CAMLparam1(ocaml_regex);
        CAMLreturn(make_capture_group_name_table(regex_of_value(ocaml_regex)->regex));
}
