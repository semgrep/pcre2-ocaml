// NOTE: Currently these bindings support only 8-bit code units. Below we use the generically named
// functions. Future versions could include support for non-8-bit code units.
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#define RESULT_OK_TAG (0)
#define RESULT_ERROR_TAG (1)

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
CAMLprim value pcre2_compile_stub(value pattern /* : string */,
                                  uint32_t options /* : int32 [@unboxed] */
                                  /* another arg for compile context options? */
                                  ) /* : -> (regex, int TODO OR? compile_error) Result.t */ {
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

        if (regex == NULL) {
                // Returns [Error e] since the pattern could not be compiled.
                result = caml_alloc_small(2, RESULT_ERROR_TAG);
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
        result = caml_alloc_small(2, RESULT_OK_TAG);
        Field(result, 0) = regex_value;

        CAMLreturn(result);
}

/// Match with the provided pattern.
///
/// @param[in] regex The compiled regex to use for matching.
/// @param[in] options Matching options, specified via a bitvector . See `pcre2_match(3)`.
// TODO: allow reusing the pcre2_match_data struct so that exec_all / find_iter
// / captures_iter can avoid a bunch of allocations. Ideally this function doesn't allocate (except
// maybe a result).
CAMLprim value pcre2_match_stub(value ocaml_re /* : regex */, value subject /* : string */,
                                int subject_offset /* : int [@untagged] */,
                                uint32_t options /* : int32 */
                                ) /* : -> match_ option */ {
        CAMLparam2(ocaml_re, subject);
        CAMLlocal1(result);

        if (subject_offset < 0) {
                // Need to handle this case manually since PCRE2 takes an unsigned value.
                result = caml_alloc_small(2, RESULT_ERROR_TAG);
                Field(result, 0) = PCRE2_ERROR_BADOFFSET;
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
        size_t *ovec = pcre2_get_ovector_pointer(match_data);

        if (ret < 0) {
                pcre2_match_data_free(match_data);
                result = caml_alloc_small(2, RESULT_ERROR_TAG);
                Field(result, 0) = ret;
                CAMLreturn(result);
        }

        pcre2_match_data_free(match_data);
        result = caml_alloc_small(2, RESULT_OK_TAG);
        Field(result, 0) = 1;
        CAMLreturn(result);
}

CAMLprim void pcre2_ocaml_init(void) {
        CAMLparam0();
        CAMLreturn0;
}
