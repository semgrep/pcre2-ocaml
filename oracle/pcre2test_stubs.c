/* Dev-only stubs backing the pcre2test-compatible harness's C-oracle driver.
 *
 * Unlike the production bindings (pcre2_c_stubs.c), these expose everything
 * the harness needs to reproduce pcre2test output byte-for-byte: compile
 * error offsets, a compile context (newline / BSR / extra options), the full
 * ovector, MARK values, and start-char. Self-contained: own custom block.
 */
#include <stdint.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

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

        pcre2_code *regex = pcre2_compile((PCRE2_SPTR)String_val(pattern), pattern_len, options,
                                          &error_code, &error_offset, ccontext);
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

/* exec : code -> string -> int (offset) -> int (options)
 *      -> int (rc) * int array (ovector, unset = -1) * string option (mark)
 *      * int (startchar) */
CAMLprim value oracle_test_exec(value vcode, value subject, value voffset, value voptions) {
        CAMLparam4(vcode, subject, voffset, voptions);
        CAMLlocal4(res, ovec_arr, mark_opt, mark_str);

        pcre2_code *regex = test_code_of_value(vcode);
        pcre2_match_data *md = pcre2_match_data_create_from_pattern(regex, NULL);
        PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(md);
        uint32_t oveccount = pcre2_get_ovector_count(md);

        /* Match pcre2test: make unset groups deterministic. */
        for (uint32_t i = 0; i < 2 * oveccount; i++) ovector[i] = PCRE2_UNSET;

        int rc = pcre2_match(regex, (PCRE2_SPTR)String_val(subject), caml_string_length(subject),
                             (PCRE2_SIZE)Long_val(voffset), (uint32_t)Long_val(voptions), md, NULL);

        ovec_arr = caml_alloc(2 * oveccount, 0);
        for (uint32_t i = 0; i < 2 * oveccount; i++) {
                long v = (ovector[i] == PCRE2_UNSET) ? -1 : (long)ovector[i];
                Store_field(ovec_arr, i, Val_long(v));
        }

        PCRE2_SPTR mark = pcre2_get_mark(md);
        if (mark == NULL) {
                mark_opt = Val_long(0); /* None */
        } else {
                /* MARK names are stored zero-terminated in the compiled code. */
                mark_str = caml_copy_string((const char *)mark);
                mark_opt = caml_alloc_small(1, 0); /* Some */
                Field(mark_opt, 0) = mark_str;
        }

        long startchar = (long)pcre2_get_startchar(md);
        pcre2_match_data_free(md);

        res = caml_alloc_small(4, 0);
        Field(res, 0) = Val_long(rc);
        Field(res, 1) = ovec_arr;
        Field(res, 2) = mark_opt;
        Field(res, 3) = Val_long(startchar);
        CAMLreturn(res);
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
