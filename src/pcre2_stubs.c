/*
   PCRE2-OCAML - Perl Compatibility Regular Expressions for OCaml

   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#if defined(_WIN32)
#define snprintf _snprintf
#endif

#if __GNUC__ >= 3
#define __unused __attribute__((unused))
#else
#define __unused
#endif

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

// NOTE: Currently these bindings support only 8-bit code units. Below we use the generically named
// functions. Future versions could include support for non-8-bit code units.
#define PCRE2_CODE_UNIT_WIDTH 8

#include <pcre2.h>

typedef const unsigned char *chartables; /* Type of chartable sets */

/* Contents of callout data */
struct cod {
        size_t subj_start;       /* Start of subject string */
        value *substrings;       /* Pointer to substrings matched so far */
        value *callout_function; /* Pointer to callout function */
        value exn;               /* Possible exception raised by callout function */
};

/* Cache for exceptions */
static const value *pcre2_exc_Error = NULL;     /* Exception [Error] */
static const value *pcre2_exc_Backtrack = NULL; /* Exception [Backtrack] */

/* Cache for polymorphic variants */
static value var_Start_only; /* Variant [`Start_only] */
static value var_ANCHORED;   /* Variant [`ANCHORED] */
static value var_Char;       /* Variant [`Char char] */

/* Data associated with OCaml values of PCRE regular expression */
struct pcre2_ocaml_regexp {
        pcre2_code *rex;
        pcre2_match_context *mcontext;
};

#define Pcre2_ocaml_regexp_val(v) ((struct pcre2_ocaml_regexp *)Data_custom_val(v))

#define get_rex(v) Pcre2_ocaml_regexp_val(v)->rex
#define get_mcontext(v) Pcre2_ocaml_regexp_val(v)->mcontext

#define set_rex(v, r) Pcre2_ocaml_regexp_val(v)->rex = r
#define set_mcontext(v, c) Pcre2_ocaml_regexp_val(v)->mcontext = c

/* Data associated with OCaml values of PCRE tables */
struct pcre2_ocaml_tables {
        chartables tables;
};

#define Pcre2_ocaml_tables_val(v) ((struct pcre2_ocaml_tables *)Data_custom_val(v))

#define get_tables(v) Pcre2_ocaml_tables_val(v)->tables
#define set_tables(v, t) Pcre2_ocaml_tables_val(v)->tables = t

/// Copies the contents of `n` elements of the PCRE2 ovector `src` to the ocaml
/// array `dst`, returning `&dst[n]`. Each copied value with have `subj_start` added to it to
/// account for any user-specified offset.
static inline value *copy_ovector(value *dst, const PCRE2_SIZE *src, size_t n, size_t subj_start) {
        for (size_t i = 0; i < n; ++i) {
                // In theory a long may not hold this, but realistically subjects will never be long
                // enough to cause an issue.
                *dst = Val_long(*src + subj_start);
                ++src;
                ++dst;
        }

        return dst;
}

/* Callout handler */
// See <https://www.pcre.org/current/doc/html/pcre2callout.html> for additional information about
// PCRE2 callouts.
static int pcre2_callout_handler(pcre2_callout_block *cb, void *data) {
        CAMLparam0();
        CAMLlocal2(callout_data, ocaml_result);

        if (data == NULL) {
                return 0;
        }
        struct cod *cod = data;

        const value substrings = *cod->substrings;
        // `cb->capture_top` is one more than the number of the higest numbered
        // captured substring (thus far). So then, for the ovector, to find the
        // offsets for subgroups which are captured we can look from `ovec[2]`
        // to `ovec[2 * cb->capture_top - 1]`.
        //
        // N.B.: `ovec[0]` and `ovec[1]` will always be `PCRE2_UNSET` since the
        // match is not complete (since we are in a callout).
        //
        // See
        // <https://www.pcre.org/current/doc/html/pcre2callout.html#:~:text=Fields%20for%20all%20callouts>
        // for more information about this in the context of callouts.
        // See <https://www.pcre.org/current/doc/html/pcre2api.html#SEC29> for more discussion of
        // the ovector format in PCRE2.
        const PCRE2_SIZE full_match_ovec_offset = 2;
        const uint32_t num_offsets = (2 * cb->capture_top) - full_match_ovec_offset;

        const PCRE2_SIZE *ovec_src = cb->offset_vector + full_match_ovec_offset;
        value *ovec_dst = &Field(Field(substrings, 1), 0);
        size_t subj_start = cod->subj_start;

        copy_ovector(ovec_dst, ovec_src, num_offsets, subj_start);

        // NOTE: direct assignment here is fine since callout data is allocated
        // with caml_alloc_small. See
        // <https://ocaml.org/manual/5.1/intfc.html>, namely rules 5 and 6.
        callout_data = caml_alloc_small(8, 0);
        Field(callout_data, 0) = Val_int(cb->callout_number);
        Field(callout_data, 1) = substrings;

        // Add the subj_start to account for user provided offset in matching.
        Field(callout_data, 2) = Val_int(cb->start_match + subj_start);
        Field(callout_data, 3) = Val_int(cb->current_position + subj_start);

        Field(callout_data, 4) = Val_int(cb->capture_top);
        Field(callout_data, 5) = Val_int(cb->capture_last);
        Field(callout_data, 6) = Val_int(cb->pattern_position);
        Field(callout_data, 7) = Val_int(cb->next_item_length);

        // Perform callout
        ocaml_result = caml_callback_exn(*cod->callout_function, callout_data);

        if (Is_exception_result(ocaml_result)) {
                // Callout raised an exception
                const value exn = Extract_exception(ocaml_result);
                if (Field(exn, 0) == *pcre2_exc_Backtrack) {
                        CAMLreturnT(int, 1);
                }
                cod->exn = exn;
                CAMLreturnT(int, PCRE2_ERROR_CALLOUT);
        }

        CAMLreturnT(int, 0);
}

/* Fetches the named OCaml-values + caches them and
   calculates + caches the variant hash values */
CAMLprim value pcre2_ocaml_init(value __unused v_unit) {
        pcre2_exc_Error = caml_named_value("Pcre2.Error");
        pcre2_exc_Backtrack = caml_named_value("Pcre2.Backtrack");

        var_Start_only = caml_hash_variant("Start_only");
        var_ANCHORED = caml_hash_variant("ANCHORED");
        var_Char = caml_hash_variant("Char");

        return Val_unit;
}

/* Finalizing deallocation function for chartable sets */
static void pcre2_dealloc_tables(value v_tables) {
#if PCRE2_MINOR >= 34
        pcre2_maketables_free(NULL, get_tables(v_tables));
#else
        free(get_tables(v_tables));
#endif
}

/* Finalizing deallocation function for compiled regular expressions */
static void pcre2_dealloc_regexp(value v_rex) {
        pcre2_code_free(get_rex(v_rex));
        pcre2_match_context_free(get_mcontext(v_rex));
}

/* Raising exceptions */

CAMLnoreturn_start static inline void raise_pcre2_error(value v_arg) CAMLnoreturn_end;
CAMLnoreturn_start static inline void raise_partial(void) CAMLnoreturn_end;
CAMLnoreturn_start static inline void raise_bad_utf(void) CAMLnoreturn_end;
CAMLnoreturn_start static inline void raise_bad_utf_offset(void) CAMLnoreturn_end;
CAMLnoreturn_start static inline void raise_match_limit(void) CAMLnoreturn_end;
CAMLnoreturn_start static inline void raise_depth_limit(void) CAMLnoreturn_end;
CAMLnoreturn_start static inline void raise_workspace_size(void) CAMLnoreturn_end;
CAMLnoreturn_start static inline void raise_bad_pattern(int code, size_t pos) CAMLnoreturn_end;
CAMLnoreturn_start static inline void raise_internal_error(const char *msg) CAMLnoreturn_end;

static inline void raise_pcre2_error(value v_arg) {
        caml_raise_with_arg(*pcre2_exc_Error, v_arg);
}

static inline void raise_partial(void) {
        raise_pcre2_error(Val_int(0));
}

static inline void raise_bad_utf(void) {
        raise_pcre2_error(Val_int(1));
}

static inline void raise_bad_utf_offset(void) {
        raise_pcre2_error(Val_int(2));
}

static inline void raise_match_limit(void) {
        raise_pcre2_error(Val_int(3));
}

static inline void raise_depth_limit(void) {
        raise_pcre2_error(Val_int(4));
}

static inline void raise_workspace_size(void) {
        raise_pcre2_error(Val_int(5));
}

static inline void raise_bad_pattern(int code, size_t pos) {
        CAMLparam0();
        CAMLlocal1(v_msg);
        value v_arg;
        char msg_buf[128];
        pcre2_get_error_message(code, (PCRE2_UCHAR *)msg_buf,
                                (sizeof msg_buf) / (sizeof(PCRE2_UCHAR)));
        v_msg = caml_copy_string(msg_buf);
        v_arg = caml_alloc_small(2, 0);
        Field(v_arg, 0) = v_msg;
        Field(v_arg, 1) = Val_int(pos);
        raise_pcre2_error(v_arg);
        CAMLnoreturn;
}

static inline void raise_internal_error(const char *msg) {
        CAMLparam0();
        CAMLlocal2(v_msg, v_arg);
        v_msg = caml_copy_string(msg);
        v_arg = caml_alloc_small(1, 1);
        Field(v_arg, 0) = v_msg;
        raise_pcre2_error(v_arg);
        CAMLnoreturn;
}

/* PCRE pattern compilation */

static struct custom_operations regexp_ops = {
    "pcre2_ocaml_regexp",       pcre2_dealloc_regexp,       custom_compare_default,
    custom_hash_default,        custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

/* Makes compiled regular expression from compilation options, an optional
   value of chartables and the pattern string */

CAMLprim value pcre2_compile_stub(value jit, int64_t v_opt, value v_tables, value v_pat) {
        value v_rex; /* Final result -> value of type [regexp] */
        size_t ocaml_regexp_size = sizeof(struct pcre2_ocaml_regexp);
        int error_code = 0;   /* error code for potential error */
        size_t error_ofs = 0; /* offset in the pattern at which error occurred */
        size_t length = caml_string_length(v_pat);

        pcre2_compile_context *ccontext = NULL;
        /* If v_tables = [None], then pointer to tables is NULL, otherwise
           set it to the appropriate value */
        if (Is_some(v_tables)) {
                ccontext = pcre2_compile_context_create(NULL);
                pcre2_set_character_tables(ccontext, get_tables(Field(v_tables, 0)));
        }

        /* Compiles the pattern */
        pcre2_code *regexp =
            pcre2_compile(Bytes_val(v_pat), length, v_opt, &error_code, &error_ofs, ccontext);

        pcre2_compile_context_free(ccontext);

        /* Raises appropriate exception with [BadPattern] if the pattern
           could not be compiled */
        if (regexp == NULL) {
                raise_bad_pattern(error_code, error_ofs);
        }

        if (Bool_val(jit)) {
                // See also <https://pcre.org/current/doc/html/pcre2jit.html>
                if (pcre2_jit_compile(regexp, PCRE2_JIT_COMPLETE) < 0) {
                        raise_internal_error("issue in JIT compilation");
                }
        }

        /* It's unknown at this point whether JIT compilation is going to be used,
           but we have to decide on a size.  Tests with some simple patterns indicate a
           roughly 50% increase in size when studying without JIT.  A factor of
           two times hence seems like a reasonable bound to use here. */
        size_t regexp_size;
        pcre2_pattern_info(regexp, PCRE2_INFO_SIZE, &regexp_size);
        v_rex = caml_alloc_custom_mem(&regexp_ops, ocaml_regexp_size, 2 * regexp_size);

        set_rex(v_rex, regexp);
        set_mcontext(v_rex, pcre2_match_context_create(NULL));

        return v_rex;
}

CAMLprim value pcre2_compile_stub_bc(value jit, value v_opt, value v_tables, value v_pat) {
        return pcre2_compile_stub(jit, Int64_val(v_opt), v_tables, v_pat);
}

/* Sets a match limit for a regular expression imperatively */
CAMLprim value pcre2_set_imp_match_limit_stub(value v_rex, intnat v_lim) {
        pcre2_match_context *mcontext = get_mcontext(v_rex);
        pcre2_set_match_limit(mcontext, v_lim);
        return v_rex;
}

CAMLprim value pcre2_set_imp_match_limit_stub_bc(value v_rex, value v_lim) {
        return pcre2_set_imp_match_limit_stub(v_rex, Int_val(v_lim));
}

/* Sets a depth limit for a regular expression imperatively */
CAMLprim value pcre2_set_imp_depth_limit_stub(value v_rex, intnat v_lim) {
        pcre2_match_context *mcontext = get_mcontext(v_rex);
        pcre2_set_depth_limit(mcontext, v_lim);
        return v_rex;
}

CAMLprim value pcre2_set_imp_depth_limit_stub_bc(value v_rex, value v_lim) {
        return pcre2_set_imp_depth_limit_stub(v_rex, Int_val(v_lim));
}

/* Performs the call to the pcre2_pattern_info function */
static inline int pcre2_pattern_info_stub(value v_rex, int what, void *where) {
        return pcre2_pattern_info(get_rex(v_rex), what, where);
}

/* Some stubs for info-functions */

/* clang-format off */ 
/* Generic macro for getting integer results from pcre2_pattern_info */
#define make_intnat_info(tp, name, option)                                                     \
        CAMLprim intnat pcre2_##name##_stub(value v_rex) {                                     \
                tp options;                                                                    \
                const int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_##option, &options); \
                if (ret != 0)                                                                  \
                        raise_internal_error("pcre2_##name##_stub");                           \
                return options;                                                                \
        }                                                                                      \
                                                                                               \
        CAMLprim value pcre2_##name##_stub_bc(value v_rex) {                                   \
                return Val_int(pcre2_##name##_stub(v_rex));                                    \
        }


make_intnat_info(size_t, size, SIZE)
make_intnat_info(int, capturecount, CAPTURECOUNT)
make_intnat_info(int, backrefmax, BACKREFMAX)
make_intnat_info(int, namecount, NAMECOUNT)
make_intnat_info(int, nameentrysize, NAMEENTRYSIZE)
    /* clang-format on */

    CAMLprim int64_t pcre2_argoptions_stub(value v_rex) {
        uint32_t options;
        const int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_ARGOPTIONS, &options);
        if (ret != 0) {
                raise_internal_error("pcre2_argoptions_stub");
        }
        return (int64_t)options;
}

CAMLprim value pcre2_argoptions_stub_bc(value v_rex) {
        CAMLparam1(v_rex);
        CAMLreturn(caml_copy_int64(pcre2_argoptions_stub(v_rex)));
}

CAMLprim value pcre2_firstcodeunit_stub(value v_rex) {
        uint32_t firstcodetype;
        const int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_FIRSTCODETYPE, &firstcodetype);

        if (ret != 0) {
                raise_internal_error("pcre2_firstcodeunit_stub");
        }

        switch (firstcodetype) {
        // start of string or after newline
        case 2: {
                return var_Start_only;
        }
        // nothing set
        case 0: {
                return var_ANCHORED;
        }
        // first code unit is set
        case 1: {
                uint32_t firstcodeunit;
                const int ret =
                    pcre2_pattern_info_stub(v_rex, PCRE2_INFO_FIRSTCODEUNIT, &firstcodeunit);
                if (ret != 0) {
                        raise_internal_error("pcre2_firstcodeunit_stub");
                }

                value v_firstbyte;
                /* Allocates the non-constant constructor [`Char of char] and fills
                   in the appropriate value */
                v_firstbyte = caml_alloc_small(2, 0);
                Field(v_firstbyte, 0) = var_Char;
                Field(v_firstbyte, 1) = Val_int(firstcodeunit);

                return v_firstbyte;
        }
        default: { /* Should not happen */
                raise_internal_error("pcre2_firstcodeunit_stub");
        }
        }
}

CAMLprim value pcre2_lastcodeunit_stub(value v_rex) {
        uint32_t lastcodetype;
        const int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_LASTCODETYPE, &lastcodetype);

        if (ret != 0) {
                raise_internal_error("pcre2_lastcodeunit_stub");
        }

        switch (lastcodetype) {
        case 0: {
                return Val_none;
        }
        case 1: {
                uint32_t lastcodeunit;
                const int ret =
                    pcre2_pattern_info_stub(v_rex, PCRE2_INFO_LASTCODEUNIT, &lastcodeunit);
                if (ret != 0) {
                        raise_internal_error("pcre2_lastcodeunit_stub");
                }
                return caml_alloc_some(Val_int(lastcodeunit));
        }
        default: { /* Should not happen */
                raise_internal_error("pcre2_lastcodeunit_stub");
        }
        }
}

CAMLnoreturn_start static inline void handle_match_error(char *loc, const int ret) CAMLnoreturn_end;

static inline void handle_match_error(char *loc, const int ret) {
        switch (ret) {
        /* Dedicated exceptions */
        case PCRE2_ERROR_NOMATCH:
                caml_raise_not_found();
        case PCRE2_ERROR_PARTIAL:
                raise_partial();
        case PCRE2_ERROR_MATCHLIMIT:
                raise_match_limit();
        case PCRE2_ERROR_BADUTFOFFSET:
                raise_bad_utf_offset();
        case PCRE2_ERROR_DEPTHLIMIT:
                raise_depth_limit();
        case PCRE2_ERROR_DFA_WSSIZE:
                raise_workspace_size();
        default: {
                // NOTE: PCRE2_ERROR_* are negative, hence why the higher numbered error is our
                // lower bound here.
                if (PCRE2_ERROR_UTF8_ERR21 <= ret && ret <= PCRE2_ERROR_UTF8_ERR1) {
                        raise_bad_utf();
                }
                /* Unknown error */
                char err_buf[100];
                snprintf(err_buf, 100, "%s: unhandled PCRE2 error code: %d", loc, ret);
                raise_internal_error(err_buf);
        }
        }
}

static inline void handle_pcre2_match_result(size_t *ovec, value v_ovec, size_t ovec_len,
                                             size_t subj_start, uint32_t match_ret) {
        CAMLparam1(v_ovec);

        const size_t num_offsets = 2 * match_ret;
        value *dst = &Field(v_ovec, 0);
        // Need to clear 2/3rd of the ovector since the first 2/3rds are
        // actual substrings and the last 3rd is just scratch space.
        value *clear_until = dst + (ovec_len * 2) / 3;

        dst = copy_ovector(dst, ovec, num_offsets, subj_start);
        // We do this so that excess capture groups which may be inspected in
        // OCaml are correctly reported as having no match.
        // See <https://github.com/mmottl/pcre-ocaml/issues/5>
        while (dst < clear_until) {
                *dst = Val_int(-1);
                ++dst;
        }

        CAMLreturn0;
}

/* Executes a pattern match with runtime options, a regular expression, a
   matching position, the start of the the subject string, a subject string,
   a number of subgroup offsets, an offset vector and an optional callout
   function */

CAMLprim value pcre2_match_stub0(int64_t v_opt, value v_rex, intnat v_pos, intnat v_subj_start,
                                 value v_subj, value v_ovec, value v_maybe_cof, value v_workspace) {
        int ret;
        int is_dfa = v_workspace != (value)NULL;
        long pos = v_pos;
        long subj_start = v_subj_start;
        size_t ovec_len = Wosize_val(v_ovec);
        size_t len = caml_string_length(v_subj);

        if (pos > (long)len || pos < subj_start) {
                caml_invalid_argument("Pcre2.pcre2_match_stub: illegal position");
        }

        if (subj_start > (long)len || subj_start < 0) {
                caml_invalid_argument("Pcre2.pcre2_match_stub: illegal subject start");
        }

        pos -= subj_start;
        len -= subj_start;

        const pcre2_code *code = get_rex(v_rex);                             /* Compiled pattern */
        pcre2_match_context *mcontext = get_mcontext(v_rex);                 /* Match context */
        PCRE2_SPTR ocaml_subj = (PCRE2_SPTR)String_val(v_subj) + subj_start; /* Subject string */

        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(code, NULL);

        /* Special case when no callout functions specified */
        if (Is_none(v_maybe_cof)) {
                /* Performs the match */
                if (is_dfa) {
                        ret =
                            pcre2_dfa_match(code, ocaml_subj, len, pos, v_opt, match_data, mcontext,
                                            (int *)&Field(v_workspace, 0), Wosize_val(v_workspace));
                } else {
                        ret = pcre2_match(code, ocaml_subj, len, pos, v_opt, match_data, mcontext);
                }

                size_t *ovec = pcre2_get_ovector_pointer(match_data);

                if (ret < 0) {
                        pcre2_match_data_free(match_data);
                        handle_match_error("pcre2_match_stub", ret);
                } else {
                        handle_pcre2_match_result(ovec, v_ovec, ovec_len, subj_start, ret);
                }
        }

        /* There are callout functions */
        else {
                value v_cof = Field(v_maybe_cof, 0);
                value v_substrings;
                PCRE2_UCHAR *subj = caml_stat_alloc(sizeof(char) * len);
                int workspace_len;
                int *workspace;
                struct cod cod = {
                    .subj_start = 0, .substrings = NULL, .callout_function = NULL, .exn = Val_unit};
                pcre2_match_context *new_mcontext = pcre2_match_context_copy(mcontext);

                pcre2_set_callout(new_mcontext, pcre2_callout_handler, &cod);

                cod.subj_start = subj_start;
                memcpy(subj, ocaml_subj, len);

                Begin_roots4(v_rex, v_cof, v_substrings, v_ovec);
                Begin_roots1(v_subj);
                v_substrings = caml_alloc_small(2, 0);
                End_roots();

                Field(v_substrings, 0) = v_subj;
                Field(v_substrings, 1) = v_ovec;

                cod.substrings = &v_substrings;
                cod.callout_function = &v_cof;

                if (is_dfa) {
                        workspace_len = Wosize_val(v_workspace);
                        workspace = caml_stat_alloc(sizeof(int) * workspace_len);
                        ret = pcre2_dfa_match(code, subj, len, pos, v_opt, match_data, new_mcontext,
                                              (int *)&Field(v_workspace, 0), workspace_len);
                } else
                        ret = pcre2_match(code, subj, len, pos, v_opt, match_data, new_mcontext);

                caml_stat_free(subj);
                End_roots();

                pcre2_match_context_free(new_mcontext);
                size_t *ovec = pcre2_get_ovector_pointer(match_data);
                if (ret < 0) {
                        if (is_dfa) {
                                caml_stat_free(workspace);
                        }
                        pcre2_match_data_free(match_data);
                        if (ret == PCRE2_ERROR_CALLOUT) {
                                caml_raise(cod.exn);
                        } else {
                                handle_match_error("pcre2_match_stub(callout)", ret);
                        }
                } else {
                        handle_pcre2_match_result(ovec, v_ovec, ovec_len, subj_start, ret);
                        if (is_dfa) {
                                value *ocaml_workspace_dst = &Field(v_workspace, 0);
                                const int *workspace_src = workspace;
                                const int *workspace_src_stop = workspace + workspace_len;
                                while (workspace_src != workspace_src_stop) {
                                        *ocaml_workspace_dst = *workspace_src;
                                        ocaml_workspace_dst++;
                                        workspace_src++;
                                }
                                caml_stat_free(workspace);
                        }
                }
        }
        pcre2_match_data_free(match_data);

        return Val_unit;
}

CAMLprim value pcre2_match_stub(int64_t v_opt, value v_rex, intnat v_pos, intnat v_subj_start,
                                value v_subj, value v_ovec, value v_maybe_cof) {
        return pcre2_match_stub0(v_opt, v_rex, v_pos, v_subj_start, v_subj, v_ovec, v_maybe_cof,
                                 (value)NULL);
}

/* Byte-code hook for pcre2_match_stub
   Needed, because there are more than 5 arguments */
CAMLprim value pcre2_match_stub_bc(value *argv, int __unused argn) {
        return pcre2_match_stub0(Int64_val(argv[0]), argv[1], Int_val(argv[2]), Int_val(argv[3]),
                                 argv[4], argv[5], argv[6], (value)NULL);
}

/* Byte-code hook for pcre2_dfa_match_stub
   Needed, because there are more than 5 arguments */
CAMLprim value pcre2_dfa_match_stub_bc(value *argv, int __unused argn) {
        return pcre2_match_stub0(Int64_val(argv[0]), argv[1], Int_val(argv[2]), Int_val(argv[3]),
                                 argv[4], argv[5], argv[6], argv[7]);
}

static struct custom_operations tables_ops = {
    "pcre2_ocaml_tables",       pcre2_dealloc_tables,       custom_compare_default,
    custom_hash_default,        custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

/* Generates a new set of chartables for the current locale (see man
   page of PCRE */
CAMLprim value pcre2_maketables_stub(value __unused v_unit) {
        /* According to testing with `malloc_size`, it seems that a typical set of
           tables will require about 1536 bytes of memory.  This may or may not
           be true on other platforms or for all versions of PCRE.  Since there
           is apparently no reliable way of finding out, 1536 is probably a good
           default value. */
        size_t tables_size = sizeof(struct pcre2_ocaml_tables);
        const value v_tables = caml_alloc_custom_mem(&tables_ops, tables_size, 1536);
        set_tables(v_tables, pcre2_maketables(NULL));
        return v_tables;
}

/* Wraps around the isspace-function */
CAMLprim value pcre2_isspace_stub(value v_c) {
        return Val_bool(isspace(Int_val(v_c)));
}

/* Returns number of substring associated with a name */

CAMLprim intnat pcre2_substring_number_from_name_stub(value v_rex, value v_name) {
        const int ret =
            pcre2_substring_number_from_name(get_rex(v_rex), (PCRE2_SPTR)String_val(v_name));
        if (ret == PCRE2_ERROR_NOSUBSTRING)
                caml_invalid_argument("Named string not found");

        return ret;
}

CAMLprim value pcre2_substring_number_from_name_stub_bc(value v_rex, value v_name) {
        return Val_int(pcre2_substring_number_from_name_stub(v_rex, v_name));
}

/* Returns array of names of named substrings in a regexp */
CAMLprim value pcre2_names_stub(value v_rex) {
        CAMLparam1(v_rex);
        CAMLlocal1(v_res);
        uint32_t name_count;
        uint32_t entry_size;
        const char *tbl_ptr;

        int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_NAMECOUNT, &name_count);
        if (ret != 0) {
                raise_internal_error("pcre2_names_stub: namecount");
        }

        ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_NAMEENTRYSIZE, &entry_size);
        if (ret != 0) {
                raise_internal_error("pcre2_names_stub: nameentrysize");
        }

        ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_NAMETABLE, &tbl_ptr);
        if (ret != 0) {
                raise_internal_error("pcre2_names_stub: nametable");
        }

        v_res = caml_alloc(name_count, 0);

        for (uint32_t i = 0; i < name_count; ++i) {
                value v_name = caml_copy_string(tbl_ptr + 2);
                Store_field(v_res, i, v_name);
                tbl_ptr += entry_size;
        }

        CAMLreturn(v_res);
}

/* Generic stub for getting integer results from pcre2_config */
static inline int pcre2_config_int(int what) {
        int ret;
        pcre2_config(what, &ret);
        return ret;
}

/* Generic stub for getting long integer results from pcre2_config */
static inline long pcre2_config_long(int what) {
        long ret;
        pcre2_config(what, &ret);
        return ret;
}

/* Some stubs for config-functions */

/* Makes OCaml-string from PCRE-version */
CAMLprim value pcre2_version_stub(value __unused v_unit) {
        CAMLparam1(v_unit);
        CAMLlocal1(v_version);
        v_version = caml_alloc_string(32);

        pcre2_config(PCRE2_CONFIG_VERSION, Bytes_val(v_version));

        CAMLreturn(v_version);
}

/* Returns boolean indicating unicode support */
CAMLprim value pcre2_config_unicode_stub(value __unused v_unit) {
        return Val_bool(pcre2_config_int(PCRE2_CONFIG_UNICODE));
}

/* Returns character used as newline */
CAMLprim value pcre2_config_newline_stub(value __unused v_unit) {
        return Val_int(pcre2_config_int(PCRE2_CONFIG_NEWLINE));
}

/* Returns number of bytes used for internal linkage of regular expressions */

CAMLprim intnat pcre2_config_link_size_stub(value __unused v_unit) {
        return pcre2_config_int(PCRE2_CONFIG_LINKSIZE);
}

CAMLprim value pcre2_config_link_size_stub_bc(value v_unit) {
        return Val_int(pcre2_config_link_size_stub(v_unit));
}

/* Returns default limit for calls to internal matching function */

CAMLprim intnat pcre2_config_match_limit_stub(value __unused v_unit) {
        return pcre2_config_long(PCRE2_CONFIG_MATCHLIMIT);
}

CAMLprim value pcre2_config_match_limit_stub_bc(value v_unit) {
        return Val_int(pcre2_config_match_limit_stub(v_unit));
}

/* Returns default limit for depth of nested backtracking  */

CAMLprim intnat pcre2_config_depth_limit_stub(value __unused v_unit) {
        return pcre2_config_long(PCRE2_CONFIG_DEPTHLIMIT);
}

CAMLprim value pcre2_config_depth_limit_stub_bc(value v_unit) {
        return Val_int(pcre2_config_depth_limit_stub(v_unit));
}

/* Returns boolean indicating use of stack recursion */
CAMLprim intnat pcre2_config_stackrecurse_stub(value __unused v_unit) {
        return Val_bool(pcre2_config_int(PCRE2_CONFIG_STACKRECURSE));
}
