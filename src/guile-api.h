/*
 * guile-api.h - compatibility and miscellaneous guile functionality
 *
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * $Id: guile-api.h,v 1.15 2003/04/06 20:01:24 ela Exp $
 *
 */

#ifndef __GUILE_API_H__
#define __GUILE_API_H__ 1

#include <libguile.h>
SCM guile_lookup (const char *name);
void guile_api_init (void);
void guile_api_finalize (void);


#if 0
/* Define this macro if Guile 1.7.x or better is in use. */
#if defined (SCM_MINOR_VERSION) && (SCM_MINOR_VERSION >= 7) && \
    defined (SCM_MAJOR_VERSION) && (SCM_MAJOR_VERSION >= 1)
#define SCM_VERSION_17X 1
#endif

/* Define this macro if Guile 1.5.x or better is in use. */
#if defined (SCM_MINOR_VERSION) && (SCM_MINOR_VERSION >= 5) && \
    defined (SCM_MAJOR_VERSION) && (SCM_MAJOR_VERSION >= 1)
#define SCM_VERSION_15X 1
#endif

#ifndef SCM_VERSION_15X
#define scm_t_bits long
#define scm_list_n scm_listify
typedef scm_catch_body_t scm_t_catch_body;
typedef scm_catch_handler_t scm_t_catch_handler;
#endif

/* Some definitions for backward compatibility with Guile 1.3.4 */
#ifndef SCM_ASSERT_TYPE
#define SCM_ASSERT_TYPE(_cond, _arg, _pos, _subr, _msg) \
    SCM_ASSERT (_cond, _arg, _pos, _subr)
#define scm_wrong_type_arg_msg(_subr, _pos, _bad, _msg) \
    scm_wrong_type_arg (_subr, _pos, _bad)
#define scm_out_of_range_pos(_subr, _bad, _pos) \
    scm_out_of_range (_subr, _bad)
#endif /* not SCM_ASSERT_TYPE */

/* Redefinition of the string and symbol predicates because they segfault
   for Guile 1.3.4 and prior version when passing immediate values. */
#ifdef SCM_STRINGP
#undef SCM_STRINGP
#endif
#define SCM_STRINGP(obj) SCM_NFALSEP (scm_string_p (obj))
#ifdef SCM_SYMBOLP
#undef SCM_SYMBOLP
#endif
#define SCM_SYMBOLP(obj) SCM_NFALSEP (scm_symbol_p (obj))

/* Compatibility definitions for various Guile versions.  These definitions
   are mainly due to the fact that the gh interface is deprecated in newer
   versions. */
#ifndef SCM_STRING_UCHARS
#define SCM_STRING_UCHARS(obj) ((unsigned char *) SCM_VELTS (obj))
#endif
#ifndef SCM_STRING_CHARS
#define SCM_STRING_CHARS(obj) ((char *) SCM_VELTS (obj))
#endif
#ifndef SCM_PROCEDUREP
#define SCM_PROCEDUREP(obj) SCM_NFALSEP (scm_procedure_p (obj))
#endif
#ifndef SCM_EXACTP
#define SCM_EXACTP(obj) SCM_NFALSEP (scm_exact_p (obj))
#endif
#ifndef SCM_POSITIVEP
#define SCM_POSITIVEP(obj) SCM_NFALSEP (scm_positive_p (obj))
#endif
#ifndef SCM_NEGATIVEP
#define SCM_NEGATIVEP(obj) SCM_NFALSEP (scm_negative_p (obj))
#endif
#ifndef SCM_PAIRP
#define SCM_PAIRP(obj) SCM_NFALSEP (scm_pair_p (obj))
#endif
#ifndef SCM_LISTP
#define SCM_LISTP(obj) SCM_NFALSEP (scm_list_p (obj))
#endif
#ifndef SCM_BOOLP
#define SCM_BOOLP(obj) SCM_NFALSEP (scm_boolean_p (obj))
#endif
#ifndef SCM_BOOL
#define SCM_BOOL(x) ((x) ? SCM_BOOL_T : SCM_BOOL_F)
#endif
#ifndef SCM_EQ_P
#define SCM_EQ_P(x, y) SCM_NFALSEP (scm_eq_p (x, y))
#endif
#ifndef SCM_CHARP
#define SCM_CHARP(obj) SCM_NFALSEP (scm_char_p (obj))
#endif
#ifndef SCM_CHAR
#define SCM_CHAR(x) SCM_ICHR (x)
#endif
#ifndef SCM_MAKE_CHAR
#define SCM_MAKE_CHAR(x) SCM_MAKICHR (x)
#endif
#ifndef SCM_NUM2INT
#define SCM_NUM2INT(pos, obj) gh_scm2int (obj)
#endif
#ifndef SCM_NUM2LONG
#define SCM_NUM2LONG(pos, obj) scm_num2long (obj, (char *) (pos), FUNC_NAME)
#endif
#ifndef SCM_NUM2ULONG
#define SCM_NUM2ULONG(pos, obj) scm_num2ulong (obj, (char *) (pos), FUNC_NAME)
#endif
#ifndef SCM_WRITABLE_VELTS
#define SCM_WRITABLE_VELTS(x) SCM_VELTS(x)
#endif
#ifndef SCM_VERSION_15X
#define scm_int2num(x) scm_long2num ((long) (x))
#endif
#ifndef SCM_VERSION_15X
#define scm_mem2string(str, len) gh_str2scm (str, len)
#endif
#ifndef SCM_VERSION_15X
#define scm_primitive_eval_x(expr) scm_eval_x (expr)
#endif
#ifndef SCM_VERSION_15X
#define scm_c_define(name, val) gh_define (name, val)
#endif
#ifndef scm_c_free
#define scm_c_free(p) scm_must_free (p)
#endif
#ifndef SCM_VERSION_15X
#define scm_c_define_gsubr(name, req, opt, rst, fcn) \
    gh_new_procedure (name, fcn, req, opt, rst)
#endif
#ifndef SCM_VERSION_15X
#define scm_c_primitive_load(file) \
    scm_primitive_load (scm_makfrom0str (file))
#endif
#ifndef SCM_VERSION_15X
#define guile_lookup(var, name) (var) = gh_lookup (name)
#else
#define guile_lookup(var, name) do {					    \
    (var) = scm_sym2var (scm_str2symbol (name),				    \
			 scm_current_module_lookup_closure (), SCM_BOOL_F); \
    if (SCM_FALSEP (var)) (var) = SCM_UNDEFINED;			    \
    else (var) = scm_variable_ref (var); } while (0)
#endif
#ifndef SCM_VERSION_15X
#define scm_gc_protect_object(obj) scm_protect_object (obj)
#endif
#ifndef SCM_VERSION_15X
#define scm_gc_unprotect_object(obj) scm_unprotect_object (obj)
#endif
#ifndef SCM_VERSION_15X
#define scm_c_make_vector(k, fill) scm_make_vector (scm_int2num (k), fill)
#endif
#ifndef SCM_VERSION_17X
#define scm_gc_malloc(len, name) scm_must_malloc (len, name)
#define scm_gc_free(mem, len, name) scm_must_free (mem)
#define scm_gc_realloc(mem, olen, nlen, name) \
    scm_must_realloc (mem, olen, nlen, name)
#endif
#ifndef SCM_VERSION_17X
#define scm_c_scm2chars(obj, data) gh_scm2chars (obj, data)
#endif
#ifndef SCM_VERSION_17X
#define scm_c_string2str(obj, str, lenp) gh_scm2newstr (obj, lenp)
#endif
#ifndef SCM_VERSION_17X
#define scm_c_symbol2str(obj, str, lenp) gh_symbol2newstr (obj, lenp)
#endif
#ifndef SCM_OUT_OF_RANGE
#define SCM_OUT_OF_RANGE(pos, arg) \
    scm_out_of_range_pos (FUNC_NAME, arg, SCM_MAKINUM (pos))
#endif

/* Return an integer. If the given Guile cell @var{obj} is not an 
   integer, the routine returns the default value @var{def}. */
#define guile_integer(pos, obj, def) \
    ((SCM_EXACTP (obj)) ? (SCM_NUM2INT (pos, obj)) : (def))

/* The GUILE_CONCAT macros create a new concatenated symbol for the 
   compiler in a portable way. It is essential to use these macros like
   GUILE_CONCAT (a,b) and *not* like GUILE_CONCAT (a, b) or its variants. */
#if defined (__STDC__) || defined (__cplusplus)
# define GUILE_CONCAT2(a, b) a##b
# define GUILE_CONCAT3(a, b, c) a##b##c
#else
# define GUILE_CONCAT2(a, b) a/* */b
# define GUILE_CONCAT3(a, b, c) a/* */b/* */c
#endif

/* Compatibility macros for Guile 1.3 version. Also defines the macro
   HAVE_OLD_SMOBS which indicates a different smob implementation. */
#ifndef SCM_NEWSMOB
#define SCM_NEWSMOB(value, tag, data) do {                         \
    SCM_NEWCELL (value);                                           \
    SCM_SETCDR (value, data); SCM_SETCAR (value, tag); } while (0)
#endif
#ifndef SCM_RETURN_NEWSMOB
#define SCM_RETURN_NEWSMOB(tag, data) do { \
    SCM value;                             \
    SCM_NEWSMOB (value, tag, data);        \
    return value; } while (0)
#endif
#ifndef SCM_SMOB_DATA
#define SCM_SMOB_DATA(data) SCM_CDR (data)
#define gh_scm2chars(obj, lenp) guile_to_string (obj)
#define HAVE_OLD_SMOBS 1
#endif
#ifndef SCM_FPORT_FDES
#define SCM_FPORT_FDES(port) fileno ((FILE *) SCM_STREAM (port))
#endif

#endif /* 0 */
#endif /* not __GUILE_API_H__ */
