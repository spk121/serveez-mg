/*
 * guile.h - interface to guile core library declarations
 *
 * Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __GUILE_H__
#define __GUILE_H__ 1

/*! \file guile.h
    \brief Low-level conversion functions between Scheme and C types.
*/

#include <libguile.h>
#include "libserveez.h"
/*
 * Converts @code{SCM} into @code{char *} no matter if it is string or
 * symbol.  Returns @code{NULL} if it was neither.  The new string must be
 * explicitly @code{free()}d.
 */
#define guile_to_string(cell)                                           \
  (scm_is_true (scm_null_p (cell)) ? NULL                               \
   : (scm_is_string (cell) ? scm_to_locale_string (cell)                \
      : (scm_is_symbol (cell) ? scm_to_locale_string (scm_symbol_to_string (cell)) \
         : NULL)))

/* FAIL breaks to the label `out' and sets an error condition.  */
#define FAIL() do { err = -1; goto out; } while(0)

/* Global error flag.  */
int guile_global_error;

/* Export these functions.  */

/*! \fn int guile_to_integer (SCM source, int *target);
 * \brief Converts guile type to integer.

 * Parse an integer value from a scheme value. Returns zero when
 * successful.  Stores the integer value where target points to. Does
 * not emit error messages.

 * \param source a scheme integer or string
 * \param target a pointer to an integer store
 * \return 0 on sucess.  Non-zero on failure.
 */
int guile_to_integer (SCM, int *);

int guile_to_boolean (SCM, int *);
svz_array_t *guile_to_intarray (SCM, char *);
svz_array_t *guile_to_strarray (SCM, char *);
svz_hash_t *guile_to_hash (SCM, char *);
svz_hash_t *guile_to_optionhash (SCM, char *, int);
SCM guile_strarray_to_guile (svz_array_t *);
SCM guile_intarray_to_guile (svz_array_t *);
SCM guile_hash_to_guile (svz_hash_t *);
void guile_error (char *, ...);
int guile_load_config (char *);
/* Guile API */
#define EXPORT __attribute__ ((visibility("default")))
EXPORT SCM guile_config_instantiate (SCM type, SCM name, SCM instance, SCM opts);
EXPORT SCM guile_define_server (SCM name, SCM args);
EXPORT SCM guile_bind_server (SCM port, SCM server);
EXPORT SCM guile_access_interfaces (SCM args);
EXPORT SCM guile_access_loadpath (SCM args);
EXPORT SCM guile_check_port (SCM arg);
EXPORT SCM guile_check_server (SCM arg);
EXPORT SCM guile_check_stype (SCM arg);
#undef EXPORT







int optionhash_validate (svz_hash_t *, int, char *, char *);
void optionhash_destroy (svz_hash_t *);
SCM optionhash_get (svz_hash_t *, char *);
int optionhash_extract_string (svz_hash_t *, char *, int, char *, char **,
                               char *);

#endif /* not __GUILE_H__ */
