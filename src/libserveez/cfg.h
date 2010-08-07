/*
 * cfg.h - configuration handling
 *
 * Copyright (C) 2002 Andreas Rottmann <a.rottmann@gmx.at>
 * Copyright (C) 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: cfg.h,v 1.2 2003/06/14 14:57:59 ela Exp $
 *
 */

#ifndef __CFG_H__
#define __CFG_H__ 1

#include "libserveez/array.h"
#include "libserveez/hash.h"
#include "libserveez/portcfg.h"

/*
 * Each server can have a an array of key-value-pairs specific for it.
 * Use macros at end of this file for setting up these.
 */
typedef struct svz_key_value_pair 
{
  int type;        /* data type (string, integer, etc.) */
  char *name;      /* variable name (symbol) */
  int defaultable; /* set if this item is defaultable */
  void *address;   /* memory address of the variable */
}
svz_key_value_pair_t;

/*
 * This structure defines the callbacks for the @code{svz_server_configure()}
 * function. Each of these have the following arguments:
 * instance: might be the name of the instance to configure
 * arg:     an optional argument (e.g. scheme cell), supplied by user
 * name:    the symbolic name of the configuration item
 * target:  target address of the configuration item
 * hasdef:  is there a default value
 * def:     the default value for this configuration item
 *
 * The 'before' and 'after' callbacks are called just before and after
 * other options are set. The user is supposed to emit error messages since
 * the library cannot guess what went wrong.
 * Both callbacks have to return @code{SVZ_ITEM_OK} to allow the configure
 * function to complete successfully. No other callback is invoked when
 * the 'before' callback fails.
 *
 * Default values and the @var{hasdef} flag are passed to callbacks for 
 * no sane reason. You do not need to care about them if you set 
 * appropriate return values. If you use them, however, everything that 
 * is a pointer needs to be copied.
 */

#define SVZ_ITEM_OK             0 /* okay, value set */
#define SVZ_ITEM_DEFAULT        1 /* use default, be silent if missing */
#define SVZ_ITEM_DEFAULT_ERRMSG 2 /* use default, croak if missing */
#define SVZ_ITEM_FAILED         3 /* error, error messages already emitted */
#define SVZ_ITEM_FAILED_ERRMSG  4 /* error, please report error */

typedef struct
{
  int (* before)   (char *instance, void *arg);
  int (* integer)  (char *instance, void *arg, char *name,
		    int *target, int hasdef, int def);
  int (* boolean)  (char *instance, void *arg, char *name,
		    int *target, int hasdef, int def);
  int (* intarray) (char *instance, void *arg, char *name,
		    svz_array_t **target, int hasdef, svz_array_t *def);
  int (* string)   (char *instance, void *arg, char *name, 
		    char **target, int hasdef, char *def);
  int (* strarray) (char *instance, void *arg, char *name,
		    svz_array_t **target, int hasdef, svz_array_t *def);
  int (* hash)     (char *instance, void *arg, char *name, 
		    svz_hash_t **target, int hasdef, svz_hash_t *def);
  int (* portcfg)  (char *instance, void *arg, char *name, 
		    svz_portcfg_t **target, int hasdef, svz_portcfg_t *def);
  int (* after)    (char *instance, void *arg);
}
svz_config_accessor_t;

typedef struct
{
  /* description of the configuration prototype */
  char *description;
  /* start of example structure */
  void *start;
  /* size of the above structure */
  int size;
  /* array of key-value-pairs of configuration items */
  svz_key_value_pair_t *items;
}
svz_config_prototype_t;

/* Constants for the @var{defaultable} argument. */
#define SVZ_ITEM_DEFAULTABLE     1
#define SVZ_ITEM_NOTDEFAULTABLE  0

/* Configuration item identifiers. */
#define SVZ_ITEM_END      0
#define SVZ_ITEM_INT      1
#define SVZ_ITEM_INTARRAY 2
#define SVZ_ITEM_STR      3
#define SVZ_ITEM_STRARRAY 4
#define SVZ_ITEM_HASH     5
#define SVZ_ITEM_PORTCFG  6
#define SVZ_ITEM_BOOL     7

/*
 * Macro for defining the example configuration @var{config} (with the
 * name @var{description} and its configuration items @var{prototypes}
 * within a server type definition. 
 */
#define SVZ_CONFIG_DEFINE(description, config, prototypes) \
  { description, &(config), sizeof (config), (prototypes) }

/* 
 * Returns a text representation of the given configuration item 
 * identifier @var{item}.
 */
#define SVZ_ITEM_TEXT(item)                                  \
  ((item) == SVZ_ITEM_INT) ? "integer" :                     \
  ((item) == SVZ_ITEM_INTARRAY) ? "integer array" :          \
  ((item) == SVZ_ITEM_STR) ? "string" :                      \
  ((item) == SVZ_ITEM_STRARRAY) ? "string array" :           \
  ((item) == SVZ_ITEM_HASH) ? "hash table" :                 \
  ((item) == SVZ_ITEM_BOOL) ? "boolean" :                    \
  ((item) == SVZ_ITEM_PORTCFG) ? "port configuration" : NULL

/*
 * Register a simple integer. C-type: @code{int}. The given @var{name} 
 * specifies the symbolic name of the integer and @var{item} the integer 
 * itself (not its address). The @var{defaultable} argument can be either 
 * @code{SVZ_ITEM_DEFAULTABLE} or @code{SVZ_ITEM_NOTDEFAULTABLE}.
 */
#define SVZ_REGISTER_INT(name, item, defaultable) \
  { SVZ_ITEM_INT, (name), (defaultable), &(item) }

/*
 * Register an array of integers. C-type: @code{svz_array_t *}.
 */
#define SVZ_REGISTER_INTARRAY(name, item, defaultable) \
  { SVZ_ITEM_INTARRAY, (name), (defaultable), &(item) }

/*
 * Register a boolean value. C-type: @code{int}.
 */
#define SVZ_REGISTER_BOOL(name, item, defaultable) \
  { SVZ_ITEM_BOOL, (name), (defaultable), &(item) }

/*
 * Register a simple character string. C-type: @code{char *}.
 */
#define SVZ_REGISTER_STR(name, item, defaultable) \
  { SVZ_ITEM_STR, (name), (defaultable), &(item) }

/*
 * Register a string array. C-type: @code{svz_array_t *}.
 */
#define SVZ_REGISTER_STRARRAY(name, item, defaultable) \
  { SVZ_ITEM_STRARRAY, (name), (defaultable), &(item) }

/*
 * Register a hash table associating strings with strings only. C-type:
 * @code{svz_hash_t *}.
 */
#define SVZ_REGISTER_HASH(name, item, defaultable) \
  { SVZ_ITEM_HASH, (name), (defaultable), &(item) }

/*
 * Register a port configuration. C-type: @code{svz_portcfg_t *}.
 */
#define SVZ_REGISTER_PORTCFG(name, item, defaultable) \
  { SVZ_ITEM_PORTCFG, (name), (defaultable), &(item) }

/*
 * This macro indicates the end of the list of configuration items. It is
 * the only mandatory item you need to specify in an example server type
 * configuration.
 */
#define SVZ_REGISTER_END() \
  { SVZ_ITEM_END, NULL, SVZ_ITEM_DEFAULTABLE, NULL }

typedef struct
{
  char *name;
  int (*instantiate) (char *, char *, void *, svz_config_accessor_t *,
                      char **);
}
svz_config_type_t;

__BEGIN_DECLS

SERVEEZ_API void *svz_config_instantiate __PARAMS ((svz_config_prototype_t *,
                                                    char *, void *,
                                                    svz_config_accessor_t *));
SERVEEZ_API void svz_config_free __PARAMS ((svz_config_prototype_t *, void *));
SERVEEZ_API void svz_config_type_add __PARAMS ((svz_config_type_t *));
SERVEEZ_API int svz_config_type_instantiate __PARAMS ((char *, char *,
						       char *, void *,
						       svz_config_accessor_t *,
						       char **));
SERVEEZ_API void svz_config_type_init __PARAMS ((void));
SERVEEZ_API void svz_config_type_finalize __PARAMS ((void));

#if SVZ_ENABLE_DEBUG
SERVEEZ_API void svz_config_prototype_print __PARAMS ((svz_config_prototype_t *));
#endif

SERVEEZ_API svz_array_t *svz_config_intarray_create __PARAMS ((int *));
SERVEEZ_API void svz_config_intarray_destroy __PARAMS ((svz_array_t *));
SERVEEZ_API svz_array_t *svz_config_intarray_dup __PARAMS ((svz_array_t *));
SERVEEZ_API svz_array_t *svz_config_strarray_create __PARAMS ((char **));
SERVEEZ_API void svz_config_strarray_destroy __PARAMS ((svz_array_t *));
SERVEEZ_API svz_array_t *svz_config_strarray_dup __PARAMS ((svz_array_t *));
SERVEEZ_API svz_hash_t *svz_config_hash_create __PARAMS ((char **));
SERVEEZ_API void svz_config_hash_destroy __PARAMS ((svz_hash_t *));
SERVEEZ_API svz_hash_t *svz_config_hash_dup __PARAMS ((svz_hash_t *));

__END_DECLS

#endif /* __CFG_H__ */
