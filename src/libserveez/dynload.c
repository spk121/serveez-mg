/*
 * dynload.c - dynamic server loading implementation
 *
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
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
 * $Id: dynload.c,v 1.24 2003/08/26 04:59:33 ela Exp $
 *
 */

#include <config.h>

#include <stdio.h>              /* sprintf */
#include <string.h>             /* strlen, strcmp */
#include <stdlib.h>             /* putenv */
#include <dlfcn.h>             /* RTLD_NOW, RTLD_GLOBAL, dlopen */

#include "svzpath.h"
#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/server.h"
#include "libserveez/core.h"
#include "libserveez/dynload.h"

/* Internal list of shared libraries. */
static int dyn_libraries = 0;
static dyn_library_t *dyn_library = NULL;

/* Define library prefix and suffix. */
# define DYNLOAD_PREFIX "lib"
# define DYNLOAD_SUFFIX "so"
# define DYNLOAD_PATH_SEPERATOR ':'
# define DYNLOAD_SYMBOL_PREFIX ""

/* Name of the additional search path environment variable. */
#define DYNLOAD_PATH "SERVEEZ_LOAD_PATH"

/*
 * Find a library handle for a given library's name @var{file} in the current
 * list of loaded shared libraries. Return @code{NULL} if there is no such 
 * thing.
 */
static dyn_library_t *
dyn_find_library (char *file)
{
  int n;

  /* go through all loaded libraries and check if there is such a */
  for (n = 0; n < dyn_libraries; n++)
    if (!strcmp (dyn_library[n].file, file))
      return &dyn_library[n];

  return NULL;
}

/*
 * This functions tries to link a library called @var{file} and returns
 * its handle or NULL if it failed. If the argument @var{path} is given
 * it prepends this to the file name of the library.
 */
static void *
dyn_get_library (char *path, char *file)
{
  void *handle = NULL;
  char *lib;

  lib = svz_file_path (path, file);


  handle = dlopen (lib, RTLD_NOW | RTLD_GLOBAL);

  svz_free (lib);
  return handle;
}

/*
 * Set the additional search paths for the serveez library. The given array of
 * strings gets @code{svz_free()}d.
 */
void
svz_dynload_path_set (svz_array_t *paths)
{
  char *str, *env;
  int n, len;

  /* Return here if necessary. */
  if (paths == NULL)
    return;

  /* Create environment variable. */
  env = svz_strdup (DYNLOAD_PATH "=");
  len = strlen (env) + 3;
  svz_array_foreach (paths, str, n)
    {
      len = strlen (env) + strlen (str) + 2;
      env = svz_realloc (env, len);
      strcat (env, str);
      env[len - 2] = DYNLOAD_PATH_SEPERATOR;
      env[len - 1] = '\0';
    }
  env[len - 2] = '\0';
  svz_array_destroy (paths);

  /* Set environment variable. */
  if (putenv (svz_pstrdup (env)) < 0)
    svz_log (LOG_ERROR, "putenv: %s\n", SYS_ERROR);
  svz_free (env);
}

/*
 * Create an array of strings containing each an additional search path.
 * The loadpath is hold in the environment variable @samp{SERVEEZ_LOAD_PATH}
 * which can be set from outside the library or modified using 
 * @code{svz_dynload_path_set()}. The returned array needs to be destroyed 
 * after usage.
 */
svz_array_t *
svz_dynload_path_get (void)
{
  svz_array_t *paths = svz_array_create (1, svz_free);
  char *path, *p, *start, *val;
  int len, n;

  /* Add some default paths. */
  svz_array_add (paths, svz_strdup ("."));
  svz_array_add (paths, svz_strdup (SVZ_LIBDIR));
  svz_array_add (paths, svz_strdup (SVZ_BINDIR));
  svz_array_add (paths, svz_strdup (SVZ_PKGDATADIR));

  /* Add environment variable paths. */
  if ((p = getenv (DYNLOAD_PATH)) != NULL)
    {
      while (*p)
	{
	  start = p;
	  while (*p && *p != DYNLOAD_PATH_SEPERATOR)
	    p++;
	  if (p > start)
	    {
	      len = p - start;
	      path = svz_malloc (len + 1);
	      memcpy (path, start, len);
	      start = path + len;
	      *start-- = 0;
	      while ((*start == '/' || *start == '\\') && start > path) 
		*start-- = 0;

	      /* Do not put duplicate paths here. */
	      svz_array_foreach (paths, val, n)
		if (!strcmp (val, path))
		  {
		    svz_free_and_zero (path);
		    break;
		  }
	      if (path)
		svz_array_add (paths, path);
	    }
	  if (*p)
	    p++;
	}
    }
  return paths;
}

/*
 * Open the given library @var{file} and put it into the currently load 
 * library list. Return a valid library handle entry on success.
 */
static dyn_library_t *
dyn_load_library (char *file)
{
  int n;
  void *handle = NULL;
  svz_array_t *paths;
  char *path;

  /* go through all loaded libraries and check if there is such a */
  for (n = 0; n < dyn_libraries; n++)
    if (!strcmp (dyn_library[n].file, file))
      {
	dyn_library[n].ref++;
	return &dyn_library[n];
      }

  /* try open the library */
  if ((handle = dyn_get_library (NULL, file)) == NULL)
    {
      paths = svz_dynload_path_get ();
      svz_array_foreach (paths, path, n)
	if ((handle = dyn_get_library (path, file)) != NULL)
	  break;
      svz_array_destroy (paths);
    }

  if (handle == NULL)
    {
      svz_log (LOG_ERROR, "link: unable to locate %s\n", file);
      return NULL;
    }

  /* check if this handle already exists */
  for (n = 0; n < dyn_libraries; n++)
    if (dyn_library[n].handle == handle)
      {
	dyn_library[n].ref++;
	return &dyn_library[n];
      }

  /* add the shared library to the handle list */
  n = dyn_libraries;
  dyn_libraries++;
  dyn_library = svz_realloc (dyn_library, sizeof (dyn_library_t) * 
			     dyn_libraries);
  dyn_library[n].file = svz_strdup (file);
  dyn_library[n].handle = handle;
  dyn_library[n].ref = 1;
  return &dyn_library[n];
}

/*
 * Unload a given library @var{lib} if possible. Return the reference 
 * counter or zero if the library has been unloaded. Return -1 if there is 
 * no such library at all or on other errors.
 */
static int
dyn_unload_library (dyn_library_t *lib)
{
  int n, err = 0;
  void *handle;

  /* check if this library really exists */
  for (n = 0; n < dyn_libraries; n++)
    if (&dyn_library[n] == lib)
      {
	/* return the remaining reference counter */
	if (--lib->ref > 0)
	  return lib->ref;

	/* unload the library */
	handle = lib->handle;
	err = dlclose (handle);
	if (err)
	  {
	    svz_log (LOG_ERROR, "unlink: %s (%s)\n", dlerror (), lib->file);
	    return -1;
	  }

	/* rearrange the library structure */
	svz_free (lib->file);
	if (--dyn_libraries > 0)
	  {
	    *lib = dyn_library[dyn_libraries];
	    svz_realloc (dyn_library, sizeof (dyn_library_t) * dyn_libraries);
	  }
	else
	  {
	    svz_free (dyn_library);
	    dyn_library = NULL;
	  }
	return 0;
      }
  return -1;
}

/*
 * Get a function or data symbol @var{symbol} from the given library 
 * @var{lib}. Return @code{NULL} on errors.
 */
static void *
dyn_load_symbol (dyn_library_t *lib, char *symbol)
{
  int n;
  void *address = NULL;

  /* check if there is such a library */
  for (n = 0; n < dyn_libraries; n++)
    if (&dyn_library[n] == lib)
      {
	address = dlsym (lib->handle, symbol);
	if (address == NULL)
	  {
	    svz_log (LOG_ERROR, "lookup: %s (%s)\n", dlerror (), symbol);
	  }
	return address;
      }
  return NULL;
}

/*
 * Finalize the shared library interface of the core library. Unload
 * all libraries no matter if referenced or not.
 */
void
svz_dynload_finalize (void)
{
  while (dyn_libraries)
    {
      dyn_library[0].ref = 1;
      dyn_unload_library (&dyn_library[0]);
    }
}

/*
 * Initialize the shared library interface of the core library.
 */
void
svz_dynload_init (void)
{
  if (dyn_libraries)
    svz_dynload_finalize ();
  dyn_libraries = 0;
  dyn_library = NULL;
}

/* 
 * Create a file name of a shared library for a given servers 
 * descriptive name @var{description}.
 */
static char *
dyn_create_file (char *description)
{
  char *file;

  file = svz_malloc (strlen (description) + 
		     strlen (DYNLOAD_PREFIX "." DYNLOAD_SUFFIX) + 1);
  sprintf (file, DYNLOAD_PREFIX "%s." DYNLOAD_SUFFIX, description);
  return file;
}

/*
 * Create a symbol name of server definition for a given servers
 * descriptive name @var{description}.
 */
static char *
dyn_create_symbol (char *description)
{
  char *symbol;

  symbol = svz_malloc (strlen (description) + 1 +
		       strlen (DYNLOAD_SYMBOL_PREFIX "_server_definition"));
  sprintf (symbol, DYNLOAD_SYMBOL_PREFIX "%s_server_definition", description);
  return symbol;
}

/*
 * Load an additional server definition from a shared library. The given
 * descriptive name @var{description} must be part of the library's name.
 */
svz_servertype_t *
svz_servertype_load (char *description)
{
  char *file, *def;
  dyn_library_t *lib;
  svz_servertype_t *server;

  /* load library */
  file = dyn_create_file (description);
  if ((lib = dyn_load_library (file)) == NULL)
    {
      svz_free (file);
      return NULL;
    }
  svz_free (file);

  /* obtain exported data */
  def = dyn_create_symbol (description);
  if ((server = dyn_load_symbol (lib, def)) == NULL)
    {
      dyn_unload_library (lib);
      svz_free (def);
      return NULL;
    }
  svz_free (def);

  return server;
}

/*
 * Unload a server definition from a shared library. The given
 * descriptive name @var{description} must be part of the library's name.
 * Return the remaining reference count or -1 on errors.
 */
int
svz_servertype_unload (char *description)
{
  dyn_library_t *lib;
  char *file;

  /* check if there is such a library loaded */
  file = dyn_create_file (description);
  if ((lib = dyn_find_library (file)) != NULL)
    {
      /* try unloading it */
      svz_free (file);
      return dyn_unload_library (lib);
    }
  svz_free (file);

  return -1;
}
