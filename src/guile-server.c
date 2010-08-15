/*
 * guile-server.c - guile server modules
 *
 * Copyright (C) 2001, 2002, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: guile-server.c,v 1.58 2008/04/25 19:11:46 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_GUILE_SERVER

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>             /* va_start, etc */
#if HAVE_FLOSS_H
# include <floss.h>
#endif
#if GUILE_SOURCE
# include <libguile/gh.h>
#else
# include <guile/gh.h>
#endif

#include "libserveez.h"
#include "guile-api.h"
#include "guile.h"
#include "guile-bin.h"
#include "guile-server.h"

/* The guile server type hash. */
static svz_hash_t *guile_server = NULL;

/* The guile socket hash. */
static svz_hash_t *guile_sock = NULL;

/* List of functions to configure for a server type. */
static char *guile_functions[] = {
  "global-init", "init", "detect-proto", "connect-socket", "finalize",
  "global-finalize", "info-client", "info-server", "notify", "reset",
  "handle-request", NULL };

/* If set to zero exception handling is disabled. */
static int guile_use_exceptions = 1;

#define MAKE_SOCK_CALLBACK(FUNC, CALLBACK, TYPE, ASSOC)                 \
  static SCM                                                            \
  FUNC (SCM sock, SCM proc)                                             \
  {                                                                     \
    svz_socket_t *xsock;                                                \
    scm_assert_smob_type (guile_svz_socket_tag, sock);                  \
    xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);                      \
    if (!SCM_UNBNDP (proc))                                             \
      {                                                                 \
        SCM_ASSERT (scm_is_true (scm_procedure_p (proc), proc,          \
                                 SCM_ARG2, FUNC_NAME);                  \
        xsock->TYPE = FUNC;                                             \
        return guile_sock_setfunction (xsock, ASSOC, proc);             \
      }                                                                 \
    return guile_sock_getfunction (xsock, ASSOC);                       \
  } 

/* Provides a socket callback setter/getter. */
#define DEFINE_SOCK_CALLBACK(assoc, func) \
  scm_c_define_gsubr (assoc, 1, 1, 0, func)

scm_t_bits guile_svz_socket_tag = 0;
scm_t_bits guile_svz_server_tag = 0;
scm_t_bits guile_svz_servertype_tag = 0;

static int
guile_svz_socket_print (SCM smob, SCM port, scm_print_state *state) 
{
  static char txt[256];
  sprintf (txt, "#<svz:sock %p>", (void *) SCM_SMOB_DATA (smob));
  scm_puts (txt, port);
  return 1;
}

static int
guile_svz_server_print (SCM smob, SCM port, scm_print_state *state) 
{
  static char txt[256];
  sprintf (txt, "#<svz:server %p>", (void *) SCM_SMOB_DATA (smob));
  scm_puts (txt, port);
  return 1;
}

static int
guile_svz_servertype_print (SCM smob, SCM port, scm_print_state *state) 
{
  static char txt[256];
  sprintf (txt, "#<svz:servertype %p>", (void *) SCM_SMOB_DATA (smob));
  scm_puts (txt, port);
  return 1;
}

/*
 * Extract a guile procedure from an option hash. Return zero on success.
 */
static int
optionhash_extract_proc (svz_hash_t *hash,
			 char *key,        /* the key to find       */
			 int hasdef,       /* if there is a default */
			 SCM defvar,       /* default               */
			 SCM *target,      /* where to put it       */
			 char *txt)        /* appended to error     */
{
  SCM proc, hvalue = optionhash_get (hash, key);
  int err = 0;
  char *str = NULL;

  /* Is there such a string in the option-hash ? */
  if (SCM_EQ_P (hvalue, SCM_UNSPECIFIED))
    {
      /* Nothing in hash, try to use default. */
      if (hasdef)
	*target = defvar;
      else
	{
	  guile_error ("No default procedure for `%s' %s", key, txt);
	  err = 1;
	}
      return err;
    }

  /* Is that guile procedure ? */
  if (scm_is_true (scm_procedure_p (hvalue))
    {
      *target = hvalue;
    }
  else if ((str = guile_to_string (hvalue)) != NULL)
    {
      guile_lookup (proc, str);
      if (!SCM_UNBNDP (proc) && scm_is_true (scm_procedure_p (proc))
	*target = proc;
      else
	{
	  guile_error ("No such procedure `%s' for `%s' %s", str, key, txt);
	  err = 1;
	}
      free (str);
    }
  else
    {
      guile_error ("Invalid procedure for `%s' %s", key, txt);
      err = 1;
    }
  return err;
}

/*
 * Add another server type @var{server} to the list of known server types.
 * Each server type is associated with a number of functions which is also
 * a hash associating function names with its guile procedures.
 */
static void
guile_servertype_add (svz_servertype_t *server, svz_hash_t *functions)
{
  svz_hash_put (functions, "server-type", server);
  svz_hash_put (guile_server, server->prefix, functions);
  svz_servertype_add (server);
}

/*
 * Lookup a functions name @var{func} in the list of known servertypes.
 * The returned guile procedure depends on the given server type 
 * @var{server}. If the lookup fails this routine return SCM_UNDEFINED.
 */
static SCM
guile_servertype_getfunction (svz_servertype_t *server, char *func)
{
  svz_hash_t *gserver;
  SCM proc;

  if (server == NULL || guile_server == NULL)
    return SCM_UNDEFINED;

  if ((gserver = svz_hash_get (guile_server, server->prefix)) == NULL)
    return SCM_UNDEFINED;

  if ((proc = (SCM) SVZ_PTR2NUM (svz_hash_get (gserver, func))) == 0)
    return SCM_UNDEFINED;
  
  return proc;
}

/*
 * Return the procedure @var{func} associated with the socket structure
 * @var{sock} or SCM_UNDEFINED if there is no such function yet.
 */
static SCM
guile_sock_getfunction (svz_socket_t *sock, char *func)
{
  svz_hash_t *gsock;
  SCM proc;

  if (sock == NULL || guile_sock == NULL)
    return SCM_UNDEFINED;

  if (svz_sock_find (sock->id, sock->version) != sock)
    return SCM_UNDEFINED;

  if ((gsock = svz_hash_get (guile_sock, svz_itoa (sock->id))) == NULL)
    return SCM_UNDEFINED;

  if ((proc = (SCM) SVZ_PTR2NUM (svz_hash_get (gsock, func))) == 0)
    return SCM_UNDEFINED;
  
  return proc;
}

/* This function is used as destruction callback for the socket and
 * servertype hashes used in the Guile servers. */
static void
guile_unprotect (SCM proc)
{
  if (!SCM_UNBNDP (proc))
    scm_gc_unprotect_object (proc);
}

/*
 * Associate the given guile procedure @var{proc} hereby named @var{func}
 * with the socket structure @var{sock}. The function returns the previously
 * set procedure if there is such a.
 */
static SCM
guile_sock_setfunction (svz_socket_t *sock, char *func, SCM proc)
{
  svz_hash_t *gsock;
  SCM oldproc;

  if (sock == NULL || func == NULL)
    return SCM_UNDEFINED;

  if ((gsock = svz_hash_get (guile_sock, svz_itoa (sock->id))) == NULL)
    {
      gsock = svz_hash_create (4, (svz_free_func_t) guile_unprotect);
      svz_hash_put (guile_sock, svz_itoa (sock->id), gsock);
    }

  /* Put guile procedure into socket hash and protect it. Removes old
     guile procedure and unprotects it. */
  scm_gc_protect_object (proc);
  oldproc = (SCM) SVZ_PTR2NUM (svz_hash_put (gsock, func, SVZ_NUM2PTR (proc)));
  if (oldproc == 0)
    return SCM_UNDEFINED;

  scm_gc_unprotect_object (oldproc);
  return oldproc;
}

/*
 * This procedure can be used to schedule Serveez for shutdown within Guile.
 * Serveez will shutdown all network connections and terminate after the next
 * event loop. You should use this instead of issuing @code{(quit)}.
 */
#define FUNC_NAME "serveez-nuke"
SCM
guile_nuke_happened (void)
{
  svz_nuke_happened = 1;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * Controls the use of exceptions handlers for the Guile procedure calls
 * of Guile server callbacks. If the optional argument @var{enable} set to 
 * @code{#t} exception handling is enabled and if set to @code{#f} 
 * exception handling is disabled. The procedure always returns the 
 * current value of this behaviour.
 */
#define FUNC_NAME "serveez-exceptions"
SCM
guile_access_exceptions (SCM enable)
{
  SCM value = SCM_BOOL (guile_use_exceptions);
  int n;

  if (!SCM_UNBNDP (enable))
    {
      if (guile_to_boolean (enable, &n))
	guile_error ("%s: Invalid boolean value", FUNC_NAME);
      else 
	guile_use_exceptions = n;
    }
  return value;
}
#undef FUNC_NAME

/*
 * The @code{guile_call()} function puts the procedure to call including
 * the arguments to it into a single scheme cell passed to this function
 * in @var{data}. The functions unpacks this cell and applies it to
 * @code{scm_apply()}.
 * By convention the @var{data} argument cell consists of three items chained
 * like this: @code{(procedure first-argument (remaining-argument-list))}
 */
static SCM
guile_call_body (SCM data)
{
  return scm_apply (SCM_CAR (data), 
		    SCM_CAR (SCM_CDR (data)), SCM_CDR (SCM_CDR (data)));
}

/*
 * This is the exception handler for calls by @code{guile_call()}. Prints
 * the procedure (passed in @var{data}), the name of the exception and the
 * error message if possible.
 */
static SCM
guile_call_handler (SCM data, SCM tag, SCM args)
{
  char *str = guile_to_string (tag);

  scm_puts ("exception in ", scm_current_error_port ());
  scm_display (data, scm_current_error_port ());
  scm_puts (" due to `", scm_current_error_port ());
  scm_puts (str, scm_current_error_port ());
  scm_puts ("'\n", scm_current_error_port ());
  scm_puts ("guile-error: ", scm_current_error_port ());
  free (str);

  /* on quit/exit */
  if (SCM_NULLP (args))
    {
      scm_display (tag, scm_current_error_port ());
      scm_puts ("\n", scm_current_error_port ());
      return SCM_BOOL_F;
    }

  if (!scm_is_false (SCM_CAR (args)))
    {
      scm_display (SCM_CAR (args), scm_current_error_port ());
      scm_puts (": ", scm_current_error_port ());
    }
  scm_display_error_message (SCM_CAR (SCM_CDR (args)), 
			     SCM_CAR (SCM_CDR (SCM_CDR (args))), 
			     scm_current_error_port ());
  return SCM_BOOL_F;
}

/*
 * The following function takes an arbitrary number of arguments (specified
 * in @var{args}) passed to @code{scm_apply()} calling the guile procedure 
 * @var{code}. The function catches exceptions occurring in the procedure 
 * @var{code}. On success (no exception) the routine returns the value 
 * returned by @code{scm_apply()} otherwise @code{SCM_BOOL_F}.
 */
static SCM
guile_call (SCM code, int args, ...)
{
  va_list list;
  SCM body_data, handler_data;
  SCM arg = SCM_EOL, arglist = SCM_EOL, ret;

  /* Setup arg and arglist correctly for use with scm_apply(). */
  va_start (list, args);
  if (args > 0)
    {
      arg = va_arg (list, SCM);
      while (--args)
	arglist = scm_cons (va_arg (list, SCM), arglist);
      arglist = scm_cons (scm_reverse (arglist), SCM_EOL);
    }
  va_end (list);

  /* Put both arguments and the procedure together into a single argument
     for the catch body. */
  body_data = scm_cons (code, scm_cons (arg, arglist));
  handler_data = code;

  /* Use exception handling if requested. */
  if (guile_use_exceptions)
    {
      ret = scm_internal_catch (SCM_BOOL_T,
				(scm_t_catch_body) guile_call_body, 
				(void *) body_data, 
				(scm_t_catch_handler) guile_call_handler, 
				(void *) handler_data);
    }
  else
    {
      ret = guile_call_body (body_data);
    }

  return ret;
}

/* Wrapper function for the global initialization of a server type. */
#define FUNC_NAME "guile_func_global_init"
static int
guile_func_global_init (svz_servertype_t *stype)
{
  SCM global_init = guile_servertype_getfunction (stype, "global-init");
  SCM ret;

  if (!SCM_UNBNDP (global_init))
    {
      SCM servertype_smob;
      SCM_NEWSMOB (servertype_smob, guile_svz_servertype_tag, stype);
      ret = guile_call (global_init, 1, servertype_smob);
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper function for the initialization of a server instance. */
#define FUNC_NAME "guile_func_init"
static int
guile_func_init (svz_server_t *server)
{
  svz_servertype_t *stype = svz_servertype_find (server);
  SCM init = guile_servertype_getfunction (stype, "init");
  SCM ret;

  if (!SCM_UNBNDP (init))
    {
      SCM server_smob;
      SCM_NEWSMOB (server_smob, guile_svz_server_tag, server);
      ret = guile_call (init, 1, server_smob);
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper routine for protocol detection of a server instance. */
#define FUNC_NAME "guile_func_detect_proto"
static int
guile_func_detect_proto (svz_server_t *server, svz_socket_t *sock)
{
  svz_servertype_t *stype = svz_servertype_find (server);
  SCM detect_proto = guile_servertype_getfunction (stype, "detect-proto");
  SCM ret;

  if (!SCM_UNBNDP (detect_proto))
    {
      SCM server_smob, socket_smob;
      SCM_NEWSMOB (server_smob, guile_svz_server_tag, server);
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (detect_proto, 2, server_smob, socket_smob); 
      return guile_integer (SCM_ARGn, ret, 0);
    }
  return 0;
}
#undef FUNC_NAME

/* Free the socket boundary if set by guile. */
static void
guile_sock_clear_boundary (svz_socket_t *sock)
{
  if (sock->boundary)
    {
      /* Free here since boundary is set by scm_to_locale_string or similar */
      free (sock->boundary);
      sock->boundary = NULL;
    }
  sock->boundary_size = 0;
}

/* Wrapper for the socket disconnected callback. Used here in order to
   delete the additional guile callbacks associated with the disconnected
   socket structure. */
#define FUNC_NAME "guile_func_disconnected_socket"
static int
guile_func_disconnected_socket (svz_socket_t *sock)
{
  SCM ret, disconnected = guile_sock_getfunction (sock, "disconnected");
  int retval = -1;
  svz_hash_t *gsock;

  /* First call the guile callback if necessary. */
  if (!SCM_UNBNDP (disconnected))
    {
      SCM socket_smob;
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (disconnected, 1, socket_smob);
      retval = guile_integer (SCM_ARGn, ret, -1);
    }

  /* Delete all the associated guile callbacks and unprotect these. */
  if ((gsock = svz_hash_delete (guile_sock, svz_itoa (sock->id))) != NULL)
    svz_hash_destroy (gsock);

  /* Release associated guile object is necessary. */
  if (sock->data != NULL)
    scm_gc_unprotect_object ((SCM) SVZ_PTR2NUM (sock->data));

  /* Free the socket boundary if set by guile. */
  guile_sock_clear_boundary (sock);

  return retval;
}
#undef FUNC_NAME

/* Wrapper for the kicked socket callback. */
#define FUNC_NAME "guile_func_kicked_socket"
static int
guile_func_kicked_socket (svz_socket_t *sock, int reason)
{
  SCM ret, kicked = guile_sock_getfunction (sock, "kicked");

  if (!SCM_UNBNDP (kicked))
    {
      SCM socket_smob;
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (kicked, 2, socket_smob, scm_from_int (reason));
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper function for the socket connection after successful detection. */
#define FUNC_NAME "guile_func_connect_socket"
static int
guile_func_connect_socket (svz_server_t *server, svz_socket_t *sock)
{
  svz_servertype_t *stype = svz_servertype_find (server);
  SCM connect_socket = guile_servertype_getfunction (stype, "connect-socket");
  SCM ret;

  /* Setup this function for later use. */
  sock->disconnected_socket = guile_func_disconnected_socket;

  if (!SCM_UNBNDP (connect_socket))
    {
      SCM server_smob, socket_smob;
      SCM_NEWSMOB (server_smob, guile_svz_server_tag, server);
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (connect_socket, 2, server_smob, socket_smob); 
      return guile_integer (SCM_ARGn, ret, 0);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper for the finalization of a server instance. */
#define FUNC_NAME "guile_func_finalize"
static int
guile_func_finalize (svz_server_t *server)
{
  svz_servertype_t *stype = svz_servertype_find (server);
  SCM ret, finalize = guile_servertype_getfunction (stype, "finalize");
  int retval = 0;
  svz_hash_t *state;

  if (!SCM_UNBNDP (finalize))
    {
      SCM server_smob;
      SCM_NEWSMOB (server_smob, guile_svz_server_tag, server);
      ret = guile_call (finalize, 1, server_smob);
      retval = guile_integer (SCM_ARGn, ret, -1);
    }

  /* Release associated guile server state objects is necessary. */
  if ((state = server->data) != NULL)
    {
      svz_hash_destroy (state);
      server->data = NULL;
    }

  return retval;
}
#undef FUNC_NAME

/* Wrapper routine for the global finalization of a server type. */
#define FUNC_NAME "guile_func_global_finalize"
static int
guile_func_global_finalize (svz_servertype_t *stype)
{
  SCM ret, global_finalize;
  global_finalize = guile_servertype_getfunction (stype, "global-finalize");

  if (!SCM_UNBNDP (global_finalize))
    {
      SCM servertype_smob;
      SCM_NEWSMOB (servertype_smob, guile_svz_servertype_tag, stype);
      ret = guile_call (global_finalize, 1, servertype_smob);
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return 0;
}
#undef FUNC_NAME

/* Min-Max definitions. */
#define GUILE_MIN(x, y) (((x) < (y)) ? (x) : (y))
#define GUILE_MAX(x, y) (((x) > (y)) ? (x) : (y))

/* Wrapper for the client info callback. */
#define FUNC_NAME "guile_func_info_client"
static char *
guile_func_info_client (svz_server_t *server, svz_socket_t *sock)
{
  svz_servertype_t *stype = svz_servertype_find (server);
  SCM info_client = guile_servertype_getfunction (stype, "info-client");
  SCM ret;
  static char text[1024];
  char *str;

  if (!SCM_UNBNDP (info_client))
    {
      SCM server_smob, socket_smob;
      SCM_NEWSMOB (server_smob, guile_svz_server_tag, server);
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (info_client, 2, server_smob, socket_smob);
      if ((str = guile_to_string (ret)) != NULL)
	{
	  memset (text, 0, sizeof (text));
	  memcpy (text, str, GUILE_MIN (strlen (str) + 1, sizeof (text) - 1));
          free (str);
	  return text;
	}
    }
  return NULL;
}
#undef FUNC_NAME

/* Wrapper for the server info callback. */
#define FUNC_NAME "guile_func_info_server"
static char *
guile_func_info_server (svz_server_t *server)
{
  svz_servertype_t *stype = svz_servertype_find (server);
  SCM info_server = guile_servertype_getfunction (stype, "info-server");
  SCM ret;
  static char text[1024];
  char *str;

  if (!SCM_UNBNDP (info_server))
    {
      SCM server_smob;
      SCM_NEWSMOB (server_smob, guile_svz_server_tag, server);
      ret = guile_call (info_server, 1, server_smob);
      if ((str = guile_to_string (ret)) != NULL)
	{
	  memset (text, 0, sizeof (text));
	  memcpy (text, str, GUILE_MIN (strlen (str) + 1, sizeof (text) - 1));
          free (str);
	  return text;
	}
    }
  return NULL;
}
#undef FUNC_NAME

/* Wrapper for the server notifier callback. */
#define FUNC_NAME "guile_func_notify"
static int
guile_func_notify (svz_server_t *server)
{
  svz_servertype_t *stype = svz_servertype_find (server);
  SCM ret, notify = guile_servertype_getfunction (stype, "notify");

  if (!SCM_UNBNDP (notify))
    {
      SCM server_smob;
      SCM_NEWSMOB (server_smob, guile_svz_server_tag, server);
      ret = guile_call (notify, 1, server_smob);
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return -1;
}
#undef FUNC_NAME

/* Wrapper for the server reset callback. */
#define FUNC_NAME "guile_func_reset"
static int
guile_func_reset (svz_server_t *server)
{
  svz_servertype_t *stype = svz_servertype_find (server);
  SCM ret, reset = guile_servertype_getfunction (stype, "reset");

  if (!SCM_UNBNDP (reset))
    {
      SCM server_smob;
      SCM_NEWSMOB (server_smob, guile_svz_server_tag, server);
      ret = guile_call (reset, 1, server_smob);
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return -1;
}
#undef FUNC_NAME

/* Wrapper for the socket check request callback. */
#define FUNC_NAME "guile_func_check_request"
static int
guile_func_check_request (svz_socket_t *sock)
{
  SCM ret, check_request;
  check_request = guile_sock_getfunction (sock, "check-request");

  if (!SCM_UNBNDP (check_request))
    {
      SCM socket_smob;
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (check_request, 1, socket_smob);
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return -1;
}
#undef FUNC_NAME

/* Wrapper for the socket handle request callback. The function searches for
   both the servertype specific and socket specific procedure. */
#define FUNC_NAME "guile_func_handle_request"
static int
guile_func_handle_request (svz_socket_t *sock, char *request, int len)
{
  svz_server_t *server;
  svz_servertype_t *stype;
  SCM ret, bin, handle_request;
  handle_request = guile_sock_getfunction (sock, "handle-request");

  if (SCM_UNBNDP (handle_request))
    {
      server = svz_server_find (sock->cfg);
      stype = svz_servertype_find (server);
      handle_request = guile_servertype_getfunction (stype, "handle-request");
    }

  if (!SCM_UNBNDP (handle_request))
    {
      SCM socket_smob;
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      bin = scm_c_take_bytevector (request, len);
      ret = guile_call (handle_request, 3, socket_smob, bin, scm_from_int (len));
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return -1;
}
#undef FUNC_NAME

/* Wrapper for the socket idle func callback. */
#define FUNC_NAME "guile_func_idle_func"
static int
guile_func_idle_func (svz_socket_t *sock)
{
  SCM ret, idle_func = guile_sock_getfunction (sock, "idle");

  if (!SCM_UNBNDP (idle_func))
    {
      SCM socket_smob;
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (idle_func, 1, socket_smob);
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper for the socket trigger condition func callback. */
#define FUNC_NAME "guile_func_trigger_cond"
static int
guile_func_trigger_cond (svz_socket_t *sock)
{
  SCM ret, trigger_cond = guile_sock_getfunction (sock, "trigger-condition");

  if (!SCM_UNBNDP (trigger_cond))
    {
      SCM socket_smob;
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (trigger_cond, 1, socket_smob);
      return scm_is_true (ret);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper for the socket trigger func callback. */
#define FUNC_NAME "guile_func_trigger_func"
static int
guile_func_trigger_func (svz_socket_t *sock)
{
  SCM ret, trigger_func = guile_sock_getfunction (sock, "trigger");

  if (!SCM_UNBNDP (trigger_func))
    {
      SCM socket_smob;
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (trigger_func, 1, socket_smob);
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return 0;
}
#undef FUNC_NAME

/* Wrapper for the socket check oob request callback. */
#define FUNC_NAME "guile_func_check_request_oob"
static int
guile_func_check_request_oob (svz_socket_t *sock)
{
  SCM ret, check_request_oob;
  check_request_oob = guile_sock_getfunction (sock, "check-oob-request");

  if (!SCM_UNBNDP (check_request_oob))
    {
      SCM socket_smob;
      SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
      ret = guile_call (check_request_oob, 2, socket_smob, 
                        scm_from_int (sock->oob));
      return guile_integer (SCM_ARGn, ret, -1);
    }
  return -1;
}
#undef FUNC_NAME

/* Set the @code{handle-request} member of the socket structure @var{sock} 
   to the Guile procedure @var{proc}. The procedure returns the previously
   set handler if there is any. */
#define FUNC_NAME "svz:sock:handle-request"
MAKE_SOCK_CALLBACK (guile_sock_handle_request, 
                    guile_func_handle_request,
                    handle_request, "handle-request")
#undef FUNC_NAME

/* Set the @code{check-request} member of the socket structure @var{sock} 
   to the Guile procedure @var{proc}. Returns the previously handler if 
   there is any. */
#define FUNC_NAME "svz:sock:check-request"
MAKE_SOCK_CALLBACK (guile_sock_check_request,
                    guile_func_handle_request,
                    check_request, "check-request")
#undef FUNC_NAME

/* Setup the packet boundary of the socket @var{sock}. The given string value
   @var{boundary} can contain any kind of data. If you pass an exact number 
   value the socket is setup to parse fixed sized packets. In fact this 
   procedure sets the @code{check-request} callback of the given socket 
   structure @var{sock} to a predefined routine which runs the 
   @code{handle-request} callback of the same socket when it detected a 
   complete packet specified by @var{boundary}. For instance you 
   can setup Serveez to pass your @code{handle-request} procedure text lines 
   by calling @code{(svz:sock:boundary sock "\\n")}. */
#define FUNC_NAME "svz:sock:boundary"
static SCM
guile_sock_boundary (SCM sock, SCM boundary)
{
  svz_socket_t *xsock;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  SCM_ASSERT (scm_is_integer (boundary) || scm_is_string (boundary), boundary,
              SCM_ARG2, FUNC_NAME);

  /* Release previously set boundaries. */
  guile_sock_clear_boundary (xsock);

  /* Setup for fixed sized packets. */
  if (scm_is_integer (boundary))
    {
      xsock->boundary = NULL;
      xsock->boundary_size = scm_to_int (boundary);
    }
  /* Handle packet delimiters. */
  else
    {
      xsock->boundary = scm_c_scm2chars (boundary, NULL);
      xsock->boundary_size = scm_c_string_length (boundary);
    }

  /* Only assign this callback for connection oriented protocols. */
  if (xsock->proto & (PROTO_TCP | PROTO_PIPE))
    xsock->check_request = svz_sock_check_request;

  return SCM_BOOL_T;
}
#undef FUNC_NAME

/* Set or unset the flood protection bit of the given socket @var{sock}.
   Returns the previous value of this bit (#t or #f). The @var{flag}
   argument must be either boolean or an exact number and is optional. */
#define FUNC_NAME "svz:sock:floodprotect"
static SCM
guile_sock_floodprotect (SCM sock, SCM flag)
{
  svz_socket_t *xsock;
  int flags;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  flags = xsock->flags;
  if (!SCM_UNBNDP (flag))
    {
      SCM_ASSERT (scm_is_bool (flag) || scm_is_integer (flag), flag, SCM_ARG2,
                  FUNC_NAME);
      if ((scm_is_bool (flag) && scm_is_true (flag) != 0) ||
	  (scm_is_integer (flag) && scm_to_int (flag) != 0))
	xsock->flags &= ~SOCK_FLAG_NOFLOOD;
      else
	xsock->flags |= SOCK_FLAG_NOFLOOD;
    }
  return (flags & SOCK_FLAG_NOFLOOD) ? SCM_BOOL_F : SCM_BOOL_T;
}
#undef FUNC_NAME

/* Write the string buffer @var{buffer} to the socket @var{sock}. The
   procedure accepts binary smobs too. Return @code{#t} on success and
   @code{#f} on failure. */
#define FUNC_NAME "svz:sock:print"
static SCM
guile_sock_print (SCM sock, SCM buffer)
{
  svz_socket_t *xsock;
  char *buf;
  int len, ret = -1;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  SCM_ASSERT (scm_is_string (buffer) || scm_is_true (scm_bytevector_p (buffer)),
              buffer, SCM_ARG2, FUNC_NAME);

  if (scm_is_string (buffer))
    {
      buf = SCM_STRING_CHARS (buffer);
      len = scm_c_string_length (buffer);
    }
  else
    {
      buf = SCM_BYTEVECTOR_CONTENTS (buffer);
      len = scm_c_bytevector_length (buffer);
    }

  /* Depending on the protocol type use different kind of senders. */
  if (xsock->proto & (PROTO_TCP | PROTO_PIPE))
    ret = svz_sock_write (xsock, buf, len);
  else if (xsock->proto & PROTO_UDP)
    ret = svz_udp_write (xsock, buf, len);
  else if (xsock->proto & PROTO_ICMP)
    ret = svz_icmp_write (xsock, buf, len);

  if (ret == -1)
    {
      svz_sock_schedule_for_shutdown (xsock);
      return SCM_BOOL_F;
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/* Associate any kind of data (any Guile object) given in the argument
   @var{data} with the socket @var{sock}. The @var{data} argument is
   optional. The procedure always returns a previously stored value or an 
   empty list. */
#define FUNC_NAME "svz:sock:data"
SCM
guile_sock_data (SCM sock, SCM data)
{
  svz_socket_t *xsock;
  SCM ret = SCM_EOL;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
 
  /* Save return value here. */
  if (xsock->data != NULL)
    ret = (SCM) SVZ_PTR2NUM (xsock->data);

  /* Replace associated guile cell and unprotect previously stored cell
     if necessary. */
  if (!SCM_UNBNDP (data))
    {
      if (xsock->data != NULL)
	scm_gc_unprotect_object (ret);
      xsock->data = SVZ_NUM2PTR (data);
      scm_gc_protect_object (data);
    }
  return ret;
}
#undef FUNC_NAME

/*
 * Convert the given value at @var{address} of the the type @var{type} into
 * a scheme cell.
 */
static SCM
guile_config_convert (void *address, int type)
{
  svz_portcfg_t *port;
  SCM ret = SCM_EOL;

  switch (type)
    {
    case SVZ_ITEM_INT:
      ret = scm_from_int (*(int *) address);
      break;
    case SVZ_ITEM_INTARRAY:
      ret = guile_intarray_to_guile (*(svz_array_t **) address);
      break;
    case SVZ_ITEM_STR:
      ret = scm_from_locale_string (*(char **) address);
      break;
    case SVZ_ITEM_STRARRAY:
      ret = guile_strarray_to_guile (*(svz_array_t **) address);
      break;
    case SVZ_ITEM_HASH:
      ret = guile_hash_to_guile (*(svz_hash_t **) address);
      break;
    case SVZ_ITEM_PORTCFG:
      if ((port = *(svz_portcfg_t **) address) != NULL)
        ret = scm_from_locale_string (port->name);
      break;
    case SVZ_ITEM_BOOL:
      ret = SCM_BOOL (*(int *) address);
      break;
    }
  return ret;
}

/* Checks if the given Guile object @var{smob} in position @var{arg} is a 
   server or socket and throws an exception if not. Otherwise it saves the
   server in the variable @var{var}. */
static void
check_server_smob_arg (SCM smob, int arg, svz_server_t **var)
{
  void *cfg;
  SCM_ASSERT (SCM_SMOB_PREDICATE (guile_svz_server_tag, smob)
              || SCM_SMOB_PREDICATE (guile_svz_socket_tag, smob),
              smob, arg, NULL);
  if (SCM_SMOB_PREDICATE (guile_svz_server_tag, smob))
    *var = (svz_server_t *) SCM_SMOB_DATA (smob);
  else
    {
      cfg = ((svz_socket_t *) SCM_SMOB_DATA (smob))->cfg;
      *var = svz_server_find (cfg);
    }
}

/* This procedure returns the configuration item specified by @var{key} of
   the given server instance @var{server}. You can pass this function a
   socket too. In this case the procedure will lookup the appropriate server
   instance itself. If the given string @var{key} is invalid (not defined 
   in the configuration alist in @code{(define-servertype!)}) then it returns
   an empty list. */
#define FUNC_NAME "svz:server:config-ref"
SCM
guile_server_config_ref (SCM server, SCM key)
{
  SCM ret = SCM_EOL;
  svz_servertype_t *stype;
  svz_server_t *xserver;
  int i;
  void *cfg, *address;
  char *str;
  svz_config_prototype_t *prototype;
  
  check_server_smob_arg (server, SCM_ARG1, &xserver);
  SCM_ASSERT (scm_is_string (key), key, SCM_ARG2, FUNC_NAME);

  str = guile_to_string (key);
  stype = svz_servertype_find (xserver);
  cfg = xserver->cfg;
  prototype = &stype->config_prototype;

  for (i = 0; prototype->items[i].type != SVZ_ITEM_END; i++)
    {
      if (strcmp (prototype->items[i].name, str) == 0)
	{
	  address = (void *) ((unsigned long) cfg + 
			      (unsigned long) prototype->items[i].address - 
			      (unsigned long) prototype->start);
	  ret = guile_config_convert (address, prototype->items[i].type);
	  break;
	}
    }
  free (str);
  return ret;
}
#undef FUNC_NAME

/* Returns the Guile object associated with the string value @var{key} which
   needs to be set via @code{(svz:server:state-set!)} previously. Otherwise
   the return value is an empty list. The given @var{server} argument must be
   either a valid @code{#<svz-server>} object or a @code{#<svz-socket>}. */
#define FUNC_NAME "svz:server:state-ref"
SCM
guile_server_state_ref (SCM server, SCM key)
{
  SCM ret = SCM_EOL;
  svz_server_t *xserver;
  char *str;
  svz_hash_t *hash;

  check_server_smob_arg (server, SCM_ARG1, &xserver);
  SCM_ASSERT (scm_is_string (key), key, SCM_ARG2, FUNC_NAME);
  str = guile_to_string (key);

  if ((hash = xserver->data) != NULL)
    if (svz_hash_exists (hash, str))
      ret = (SCM) SVZ_PTR2NUM (svz_hash_get (hash, str));
  free (str);
  return ret;
}
#undef FUNC_NAME

/* Associates the Guile object @var{value} with the string @var{key}. The
   given @var{server} argument can be both, a @code{#<svz-server>} or a 
   @code{#<svz-socket>}. Returns the previously associated object or an
   empty list if there was no such association. This procedure is useful 
   for server instance state savings. */
#define FUNC_NAME "svz:server:state-set!"
SCM
guile_server_state_set_x (SCM server, SCM key, SCM value)
{
  SCM ret = SCM_EOL;
  svz_server_t *xserver;
  char *str;
  svz_hash_t *hash;

  check_server_smob_arg (server, SCM_ARG1, &xserver);
  SCM_ASSERT (scm_is_string (key), key, SCM_ARG2, FUNC_NAME);
  str = guile_to_string (key);

  if ((hash = xserver->data) == NULL)
    {
      hash = svz_hash_create (4, (svz_free_func_t) guile_unprotect);
      xserver->data = hash;
    }
  if (svz_hash_exists (hash, str))
    {
      ret = (SCM) SVZ_PTR2NUM (svz_hash_put (hash, str, SVZ_NUM2PTR (value)));
      scm_gc_unprotect_object (ret);
    }
  else
    svz_hash_put (hash, str, SVZ_NUM2PTR (value));
  scm_gc_protect_object (value);
  free (str);
  return ret;
}
#undef FUNC_NAME

/* Converts the @var{server} instance's state into a Guile hash. 
   Returns an empty list if there is no such state yet. */
#define FUNC_NAME "svz:server:state->hash"
SCM
guile_server_state_to_hash (SCM server)
{
  SCM hash = SCM_EOL;
  svz_hash_t *data;
  svz_server_t *xserver;
  int i;
  char **key;

  check_server_smob_arg (server, SCM_ARG1, &xserver);
  if ((data = xserver->data) != NULL)
    {
      hash = scm_c_make_vector (svz_hash_size (data), SCM_EOL);
      svz_hash_foreach_key (data, key, i)
        scm_hash_set_x (hash, scm_from_locale_string (key[i]),
			(SCM) SVZ_PTR2NUM (svz_hash_get (data, key[i])));
    }
  return hash;
}
#undef FUNC_NAME

/*
 * Returns the length of a configuration item type, updates the configuration
 * item structure @var{item} and increases the @var{size} value if the
 * text representation @var{str} fits one of the item types understood by
 * Serveez. Returns zero if there is no such type.
 */
static int
guile_servertype_config_type (char *str, svz_key_value_pair_t *item, int *size)
{
  int n;
  struct {
    char *key;
    int size;
    int type;
  }
  config_types[] = {
    { "integer", sizeof (int), SVZ_ITEM_INT },
    { "intarray", sizeof (svz_array_t *), SVZ_ITEM_INTARRAY },
    { "string", sizeof (char *), SVZ_ITEM_STR },
    { "strarray", sizeof (svz_array_t *), SVZ_ITEM_STRARRAY },
    { "hash", sizeof (svz_hash_t *), SVZ_ITEM_HASH },
    { "portcfg", sizeof (svz_portcfg_t *), SVZ_ITEM_PORTCFG },
    { "boolean", sizeof (int), SVZ_ITEM_BOOL },
    { NULL, 0, -1 }
  };

  for (n = 0; config_types[n].key != NULL; n++)
    {
      if (strcmp (str, config_types[n].key) == 0)
	{
	  item->type = config_types[n].type;
	  *size += config_types[n].size;
	  return config_types[n].size;
	}
    }
  return 0;
}

/*
 * Release the default configuration items of a servertype defined
 * in Guile. This is necessary because each of these items is dynamically
 * allocated if defined in Guile.
 */
static void
guile_servertype_config_free (svz_servertype_t *server)
{
  int n;
  svz_config_prototype_t *prototype = &server->config_prototype;
  
  for (n = 0; prototype->items[n].type != SVZ_ITEM_END; n++)
    svz_free (prototype->items[n].name);
  svz_config_free (prototype, prototype->start);
  svz_free (prototype->items);
}

#if SVZ_ENABLE_DEBUG
/*
 * Debug helper: Display a text representation of the configuration items
 * of a guile servertype.
 */
static void
guile_servertype_config_print (svz_servertype_t *server)
{
  int n, i;
  svz_array_t *array;
  svz_hash_t *hash;
  svz_portcfg_t *port;
  char **key;
  svz_config_prototype_t *prototype = &server->config_prototype;
  
  fprintf (stderr, "Configuration of `%s':\n", server->prefix);
  for (n = 0; prototype->items[n].type != SVZ_ITEM_END; n++)
    {
      fprintf (stderr, " * %s `%s' is %sdefaultable\n", 
	       SVZ_ITEM_TEXT (prototype->items[n].type),
	       prototype->items[n].name, prototype->items[n].defaultable ?
	       "" : "not ");
      if (prototype->items[n].defaultable)
	{
	  fprintf (stderr, "   Default value: ");
	  switch (prototype->items[n].type)
	    {
	    case SVZ_ITEM_INT:
	      fprintf (stderr, "%d", *(int *) prototype->items[n].address);
	      break; 
	    case SVZ_ITEM_INTARRAY:
	      array = *(svz_array_t **) prototype->items[n].address;
	      fprintf (stderr, "( ");
	      for (i = 0; i < (int) svz_array_size (array); i++)
		fprintf (stderr, "%ld ", (long) svz_array_get (array, i));
	      fprintf (stderr, ")");
	      break;
	    case SVZ_ITEM_STR:
	      fprintf (stderr, "%s", *(char **) prototype->items[n].address);
	      break;
	    case SVZ_ITEM_STRARRAY:
	      array = *(svz_array_t **) prototype->items[n].address;
	      fprintf (stderr, "( ");
	      for (i = 0; i < (int) svz_array_size (array); i++)
		fprintf (stderr, "`%s' ", (char *) svz_array_get (array, i));
	      fprintf (stderr, ")");
	      break;
	    case SVZ_ITEM_HASH:
	      hash = *(svz_hash_t **) prototype->items[n].address;
	      fprintf (stderr, "( ");
	      svz_hash_foreach_key (hash, key, i)
		fprintf (stderr, "(%s => %s) ", key[i], 
			 (char *) svz_hash_get (hash, key[i]));
	      fprintf (stderr, ")");
	      break;
	    case SVZ_ITEM_PORTCFG:
	      port = *(svz_portcfg_t **) prototype->items[n].address;
	      svz_portcfg_print (port, stderr);
	      break;
	    case SVZ_ITEM_BOOL:
	      fprintf (stderr, "%d", *(int *) prototype->items[n].address);
	      break;
	  }
	  fprintf (stderr, " at %p\n", prototype->items[n].address);
	}
    }
}
#endif /* SVZ_ENABLE_DEBUG */

/*
 * Obtain a default value from the scheme cell @var{value}. The configuration
 * item type is specified by @var{type}. The default value is stored then at
 * @var{address}. Returns zero on success.
 */
static int
guile_servertype_config_default (svz_servertype_t *server, SCM value, 
				 void *address, int len, int type, char *key)
{
  int err = 0, n;
  char *str, *txt;
  svz_array_t *array;
  svz_hash_t *hash;
  svz_portcfg_t *port, *dup;

  switch (type)
    {
      /* Integer. */
    case SVZ_ITEM_INT:
      if (guile_to_integer (value, &n) != 0)
	{
	  guile_error ("%s: Invalid integer value for `%s'", 
		       server->prefix, key);
	  err = -1;
	}
      else
	memcpy (address, &n, len);
      break;

      /* Array of integers. */
    case SVZ_ITEM_INTARRAY:
      if ((array = guile_to_intarray (value, key)) == NULL)
	err = -1;
      else
	memcpy (address, &array, len);
      break;

      /* Character string. */
    case SVZ_ITEM_STR:
      if ((str = guile_to_string (value)) == NULL)
	{
	  guile_error ("%s: Invalid string value for `%s'", 
		       server->prefix, key);
	  err = -1;
	}
      else
	{
	  txt = svz_strdup (str);
	  memcpy (address, &txt, len);
	  free (str);
	}
      break;

      /* Array of character strings. */
    case SVZ_ITEM_STRARRAY:
      if ((array = guile_to_strarray (value, key)) == NULL)
	err = -1;
      else
	memcpy (address, &array, len);
      break;

      /* Hash. */
    case SVZ_ITEM_HASH:
      if ((hash = guile_to_hash (value, key)) == NULL)
	err = -1;
      else
	memcpy (address, &hash, len);
      break;

      /* Port configuration. */
    case SVZ_ITEM_PORTCFG:
      if ((str = guile_to_string (value)) == NULL)
	{
	  guile_error ("%s: Invalid string value for `%s'",
		       server->prefix, key);
	  err = -1;
	}
      else if ((port = svz_portcfg_get (str)) == NULL)
	{
	  guile_error ("%s: No such port configuration: `%s'", 
		       server->prefix, str);
	  free (str);
	  err = -1;
	}
      else
	{
	  free (str);
	  dup = svz_portcfg_dup (port);
	  memcpy (address, &dup, len);
	}
      break;

      /* Boolean value. */
    case SVZ_ITEM_BOOL:
      if (guile_to_boolean (value, &n) != 0)
	{
	  guile_error ("%s: Invalid boolean value for `%s'", 
		       server->prefix, key);
	  err = -1;
	}
      else
	memcpy (address, &n, sizeof (int));
      break;

      /* Invalid type. */
    default:
      err = -1;
    }
  return err;
}

/*
 * Parse the configuration of the server type @var{server} stored in the
 * scheme cell @var{cfg}.
 */
#define FUNC_NAME "guile_servertype_config"
static int
guile_servertype_config (svz_servertype_t *server, SCM cfg)
{
  int def, n, err = 0;
  char *txt = NULL, **key;
  svz_hash_t *options = NULL;
  svz_key_value_pair_t item;
  svz_key_value_pair_t *items = NULL;
  char *prototype = NULL;
  int size = 0, len;

  asprintf (&txt, "parsing configuration of `%s'", server->prefix);

  /* Check if the configuration alist is given or not. */
  if (SCM_EQ_P (cfg, SCM_UNSPECIFIED))
    {
      guile_error ("Missing servertype `configuration' for `%s'", 
		   server->prefix);
      FAIL ();
    }

  /* Try parsing this alist is valid. */
  if (NULL == (options = guile_to_optionhash (cfg, txt, 0)))
    FAIL (); /* Message already emitted. */

  /* Check the servertype configuration definition for duplicates. */
  err |= optionhash_validate (options, 1, "configuration", server->prefix);
  
  /* Now check all configuration items. */
  svz_hash_foreach_key (options, key, n)
    {
      SCM list = optionhash_get (options, key[n]);
      SCM value;
      char *str;

      /* Each configuration item must be a scheme list with three elements. */
      if (!SCM_LISTP (list) || scm_to_ulong (scm_length (list)) != 3)
	{
	  guile_error ("Invalid definition for `%s' %s", key[n], txt);
	  err = -1;
	  continue;
	}
      
      /* Assign address offset. */
      item.address = SVZ_NUM2PTR (size);

      /* First appears the type of item. */
      value = SCM_CAR (list);
      if ((str = guile_to_string (value)) == NULL)
	{
	  guile_error ("Invalid type definition for `%s' %s", key[n], txt);
	  err = -1;
	  continue;
	}
      else
	{
	  len = guile_servertype_config_type (str, &item, &size);
	  free (str);
	  if (len == 0)
	    {
	      guile_error ("Invalid type for `%s' %s", key[n], txt);
	      err = -1;
	      continue;
	    }
	}

      /* Then appears a boolean value specifying if the configuration 
	 item is defaultable or not. */
      list = SCM_CDR (list);
      value = SCM_CAR (list);
      if (guile_to_boolean (value, &def) != 0)
	{
	  guile_error ("Invalid defaultable value for `%s' %s", key[n], txt);
	  err = -1;
	  continue;
	}
      else if (def)
	item.defaultable = SVZ_ITEM_DEFAULTABLE;
      else
	item.defaultable = SVZ_ITEM_NOTDEFAULTABLE;

      /* Finally the default value itself. */
      list = SCM_CDR (list);
      value = SCM_CAR (list);
      prototype = svz_realloc (prototype, size);
      memset (prototype + size - len, 0, len);
      if (item.defaultable == SVZ_ITEM_DEFAULTABLE)
	{
	  err |= guile_servertype_config_default (server, value, 
						  prototype + size - len,
						  len, item.type, key[n]);
	}

      /* Increase the number of configuration items. */
      item.name = svz_strdup (key[n]);
      items = svz_realloc (items, sizeof (svz_key_value_pair_t) * (n + 1));
      memcpy (&items[n], &item, sizeof (svz_key_value_pair_t));
    }

  /* Append the last configuration item identifying the end of the
     configuration item list. */
  n = svz_hash_size (options);
  items = svz_realloc (items, sizeof (svz_key_value_pair_t) * (n + 1));
  item.type = SVZ_ITEM_END;
  item.address = NULL;
  item.defaultable = 0;
  item.name = NULL;
  memcpy (&items[n], &item, sizeof (svz_key_value_pair_t));

  /* Adjust the address values of the configuration items and assign
     all gathered information to the given servertype. */
  for (n = 0; n < svz_hash_size (options); n++)
    items[n].address = (void *) ((unsigned long) items[n].address + 
      (unsigned long) prototype);
  server->config_prototype.start = prototype;
  server->config_prototype.size = size;
  server->config_prototype.items = items;

#if 0
  guile_servertype_config_print (server);
#endif

 out:
  optionhash_destroy (options);
  svz_free (txt);
  return err;
}
#undef FUNC_NAME

/*
 * Guile server definition: This procedure takes one argument containing
 * the information about a new server type. If everything works fine you
 * have a freshly registered server type afterwards. Return @code{#t} on 
 * success.
 */
#define FUNC_NAME "define-servertype!"
SCM
guile_define_servertype (SCM args)
{
  int n, err = 0;
  char *txt = NULL;
  svz_hash_t *options;
  SCM proc;
  svz_servertype_t *server;
  svz_hash_t *functions;

  server = svz_calloc (sizeof (svz_servertype_t));
  asprintf (&txt, "defining servertype");

  if (NULL == (options = guile_to_optionhash (args, txt, 0)))
    FAIL (); /* Message already emitted. */

  /* Obtain the servertype prefix variable (Mandatory). */
  if (optionhash_extract_string (options, "prefix", 0, NULL, 
				 &server->prefix, txt) != 0)
    FAIL ();
  asprintf (&txt, "defining servertype `%s'", server->prefix);

  /* Check the servertype definition once. */
  err |= optionhash_validate (options, 1, "servertype", server->prefix);
  
  /* Get the description of the server type. */
  err |= optionhash_extract_string (options, "description", 0, NULL, 
				    &server->description, txt);

  /* Set the procedures. */
  functions = svz_hash_create (4, (svz_free_func_t) guile_unprotect);
  for (n = 0; guile_functions[n] != NULL; n++)
    {
      proc = SCM_UNDEFINED;
      err |= optionhash_extract_proc (options, guile_functions[n],
				      1, SCM_UNDEFINED, &proc, txt);
      svz_hash_put (functions, guile_functions[n], SVZ_NUM2PTR (proc));
      if (!SCM_UNBNDP (proc))
	scm_gc_protect_object (proc);
    }

  /* Check duplicate server types. */
  if (svz_servertype_get (server->prefix, 0) != NULL)
    {
      guile_error ("Duplicate servertype definition: `%s'", server->prefix);
      err = -1;
    }
  else
    {
      /* Check the configuration items for this servertype. */
      err |= guile_servertype_config (server, 
				      optionhash_get (options, 
						      "configuration"));
    }

  if (!err)
    {
      server->global_init = guile_func_global_init;
      server->init = guile_func_init;
      server->detect_proto = guile_func_detect_proto;
      server->connect_socket = guile_func_connect_socket;
      server->finalize = guile_func_finalize;
      server->global_finalize = guile_func_global_finalize;
      server->info_client = guile_func_info_client;
      server->info_server = guile_func_info_server;
      server->notify = guile_func_notify;
      server->reset = guile_func_reset;
      server->handle_request = guile_func_handle_request;
      guile_servertype_add (server, functions);
    }
  else
    {
      svz_free (server->prefix);
      if (server->description)
	svz_free (server->description);
      svz_free (server);
      svz_hash_destroy (functions);
    }

 out:
  optionhash_destroy (options);
  svz_free (txt);
  guile_global_error |= err;
  return err ? SCM_BOOL_F : SCM_BOOL_T;
}
#undef FUNC_NAME

/*
 * Destroys the servertype represented by the hash @var{callbacks}. Removes
 * the servertype pointer from the hash, destroys the remaining callback
 * hash and finally frees all resources allocated by the servertype.
 */
static void
guile_servertype_destroy (svz_hash_t *callbacks)
{
  svz_servertype_t *stype;

  stype = svz_hash_delete (callbacks, "server-type");
  svz_hash_destroy (callbacks);
  svz_free (stype->prefix);
  svz_free (stype->description);
  guile_servertype_config_free (stype);
  svz_free (stype);
}

/*
 * Destroys the given socket represented by the hash @var{callbacks} and
 * destroy this hash recursively.
 */
static void
guile_sock_destroy (svz_hash_t *callbacks)
{
  svz_hash_destroy (callbacks);
}

#include "guile-api.c"

/*
 * Initialization of the guile server module. Should be run before calling
 * @code{guile_eval_file()}. It registers some new guile procedures and 
 * creates some static data.
 */
void
guile_server_init (void)
{
  if (guile_server || guile_sock)
    return;

  guile_server = 
    svz_hash_create (4, (svz_free_func_t) guile_servertype_destroy);
  guile_sock = 
    svz_hash_create (4, (svz_free_func_t) guile_sock_destroy);

  scm_c_define_gsubr ("define-servertype!", 
		      1, 0, 0, guile_define_servertype);
  scm_c_define_gsubr ("svz:sock:boundary", 
		      2, 0, 0, guile_sock_boundary);
  scm_c_define_gsubr ("svz:sock:floodprotect", 
		      1, 1, 0, guile_sock_floodprotect);
  scm_c_define_gsubr ("svz:sock:print", 
		      2, 0, 0, guile_sock_print);
  scm_c_define_gsubr ("svz:sock:data", 
		      1, 1, 0, guile_sock_data);
  scm_c_define_gsubr ("svz:server:config-ref", 
		      2, 0, 0, guile_server_config_ref);
  scm_c_define_gsubr ("svz:server:state-ref",
		      2, 0, 0, guile_server_state_ref);
  scm_c_define_gsubr ("svz:server:state-set!",
		      3, 0, 0, guile_server_state_set_x);
  scm_c_define_gsubr ("svz:server:state->hash",
		      1, 0, 0, guile_server_state_to_hash);
  scm_c_define_gsubr ("serveez-exceptions",
		      0, 1, 0, guile_access_exceptions);
  scm_c_define_gsubr ("serveez-nuke",
		      0, 0, 0, guile_nuke_happened);
  DEFINE_SOCK_CALLBACK ("svz:sock:handle-request", guile_sock_handle_request);
  DEFINE_SOCK_CALLBACK ("svz:sock:check-request", guile_sock_check_request);

  guile_svz_socket_tag = scm_make_smob_type ("svz:sock", 0);
  scm_set_smob_print (guile_svz_socket_tag, guile_svz_socket_print);

  guile_svz_server_tag = scm_make_smob_type ("svz:server", 0);
  scm_set_smob_print (guile_svz_server_tag, guile_svz_server_print);

  guile_svz_servertype_tag = scm_make_smob_type ("svz:servertype", 0);
  scm_set_smob_print (guile_svz_servertype_tag, guile_svz_servertype_print);

  guile_api_init ();
}

/*
 * This function should be called before shutting down the core library in
 * order to avoid memory leaks. It releases the server types defined with
 * guile.
 */
void
guile_server_finalize (void)
{
  guile_api_finalize ();

  if (guile_server)
    {
      svz_hash_destroy (guile_server);
      guile_server = NULL;
    }

  if (guile_sock)
    {
      svz_hash_destroy (guile_sock);
      guile_sock = NULL;
    }
}

#else /* not ENABLE_GUILE_SERVER */

static int have_guile_server = 0;

#endif /* ENABLE_GUILE_SERVER */
