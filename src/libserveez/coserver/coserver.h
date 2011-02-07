/*
 * coserver.h - internal coserver header definitions
 *
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __COSERVER_H__
#define __COSERVER_H__ 1

#include "defines.h"
#include "array.h"
#include "socket.h"

/*
 * Every invoked internal coserver has got such a structure.
 * It contains all the data it needs to run properly.
 */
typedef struct
{
  int pid;                      /* process id */
  char * (* callback) (char *); /* callback routine, blocking... */
  svz_socket_t *sock;           /* socket structure for this coserver */
  int type;                     /* coserver type id */
  int busy;                     /* is this thread currently busy?  */
}
svz_coserver_t;

/*
 * This structure contains the type id and the callback
 * pointer of the internal coserver routines where CALLBACK is
 * the actual (blocking) processing routine.
 */
typedef struct
{
  int type;                       /* coserver type id */
  char *name;                     /* name of the internal coserver */
  char * (* callback) (char *);   /* coserver callback */
  int instances;                  /* the amount of coserver instances */
  void (* init) (void);           /* coserver initialization routine */
  long last_start;                /* time stamp of the last instance fork() */
}
svz_coservertype_t;

/* Definitions for argument list of the coserver callbacks.  */
typedef void * svz_coserver_arg_t;
#define COSERVER_ARGS 2
#define svz_coserver_args_t                             \
  svz_coserver_arg_t arg0, svz_coserver_arg_t arg1

/* Buffer size for the coservers.  */
#define COSERVER_BUFSIZE 256

/*
 * The callback structure is used to finally execute some code
 * which should be called whenever one of the coservers produces
 * any data for the server.
 */
typedef int (* svz_coserver_handle_result_t) (char *, svz_coserver_args_t);

typedef struct
{
  svz_coserver_handle_result_t handle_result; /* any code callback */
  svz_coserver_arg_t arg[COSERVER_ARGS];      /* passed argument array */
}
svz_coserver_callback_t;

/*
 * Types of internal servers you can start as threads or processes.
 * coserver-TODO:
 * add your coserver identification here and increase MAX_COSERVER_TYPES
 */
#define COSERVER_REVERSE_DNS 0 /* reverse DNS lookup ID */
#define COSERVER_IDENT       1 /* identification ID */
#define COSERVER_DNS         2 /* DNS lookup ID */
#define MAX_COSERVER_TYPES   3 /* number of different coservers */

__BEGIN_DECLS

SERVEEZ_API svz_coservertype_t svz_coservertypes[MAX_COSERVER_TYPES];
SERVEEZ_API svz_array_t *svz_coservers;

SERVEEZ_API void svz_coserver_check (void);
SERVEEZ_API int svz_coserver_init (void);
SERVEEZ_API int svz_coserver_finalize (void);
SERVEEZ_API void svz_coserver_destroy (int);
SERVEEZ_API void svz_coserver_create (int);
SERVEEZ_API void svz_coserver_send_request (int, char *, 
                                            svz_coserver_handle_result_t,
                                            svz_coserver_args_t);

/*
 * These are the three wrappers for our existing coservers.
 */
SERVEEZ_API void svz_coserver_rdns_invoke (unsigned long,
                                           svz_coserver_handle_result_t,
                                           svz_coserver_args_t);

/*
 * This macro is considered to be the usual way to make a request to the
 * reverse DNS coserver.  It calls @code{svz_coserver_rdns_invoke()} therefore.
 * If the given @var{ip} has been resolved by the coserver to a valid computer
 * name the callback @var{cb} gets invoked with the additional arguments
 * passed to this macro.
 */
#define svz_coserver_rdns(ip, cb, arg0, arg1)                            \
  svz_coserver_rdns_invoke (ip, (svz_coserver_handle_result_t) cb,       \
                            (svz_coserver_arg_t) ((unsigned long) arg0), \
                            (svz_coserver_arg_t) ((unsigned long) arg1))

SERVEEZ_API void svz_coserver_dns_invoke (char *,
                                          svz_coserver_handle_result_t,
                                          svz_coserver_args_t);

/*
 * This macro is the usual way to make use of the internal DNS coserver.
 * When the given computer name @var{host} has been resolved to a valid
 * ip address the function @var{cb} will be called with the additional
 * arguments @var{arg0} and @var{arg1}.
 */
#define svz_coserver_dns(host, cb, arg0, arg1)                          \
  svz_coserver_dns_invoke (host, (svz_coserver_handle_result_t) cb,     \
                           (svz_coserver_arg_t) ((unsigned long) arg0), \
                           (svz_coserver_arg_t) ((unsigned long) arg1))

SERVEEZ_API void svz_coserver_ident_invoke (svz_socket_t *,
                                            svz_coserver_handle_result_t,
                                            svz_coserver_args_t);

/*
 * This macro uses the internal ident coserver in order to identify the
 * connection of the given socket structure @var{sock}.  The function @var{cb}
 * will be called when the coserver successfully delivers the identified
 * user on the other end of the connection.  Both the arguments @var{arg0}
 * and @var{arg1} are passed to @var{cb}.
 */
#define svz_coserver_ident(sock, cb, arg0, arg1)                          \
  svz_coserver_ident_invoke (sock, (svz_coserver_handle_result_t) cb,     \
                             (svz_coserver_arg_t) ((unsigned long) arg0), \
                             (svz_coserver_arg_t) ((unsigned long) arg1))

__END_DECLS

#endif /* not __COSERVER_H__ */
