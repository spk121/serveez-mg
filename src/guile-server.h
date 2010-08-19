/*
 * guile-server.h - guile server module interface definitions
 *
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: guile-server.h,v 1.1 2001/06/27 20:38:36 ela Exp $
 *
 */

#ifndef __GUILE_SERVER_H__
#define __GUILE_SERVER_H__ 1

#include "libserveez.h"
#include <libguile.h>

void guile_server_init (void);
void guile_server_finalize (void);
SCM guile_sock_setfunction (svz_socket_t *sock, char *func, SCM proc);
SCM guile_sock_getfunction (svz_socket_t *sock, char *func);
int guile_func_kicked_socket (svz_socket_t *sock, int reason);
int guile_func_trigger_cond (svz_socket_t *sock);
int guile_func_idle_func (svz_socket_t *sock);
int guile_func_check_request_oob (svz_socket_t *sock);
int guile_func_trigger_func (svz_socket_t *sock);






int guile_func_disconnected_socket (svz_socket_t *sock);

#define MAKE_SOCK_CALLBACK(C_FUNCNAME, C_CALLBACK, TYPE, SCM_FUNCNAME)  \
  SCM                                                                   \
  C_FUNCNAME (SCM sock, SCM proc)                                       \
  {                                                                     \
    svz_socket_t *xsock;                                                \
    scm_assert_smob_type (guile_svz_socket_tag, sock);                  \
    xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);                      \
    if (!SCM_UNBNDP (proc))                                             \
      {                                                                 \
        SCM_ASSERT (scm_is_true (scm_procedure_p (proc)), proc,         \
                                 SCM_ARG2, FUNC_NAME);                  \
        xsock->TYPE = C_CALLBACK;                                       \
        return guile_sock_setfunction (xsock, SCM_FUNCNAME, proc);      \
      }                                                                 \
    return guile_sock_getfunction (xsock, SCM_FUNCNAME);                \
  } 


#endif /* not __GUILE_SERVER_H__ */
