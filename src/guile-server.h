/*
 * guile-server.h - guile server module interface definitions
 *
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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
SCM guile_call (SCM code, int args, ...);

#define EXPORT __attribute__((visibility("default")))
SCM EXPORT guile_nuke_happened (void);
SCM EXPORT guile_access_exceptions (SCM enable);
SCM EXPORT guile_sock_handle_request (SCM sock, SCM proc);
SCM EXPORT guile_sock_check_request (SCM sock, SCM proc);
SCM EXPORT guile_sock_data (SCM sock, SCM data);
SCM EXPORT guile_server_config_ref (SCM server, SCM key);
SCM EXPORT guile_server_state_ref (SCM server, SCM key);
SCM EXPORT guile_server_state_set_x (SCM server, SCM key, SCM value);
SCM EXPORT guile_server_state_to_hash (SCM server);
SCM EXPORT guile_define_servertype (SCM args);
#undef EXPORT











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

/* Provides a socket callback setter/getter. */
#define DEFINE_SOCK_CALLBACK(assoc, func) \
  scm_c_define_gsubr (assoc, 1, 1, 0, func)



#endif /* not __GUILE_SERVER_H__ */
