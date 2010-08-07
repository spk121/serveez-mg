/*
 * binding.h - server to port binding declarations and definitions
 *
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
 * $Id: binding.h,v 1.11 2002/01/24 15:18:03 ela Exp $
 *
 */

#ifndef __BINDING_H__
#define __BINDING_H__ 1

#include "libserveez/defines.h"

/*
 * A server can typically be bound to different port configurations.  This
 * structure hold the binding for a single listening socket structure of a
 * server.
 */
typedef struct svz_binding
{
  svz_server_t *server; /* The server structure. */
  svz_portcfg_t *port;  /* The port configuration the server is bound to. */
}
svz_binding_t;

__BEGIN_DECLS

SERVEEZ_API int svz_server_bind __PARAMS ((svz_server_t *, svz_portcfg_t *));
SERVEEZ_API svz_array_t *svz_server_portcfgs __PARAMS ((svz_server_t *));
SERVEEZ_API char *svz_server_bindings __PARAMS ((svz_server_t *));
SERVEEZ_API void svz_server_unbind __PARAMS ((svz_server_t *));
SERVEEZ_API int svz_server_single_listener __PARAMS ((svz_server_t *, 
						      svz_socket_t *));
SERVEEZ_API svz_array_t *svz_server_listeners __PARAMS ((svz_server_t *));

SERVEEZ_API int svz_sock_add_server __PARAMS ((svz_socket_t *, 
					       svz_server_t *,
					       svz_portcfg_t *));
SERVEEZ_API int svz_sock_del_server __PARAMS ((svz_socket_t *, 
					       svz_server_t *));
SERVEEZ_API svz_socket_t *svz_sock_find_portcfg __PARAMS ((svz_portcfg_t *));
SERVEEZ_API svz_array_t *svz_sock_find_portcfgs __PARAMS ((svz_portcfg_t *));
SERVEEZ_API svz_array_t *svz_sock_bindings __PARAMS ((svz_socket_t *));
SERVEEZ_API svz_array_t *svz_sock_servers __PARAMS ((svz_socket_t *));
SERVEEZ_API svz_socket_t *svz_sock_bind_port __PARAMS ((svz_portcfg_t *));

SERVEEZ_API svz_array_t *svz_binding_filter_net __PARAMS ((svz_socket_t *,
							   unsigned long,
							   unsigned short));
SERVEEZ_API svz_array_t *svz_binding_filter_pipe __PARAMS ((svz_socket_t *)); 
SERVEEZ_API svz_array_t *svz_binding_filter __PARAMS ((svz_socket_t *));
SERVEEZ_API svz_binding_t *svz_binding_create __PARAMS ((svz_server_t *,
							 svz_portcfg_t *));
SERVEEZ_API void svz_binding_destroy __PARAMS ((svz_binding_t *));
SERVEEZ_API int svz_binding_contains __PARAMS ((svz_array_t *, 
						svz_binding_t *));
SERVEEZ_API svz_array_t *svz_binding_join __PARAMS ((svz_array_t *, 
						     svz_socket_t *));
SERVEEZ_API svz_array_t *svz_binding_find_server __PARAMS ((svz_socket_t *,
							    svz_server_t *));
SERVEEZ_API svz_binding_t *svz_binding_find __PARAMS ((svz_socket_t *,
						       svz_server_t *,
						       svz_portcfg_t *));
SERVEEZ_API int svz_binding_contains_server __PARAMS ((svz_socket_t *,
						       svz_server_t *));

__END_DECLS

#endif /* not __BINDING_H__ */
