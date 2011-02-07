/*
 * server-socket.h - server socket definitions and declarations
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

#ifndef __SERVER_SOCKET_H__
#define __SERVER_SOCKET_H__ 1

#include "defines.h"

__BEGIN_DECLS

SERVEEZ_API svz_socket_t *svz_server_create (svz_portcfg_t *);
SERVEEZ_API int svz_tcp_accept (svz_socket_t *);
SERVEEZ_API int svz_pipe_accept (svz_socket_t *);

__END_DECLS

#endif /* not __SERVER_SOCKET_H__ */
