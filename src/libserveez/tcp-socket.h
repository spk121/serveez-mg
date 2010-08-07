/*
 * tcp-socket.h - TCP socket connection definition
 *
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 *
 * $Id: tcp-socket.h,v 1.5 2002/06/06 20:04:51 ela Exp $
 *
 */

#ifndef __TCP_SOCKET_H__
#define __TCP_SOCKET_H__ 1

#include "libserveez/defines.h"
#include "libserveez/socket.h"

__BEGIN_DECLS

SERVEEZ_API svz_socket_t *svz_tcp_connect __PARAMS ((unsigned long, 
						     unsigned short));
SERVEEZ_API int svz_tcp_default_connect __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_tcp_read_socket __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_tcp_write_socket __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_tcp_recv_oob __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_tcp_send_oob __PARAMS ((svz_socket_t *));

__END_DECLS

#endif /* not __TCP_SOCKET_H__ */
