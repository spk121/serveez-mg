/*
 * core.h - socket and file descriptor declarations and definitions
 *
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
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
 * $Id: core.h,v 1.16 2003/06/14 14:57:59 ela Exp $
 *
 */

#ifndef __CORE_H__
#define __CORE_H__ 1

#include <stdio.h>              /* FILE * */
#include <netinet/in.h>         /* struct sockaddr_in */
#include <sys/types.h>          /* off_t */

#include "defines.h"
#include "array.h"
#include "socket.h"  /* svz_t_socket */


/* protocol definitions */
#define PROTO_TCP   0x00000001 /* tcp  - bidirectional, reliable */
#define PROTO_UDP   0x00000002 /* udp  - multidirectional, unreliable */
#define PROTO_PIPE  0x00000004 /* pipe - unidirectional, reliable */
#define PROTO_ICMP  0x00000008 /* icmp - multidirectional, unreliable */
#define PROTO_RAW   0x00000010 /* raw  - multidirectional, unreliable */

__BEGIN_DECLS

SERVEEZ_API int svz_fd_nonblock __PARAMS ((int));
SERVEEZ_API int svz_fd_block __PARAMS ((int));
SERVEEZ_API int svz_fd_cloexec __PARAMS ((int));
SERVEEZ_API int svz_tcp_cork __PARAMS ((svz_t_socket, int));
SERVEEZ_API int svz_tcp_nodelay __PARAMS ((svz_t_socket, int, int *));
SERVEEZ_API int svz_socket_type __PARAMS ((svz_t_socket, int *));
SERVEEZ_API int svz_socket_connect __PARAMS ((svz_t_socket, unsigned long, 
					      unsigned short));
SERVEEZ_API svz_t_socket svz_socket_create __PARAMS ((int));
SERVEEZ_API int svz_socket_create_pair __PARAMS ((int, svz_t_socket desc[2]));
SERVEEZ_API char *svz_inet_ntoa __PARAMS ((unsigned long));
SERVEEZ_API int svz_inet_aton __PARAMS ((char *, struct sockaddr_in *));
SERVEEZ_API int svz_sendfile __PARAMS ((int, int, off_t *, unsigned int));
SERVEEZ_API int svz_open __PARAMS ((const char *, int, unsigned int));
SERVEEZ_API int svz_close __PARAMS ((int));
SERVEEZ_API int svz_fstat __PARAMS ((int, struct stat *));
SERVEEZ_API FILE *svz_fopen __PARAMS ((const char *, 
				       const char *));
SERVEEZ_API int svz_fclose __PARAMS ((FILE *));
SERVEEZ_API int svz_file_check __PARAMS ((char *));
SERVEEZ_API char *svz_file_path __PARAMS ((char *, char *));

SERVEEZ_API void svz_file_closeall __PARAMS ((void));

__END_DECLS

#endif /* not __CORE_H__ */
