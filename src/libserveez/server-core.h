/*
 * server-core.h - server management definition
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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
 * $Id: server-core.h,v 1.18 2003/06/14 14:57:59 ela Exp $
 *
 */

#ifndef __SERVER_CORE_H__
#define __SERVER_CORE_H__ 1

#include "defines.h"
#include "socket.h"
#include "portcfg.h"

SERVEEZ_API int svz_nuke_happened;
SERVEEZ_API svz_t_handle svz_child_died;
SERVEEZ_API long svz_notify;

SERVEEZ_API svz_socket_t *svz_sock_root;
SERVEEZ_API svz_socket_t *svz_sock_last;

/*
 * Go through each socket structure in the chained list.
 */
#define svz_sock_foreach(sock) \
  for ((sock) = svz_sock_root; (sock) != NULL; (sock) = (sock)->next)

/*
 * Goes through the chained list of socket structures and filters each
 * listener.
 */
#define svz_sock_foreach_listener(sock)                                \
  svz_sock_foreach (sock)                                              \
    if (((sock)->flags & SOCK_FLAG_LISTENING) && (sock)->port != NULL)

__BEGIN_DECLS

SERVEEZ_API void svz_sock_table_create __PARAMS ((void));
SERVEEZ_API void svz_sock_table_destroy __PARAMS ((void));
SERVEEZ_API svz_socket_t *svz_sock_find __PARAMS ((int, int));
SERVEEZ_API int svz_sock_schedule_for_shutdown __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_sock_shutdown __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_sock_enqueue __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_sock_dequeue __PARAMS ((svz_socket_t *));
SERVEEZ_API void svz_sock_shutdown_all __PARAMS ((void));
SERVEEZ_API void svz_sock_setparent __PARAMS ((svz_socket_t *, 
					       svz_socket_t *));
SERVEEZ_API svz_socket_t *svz_sock_getparent __PARAMS ((svz_socket_t *));
SERVEEZ_API void svz_sock_setreferrer __PARAMS ((svz_socket_t *, 
						 svz_socket_t *));
SERVEEZ_API svz_socket_t *svz_sock_getreferrer __PARAMS ((svz_socket_t *));
SERVEEZ_API svz_portcfg_t *svz_sock_portcfg __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_sock_check_access __PARAMS ((svz_socket_t *, 
						 svz_socket_t *));
SERVEEZ_API int svz_sock_check_frequency __PARAMS ((svz_socket_t *, 
					       svz_socket_t *));
SERVEEZ_API void svz_sock_check_children __PARAMS ((void));
SERVEEZ_API int svz_sock_child_died __PARAMS ((svz_socket_t *));

SERVEEZ_API void svz_executable __PARAMS ((char *));
SERVEEZ_API void svz_sock_check_bogus __PARAMS ((void));
SERVEEZ_API int svz_periodic_tasks __PARAMS ((void));
SERVEEZ_API void svz_loop_pre __PARAMS ((void));
SERVEEZ_API void svz_loop_post __PARAMS ((void));
SERVEEZ_API void svz_loop __PARAMS ((void));
SERVEEZ_API void svz_loop_one __PARAMS ((void));
SERVEEZ_API void svz_signal_up __PARAMS ((void));
SERVEEZ_API void svz_signal_dn __PARAMS ((void));
SERVEEZ_API void svz_signal_handler __PARAMS ((int));
SERVEEZ_API void svz_strsignal_init __PARAMS ((void));
SERVEEZ_API void svz_strsignal_destroy __PARAMS ((void));
SERVEEZ_API char *svz_strsignal __PARAMS ((int));

__END_DECLS

#endif /* not __SERVER_CORE_H__ */
