/*
 * foo-proto.h - example server header
 *
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: foo-proto.h,v 1.13 2001/06/27 20:38:36 ela Exp $
 *
 */

#ifndef __FOO_PROTO_H__
#define __FOO_PROTO_H__ 1

#if HAVE_CONFIG_H
# include <config.h>
#endif

/*
 * Protocol server specific configuration.
 */
typedef struct
{
  int dummy;              /* integer example */
  svz_array_t *messages;  /* string array example */
  char *reply;            /* string example */
  svz_array_t *ports;     /* integer array example */
  int bar;                /* integer example */
  svz_portcfg_t *port;    /* example port configuration */
  svz_hash_t *assoc;      /* a hash example */
  int truth;              /* boolean example */
}
foo_config_t;

/*
 * Basic server callback definitions.
 */
int foo_detect_proto (svz_server_t *server, svz_socket_t *sock);
int foo_connect_socket (svz_server_t *server, svz_socket_t *sock);
int foo_init (svz_server_t *server);
int foo_global_init (svz_servertype_t *server);
int foo_finalize (svz_server_t *server);
int foo_global_finalize (svz_servertype_t *server);
char *foo_info_server (svz_server_t *server);

/*
 * This server's definition.
 */
extern svz_servertype_t foo_server_definition;

#endif /* not __FOO_PROTO_H__ */
