/*
 * portcfg.h - port configuration interface
 *
 * Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: portcfg.h,v 1.21 2002/01/31 18:31:50 ela Exp $
 *
 */

#ifndef __PORTCFG_H__
#define __PORTCFG_H__ 1

#define _GNU_SOURCE 1
#include <sys/socket.h>
#include <netinet/in.h>

#include "defines.h"
#include "array.h"
#include "hash.h"
#include "pipe-socket.h"

/* Port configuration items. */
#define PORTCFG_PORT    "port"
#define PORTCFG_PROTO   "proto"
#define PORTCFG_TCP     "tcp"
#define PORTCFG_UDP     "udp"
#define PORTCFG_ICMP    "icmp"
#define PORTCFG_RAW     "raw"
#define PORTCFG_PIPE    "pipe"
#define PORTCFG_IP      "ipaddr"
#define PORTCFG_DEVICE  "device"
#define PORTCFG_ANY     "any"
#define PORTCFG_NOIP    "*"
#define PORTCFG_BACKLOG "backlog"
#define PORTCFG_TYPE    "type"

/* Pipe definitions. */
#define PORTCFG_RECV  "recv"
#define PORTCFG_SEND  "send"
#define PORTCFG_NAME  "name"
#define PORTCFG_PERMS "permissions"
#define PORTCFG_USER  "user"
#define PORTCFG_GROUP "group"
#define PORTCFG_UID   "uid"
#define PORTCFG_GID   "gid"

/* Miscellaneous definitions. */
#define PORTCFG_SEND_BUFSIZE "send-buffer-size"
#define PORTCFG_RECV_BUFSIZE "recv-buffer-size"
#define PORTCFG_FREQ         "connect-frequency"
#define PORTCFG_ALLOW        "allow"
#define PORTCFG_DENY         "deny"

/* Port configuration flags. */
#define PORTCFG_FLAG_ANY    0x0001
#define PORTCFG_FLAG_ALL    0x0002
#define PORTCFG_FLAG_DEVICE 0x0004

/* Return values for port configuration comparisons. */
#define PORTCFG_NOMATCH  0x0001
#define PORTCFG_EQUAL    0x0002
#define PORTCFG_MATCH    0x0004
#define PORTCFG_CONFLICT 0x0008

/*
 * Definition of a single port configuration reflecting either a network
 * (TCP, UDP, ICMP or RAW) or filesystem configuration (PIPE).
 */
typedef struct svz_portcfg
{
  /* the symbolic name of this port configuration */
  char *name;

  /* one of the PROTO_ flags defined in <core.h> */
  int proto;

  /* one of PORTCFG_FLAG_ flags */
  int flags;

  /* unified structure for each type of port */
  union protocol_t
  {
    /* tcp port */
    struct tcp_t
    {
      unsigned short port;     /* TCP/IP port */
      char *ipaddr;            /* dotted decimal or "*" for any address */
      struct sockaddr_in addr; /* converted from the above 2 values */
      char *device;            /* network device */
      int backlog;             /* backlog argument for listen() */
    } tcp;

    /* udp port */
    struct udp_t
    {
      unsigned short port;     /* UDP port */
      char *ipaddr;            /* dotted decimal or "*" */
      struct sockaddr_in addr; /* converted from the above 2 values */
      char *device;            /* network device */
    } udp;

    /* icmp port */
    struct icmp_t
    {
      char *ipaddr;            /* dotted decimal or "*" */
      struct sockaddr_in addr; /* converted from the above value */
      char *device;            /* network device */
      unsigned char type;      /* message type */
    } icmp;

    /* raw ip port */
    struct raw_t
    {
      char *ipaddr;            /* dotted decimal or "*" */
      struct sockaddr_in addr; /* converted from the above value */
      char *device;            /* network device */
    } raw;

    /* pipe port */
    struct pipe_t
    {
      svz_pipe_t recv;         /* pipe for sending data into serveez */
      svz_pipe_t send;         /* pipe serveez sends responses out on */
    } pipe;
  }
  protocol;

  /* maximum number of bytes for protocol identification */
  int detection_fill;

  /* maximum seconds for protocol identification */
  int detection_wait;

  /* initial buffer sizes */
  int send_buffer_size;
  int recv_buffer_size;

  /* allowed number of connects per second (hammer protection) */
  int connect_freq;

  /* remembers connect frequency for each ip */
  svz_hash_t *accepted;

  /* denied and allowed access list (ip based) */
  svz_array_t *deny;
  svz_array_t *allow;
}
svz_portcfg_t;

/* 
 * Accessor definitions for each type of protocol. 
 */
#define tcp_port protocol.tcp.port
#define tcp_addr protocol.tcp.addr
#define tcp_ipaddr protocol.tcp.ipaddr
#define tcp_device protocol.tcp.device
#define tcp_backlog protocol.tcp.backlog

#define udp_port protocol.udp.port
#define udp_addr protocol.udp.addr
#define udp_ipaddr protocol.udp.ipaddr
#define udp_device protocol.udp.device

#define icmp_addr protocol.icmp.addr
#define icmp_ipaddr protocol.icmp.ipaddr
#define icmp_device protocol.icmp.device
#define icmp_type protocol.icmp.type

#define raw_addr protocol.raw.addr
#define raw_ipaddr protocol.raw.ipaddr
#define raw_device protocol.raw.device

#define pipe_recv protocol.pipe.recv
#define pipe_send protocol.pipe.send

/*
 * Return the pointer of the @code{sockaddr_in} structure of the given
 * port configuration @var{port} if it is a network port configuration. 
 * Otherwise return @code{NULL}.
 */
#define svz_portcfg_addr(port)                               \
  (((port)->proto & PROTO_TCP) ? &((port)->tcp_addr) :       \
   ((port)->proto & PROTO_UDP) ? &((port)->udp_addr) :       \
   ((port)->proto & PROTO_ICMP) ? &((port)->icmp_addr) :     \
   ((port)->proto & PROTO_RAW) ? &((port)->raw_addr) : NULL) \

/*
 * Return the pointer to the ip address @code{ipaddr} of the given
 * port configuration @var{port} if it is a network port configuration. 
 * Otherwise return @code{NULL}.
 */
#define svz_portcfg_ipaddr(port)                            \
  (((port)->proto & PROTO_TCP) ? (port)->tcp_ipaddr :       \
   ((port)->proto & PROTO_UDP) ? (port)->udp_ipaddr :       \
   ((port)->proto & PROTO_ICMP) ? (port)->icmp_ipaddr :     \
   ((port)->proto & PROTO_RAW) ? (port)->raw_ipaddr : NULL) \

/*
 * This macro returns the network device name stored in the given port
 * configuration @var{port} if it is a network port configuration. The
 * returned pointer can be @code{NULL} if there is no such device set
 * or if the port configuration is not a network port configuration.
 */
#define svz_portcfg_device(port)                            \
  (((port)->proto & PROTO_TCP) ? (port)->tcp_device :       \
   ((port)->proto & PROTO_UDP) ? (port)->udp_device :       \
   ((port)->proto & PROTO_ICMP) ? (port)->icmp_device :     \
   ((port)->proto & PROTO_RAW) ? (port)->raw_device : NULL)

#define svz_portcfg_tcp_udp_device(port)                                \
  (((port)->proto & PROTO_TCP) ? (port)->tcp_device : (port)->udp_device )
#define svz_portcfg_icmp_device(port)           \
  ((port)->icmp_device)
#define svz_portcfg_raw_device(port)            \
  ((port)->raw_device)

/*
 * Return the UDP or TCP port of the given port configuration or zero
 * if it neither TCP nor UDP.
 */
#define svz_portcfg_port(port)                         \
  (((port)->proto & PROTO_TCP) ? (port)->tcp_port :    \
   ((port)->proto & PROTO_UDP) ? (port)->udp_port : 0) \

__BEGIN_DECLS

SERVEEZ_API svz_portcfg_t *svz_portcfg_create (void);
SERVEEZ_API int svz_portcfg_equal (svz_portcfg_t *, 
					     svz_portcfg_t *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_add (char *, 
						      svz_portcfg_t *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_del (char *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_get (char *);
SERVEEZ_API void svz_portcfg_destroy (svz_portcfg_t *);
SERVEEZ_API void svz_portcfg_free (svz_portcfg_t *);
SERVEEZ_API void svz_portcfg_finalize (void);
SERVEEZ_API int svz_portcfg_mkaddr (svz_portcfg_t *);
SERVEEZ_API void svz_portcfg_prepare (svz_portcfg_t *);
SERVEEZ_API void svz_portcfg_print (svz_portcfg_t *, FILE *);
SERVEEZ_API char *svz_portcfg_text (svz_portcfg_t *);
SERVEEZ_API svz_portcfg_t *svz_portcfg_dup (svz_portcfg_t *);
SERVEEZ_API svz_array_t *svz_portcfg_expand (svz_portcfg_t *);
SERVEEZ_API int svz_portcfg_set_ipaddr (svz_portcfg_t *, char *);
SERVEEZ_API void svz_portcfg_destroy_access (svz_portcfg_t *);
SERVEEZ_API void svz_portcfg_destroy_accepted (svz_portcfg_t *);

__END_DECLS

#endif /* __PORTCFG_H__ */
