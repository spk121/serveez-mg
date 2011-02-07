/*
 * icmp-socket.h - ICMP socket definitions and declarations
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
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

#ifndef __ICMP_SOCKET_H__
#define __ICMP_SOCKET_H__ 1

#include <stdint.h>             /* uint8_t */
#include "defines.h"
#include "socket.h"

/* local definitions */
#define ICMP_HEADER_SIZE 10
#define ICMP_MSG_SIZE    (64 * 1024)
#define ICMP_BUF_SIZE    (4 * (ICMP_MSG_SIZE + ICMP_HEADER_SIZE + 24))

/* general definitions */
#define ICMP_ECHOREPLY          0       /* Echo Reply                   */
#define ICMP_DEST_UNREACH       3       /* Destination Unreachable      */
#define ICMP_SOURCE_QUENCH      4       /* Source Quench                */
#define ICMP_REDIRECT           5       /* Redirect (change route)      */
#define ICMP_ECHO               8       /* Echo Request                 */
#define ICMP_TIME_EXCEEDED      11      /* Time Exceeded                */
#define ICMP_PARAMETERPROB      12      /* Parameter Problem            */
#define ICMP_TIMESTAMP          13      /* Timestamp Request            */
#define ICMP_TIMESTAMPREPLY     14      /* Timestamp Reply              */
#define ICMP_INFO_REQUEST       15      /* Information Request          */
#define ICMP_INFO_REPLY         16      /* Information Reply            */
#define ICMP_ADDRESS            17      /* Address Mask Request         */
#define ICMP_ADDRESSREPLY       18      /* Address Mask Reply           */
#define ICMP_MAX_TYPE           18

/* serveez ICMP types and sub-codes */
#define ICMP_SERVEEZ        42
#define ICMP_SERVEEZ_DATA    0
#define ICMP_SERVEEZ_REQ     1
#define ICMP_SERVEEZ_ACK     2
#define ICMP_SERVEEZ_CLOSE   3
#define ICMP_SERVEEZ_CONNECT 4

/* ICMP header structure.  */
typedef struct
{
  uint8_t type;        /* message type */
  uint8_t code;        /* type sub-code */
  unsigned short checksum; /* check sum */
  unsigned short ident;    /* identifier */
  unsigned short sequence; /* sequence number */
  unsigned short port;     /* remote port address */
}
svz_icmp_header_t;

__BEGIN_DECLS

/* Exported ICMP socket functions.  */
SERVEEZ_API int svz_icmp_read_socket (svz_socket_t *);
SERVEEZ_API int svz_icmp_lazy_read_socket (svz_socket_t *);
SERVEEZ_API int svz_icmp_write_socket (svz_socket_t *);
SERVEEZ_API int svz_icmp_check_request (svz_socket_t *);
SERVEEZ_API svz_socket_t *svz_icmp_connect (unsigned long,
                                            unsigned short,
                                            unsigned char);
SERVEEZ_API int svz_icmp_send_control (svz_socket_t *, uint8_t);
SERVEEZ_API int svz_icmp_write (svz_socket_t *, char *, int);
SERVEEZ_API int svz_icmp_printf (svz_socket_t *,
                                 const char *, ...);

__END_DECLS

#endif /* !__ICMP_SOCKET_H__ */
