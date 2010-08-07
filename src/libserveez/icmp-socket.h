/*
 * icmp-socket.h - ICMP socket definitions and declarations
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: icmp-socket.h,v 1.9 2003/06/14 14:57:59 ela Exp $
 *
 */

#ifndef __ICMP_SOCKET_H__
#define __ICMP_SOCKET_H__ 1

#include "libserveez/defines.h"
#include "libserveez/socket.h"

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

/* ICMP header structure. */
typedef struct
{
  svz_uint8_t type;        /* message type */
  svz_uint8_t code;        /* type sub-code */
  unsigned short checksum; /* check sum */
  unsigned short ident;    /* identifier */
  unsigned short sequence; /* sequence number */
  unsigned short port;     /* remote port address */
}
svz_icmp_header_t;

__BEGIN_DECLS

#ifdef __MINGW32__

/* Exported `ICMP.DLL' functions. */
SERVEEZ_API void svz_icmp_startup __PARAMS ((void));
SERVEEZ_API void svz_icmp_cleanup __PARAMS ((void));

#endif /* __MINGW32__ */

/* Exported ICMP socket functions. */
SERVEEZ_API int svz_icmp_read_socket __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_icmp_lazy_read_socket __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_icmp_write_socket __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_icmp_check_request __PARAMS ((svz_socket_t *));
SERVEEZ_API svz_socket_t *svz_icmp_connect __PARAMS ((unsigned long,
						      unsigned short,
						      unsigned char));
SERVEEZ_API int svz_icmp_send_control __PARAMS ((svz_socket_t *, svz_uint8_t));
SERVEEZ_API int svz_icmp_write __PARAMS ((svz_socket_t *, char *, int));
SERVEEZ_API int svz_icmp_printf __PARAMS ((svz_socket_t *, 
					   svz_c_const char *, ...));

__END_DECLS

#endif /* !__ICMP_SOCKET_H__ */
