/*
 * raw-socket.h - raw ip socket header definitions
 *
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
 * $Id: raw-socket.h,v 1.6 2001/12/13 18:00:00 ela Exp $
 *
 */

#ifndef __RAW_SOCKET_H__
#define __RAW_SOCKET_H__ 1

#include <stdint.h>             /* uint8_t */
#include "defines.h"

/* local definitions */
#define IP_VERSION_4     4
#define IP_CHECKSUM_OFS  10
#define IP_HEADER_SIZE   20
#define SIZEOF_UINT16    2
#define SIZEOF_UINT32    4

/* ip protocol definitions */
#define ICMP_PROTOCOL  1
#define TCP_PROTOCOL   6
#define UDP_PROTOCOL  17

/* version and length are 4 bit values in the ip header */
#define IP_HDR_VERSION(hdr) ((hdr->version_length >> 4) & 0x0f)
#define IP_HDR_LENGTH(hdr)  ((hdr->version_length & 0x0f) << 2)

/* ip header flags (part of frag_offset) */
#define IP_HDR_FLAGS(hdr) ((hdr->frag_offset) & 0xE000)
#define IP_FLAG_DF 0x4000 /* Don't Fragment This Datagram (DF). */
#define IP_FLAG_MF 0x2000 /* More Fragments Flag (MF). */
#define IP_HDR_FRAG(hdr) ((hdr->frag_offset) & 0x1FFF)

/* IP header structure. */
typedef struct
{
  uint8_t version_length;     /* header length (in DWORDs) and ip version */
  uint8_t tos;                /* type of service = 0 */
  unsigned short length;      /* total ip packet length */
  unsigned short ident;       /* ip identifier */
  unsigned short frag_offset; /* fragment offset (in 8 bytes) and flags */
  uint8_t ttl;                /* time to live */
  uint8_t protocol;           /* ip protocol */
  unsigned short checksum;    /* ip header checksum */
  unsigned long src;          /* source address */
  unsigned long dst;          /* destination address */
}
svz_ip_header_t;

__BEGIN_DECLS

/* Exported RAW IP socket functions. */
SERVEEZ_API svz_ip_header_t * svz_raw_get_ip_header __PARAMS ((uint8_t *));
SERVEEZ_API uint8_t * svz_raw_put_ip_header __PARAMS ((svz_ip_header_t *));
SERVEEZ_API unsigned short svz_raw_ip_checksum __PARAMS ((uint8_t *, int));
SERVEEZ_API int svz_raw_check_ip_header __PARAMS ((uint8_t *, int));

__END_DECLS

#endif /* not __RAW_SOCKET_H__ */
