/*
 * raw-socket.c - raw ip socket implementations
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
 * $Id: raw-socket.c,v 1.9 2003/06/14 14:57:59 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <netinet/in.h>
#endif

#ifdef __MINGW32__
# include <winsock2.h>
#endif

#include "libserveez/util.h"
#include "libserveez/core.h"
#include "libserveez/raw-socket.h"

/*
 * The raw socket support on Windoze machines is quite doubtful and
 * almostly unusable because:
 * 1. sent packets are not received on the same socket
 * 2. not all raw sockets get all ip packets in all processes
 * 3. not sure about Winsock 1/2 versions to use (ws2_32.dll or wsock32.dll)
 */

/*
 * Get IP header from plain data.
 */
svz_ip_header_t *
svz_raw_get_ip_header (svz_uint8_t *data)
{
  static svz_ip_header_t hdr;
  unsigned short uint16;
  unsigned int uint32;

  hdr.version_length = *data++;
  hdr.tos = *data++;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.length = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.ident = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.frag_offset = ntohs (uint16);
  data += SIZEOF_UINT16;
  hdr.ttl = *data++;
  hdr.protocol = *data++;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.checksum = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint32, data, SIZEOF_UINT32);
  hdr.src = uint32;
  data += SIZEOF_UINT32;
  memcpy (&uint32, data, SIZEOF_UINT32);
  hdr.dst = uint32;

  return &hdr;
}

/*
 * Put IP header to plain data. This is currently not in use but can be
 * used when creating raw sockets with setsockopt (SOL_IP, IP_HDRINCL).
 */
svz_uint8_t *
svz_raw_put_ip_header (svz_ip_header_t *hdr)
{
  static svz_uint8_t buffer[IP_HEADER_SIZE];
  svz_uint8_t *data = buffer;
  unsigned short uint16;
  unsigned int uint32;

  *data++ = hdr->version_length;
  *data++ = hdr->tos;
  uint16 = htons (hdr->length);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  uint16 = htons (hdr->ident);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  uint16 = htons (hdr->frag_offset);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  *data++ = hdr->ttl;
  *data++ = hdr->protocol;
  uint16 = htons (hdr->checksum);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  uint32 = hdr->src;
  memcpy (data, &uint32, SIZEOF_UINT32);
  data += SIZEOF_UINT32;
  uint32 = hdr->dst;
  memcpy (data, &uint32, SIZEOF_UINT32);

  return buffer;
}

/*
 * Recalculate any IP checksum.
 */
unsigned short
svz_raw_ip_checksum (svz_uint8_t *data, int len)
{
  register unsigned checksum = 0;

  /* 
   * Calculate the 16 bit one's complement of the one's complement sum 
   * of all 16 bit words in the header. For computing the checksum, 
   * the checksum field should be zero. This checksum may be replaced in 
   * the future.
   */
  while (len > 1)
    {
      /* This is the inner loop */
      checksum += *data | (*(data + 1) << 8);
      len -= 2;
      data += 2;
    }

  /* Add left-over byte, if any */
  if (len > 0)
    checksum += *data;

  /* Fold 32-bit checksum to 16 bits */
  while (checksum >> 16)
    checksum = (checksum & 0xffff) + (checksum >> 16);
  checksum = ~checksum;

  return htons ((unsigned short) checksum);
}

/*
 * Checking the IP header only. Return the length of the header if it 
 * is valid, otherwise -1.
 */
int
svz_raw_check_ip_header (svz_uint8_t *data, int len)
{
  svz_ip_header_t *ip_header;

  /* get the IP header and reject the checksum within plain data */
  ip_header = svz_raw_get_ip_header (data);
  data[IP_CHECKSUM_OFS] = 0;
  data[IP_CHECKSUM_OFS + 1] = 0;

#if 0
  printf ("ip version      : %d\n"
	  "header length   : %d byte\n"
	  "type of service : %d\n"
	  "total length    : %d byte\n"
	  "ident           : 0x%04X\n"
	  "flags           : 0x%04X %s %s\n"
	  "frag. offset    : %d\n"
	  "ttl             : %d\n"
	  "protocol        : 0x%02X\n"
	  "checksum        : 0x%04X\n"
	  "source          : %s\n",
	  IP_HDR_VERSION (ip_header), IP_HDR_LENGTH (ip_header),
	  ip_header->tos, ip_header->length, ip_header->ident, 
	  IP_HDR_FLAGS (ip_header),
	  IP_HDR_FLAGS (ip_header) & IP_FLAG_DF ? "[Don't Fragment]" : "",
	  IP_HDR_FLAGS (ip_header) & IP_FLAG_MF ? "[More Fragments]" : "",
	  IP_HDR_FRAG (ip_header), ip_header->ttl, ip_header->protocol,
	  ip_header->checksum, svz_inet_ntoa (ip_header->src));
  printf ("destination     : %s\n", svz_inet_ntoa (ip_header->dst));
#endif

  /* Is this IPv4 version ? */
  if (IP_HDR_VERSION (ip_header) != IP_VERSION_4)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "raw: cannot handle IPv%d\n", 
	       IP_HDR_VERSION (ip_header));
#endif
      return -1;
    }

  /* Check Internet Header Length. */
  if (IP_HDR_LENGTH (ip_header) > len)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "raw: invalid IHL (%d > %d)\n",
	       IP_HDR_LENGTH (ip_header), len);
#endif
      return -1;
    }

  /* Check total length. */
  if (ip_header->length < len)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "raw: invalid total length (%d < %d)\n",
	       ip_header->length, len);
#endif
      return -1;
    }

  /* Check protocol type. */
  if (ip_header->protocol != ICMP_PROTOCOL)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "raw: invalid protocol 0x%02X\n",
	       ip_header->protocol);
#endif
      return -1;
    }

  /* Recalculate and check the header checksum. */
  if (svz_raw_ip_checksum (data, IP_HDR_LENGTH (ip_header)) != 
      ip_header->checksum)
    {
      /* FIXME: Why are header checksums invalid on big packets ? */
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, 
	       "raw: invalid ip header checksum (%04X != %04X)\n",
	       svz_raw_ip_checksum (data, IP_HDR_LENGTH (ip_header)),
	       ip_header->checksum);
#endif
    }

  return IP_HDR_LENGTH (ip_header);
}
