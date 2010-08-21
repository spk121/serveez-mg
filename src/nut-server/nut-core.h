/*
 * nut-core.h - gnutella core definitions
 *
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: nut-core.h,v 1.9 2002/02/03 09:34:05 ela Exp $
 *
 */

#ifndef __NUT_CORE_H__
#define __NUT_CORE_H__

#include <config.h>

/* Gnutella core functions. */
nut_client_t *nut_create_client (void);
void nut_calc_guid (uint8_t *guid);
char *nut_print_guid (uint8_t *guid);
char *nut_text_guid (uint8_t *guid);
char *nut_parse_host (char *addr, unsigned short *port);
int nut_parse_addr (char *addr, unsigned long *ip, unsigned short *port);
char *nut_client_key (unsigned long ip, unsigned short port);
char *nut_parse_property (char *header, int len, char *property);
void nut_canonize_file (char *file);

/*
 * Because the gnutella protocol is a binary protocol we need to convert
 * packets to structures and backwards.
 */
nut_header_t * nut_get_header (uint8_t *data);
uint8_t * nut_put_header (nut_header_t *hdr);

nut_pong_t * nut_get_pong (uint8_t *data);
uint8_t * nut_put_pong (nut_pong_t *reply);

nut_query_t * nut_get_query (uint8_t *data);
uint8_t * nut_put_query (nut_query_t *query);

nut_record_t * nut_get_record (uint8_t *data);
uint8_t * nut_put_record (nut_record_t *record);

nut_reply_t * nut_get_reply (uint8_t *data);
uint8_t * nut_put_reply (nut_reply_t *reply);

nut_push_t * nut_get_push (uint8_t *data);
uint8_t * nut_put_push (nut_push_t *push);

/*
 * Little / Big Endian conversions for 4 byte (long) and 2 byte (short)
 * values. BTW: Network byte order is big endian.
 */

#define SIZEOF_UINT16 2
#define SIZEOF_UINT32 4

#define __BSWAP_32(x) ((unsigned long) \
  ((((x) & 0xff000000) >> 24) | (((x) & 0x000000ff) << 24) | \
   (((x) & 0x0000ff00) << 8)  | (((x) & 0x00ff0000) >> 8)))
#define __BSWAP_16(x) ((unsigned short) \
  ((((x) >> 8) & 0x00ff) | (((x) << 8) & 0xff00)))

#define ltons(x) __BSWAP_16(x)
#define ntols(x) __BSWAP_16(x)
#define ltonl(x) __BSWAP_32(x)
#define ntoll(x) __BSWAP_32(x)

#if WORDS_BIGENDIAN /* big endian */
# define ltohs(x) __BSWAP_16(x)
# define htols(x) __BSWAP_16(x)
# define ltohl(x) __BSWAP_32(x)
# define htoll(x) __BSWAP_32(x)
#else /* little endian */
# define ltohs(x) (x)
# define htols(x) (x)
# define ltohl(x) (x)
# define htoll(x) (x)
#endif

#endif /* __NUT_CORE_H__ */
