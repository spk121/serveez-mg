/*
 * nut-request.h - gnutella requests header file
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
 * $Id: nut-request.h,v 1.4 2001/08/01 10:16:23 ela Exp $
 *
 */

#ifndef __NUT_REQUEST_H__
#define __NUT_REQUEST_H__ 1

#include <config.h>
#include <stdint.h>

/* Exported functions. */
int nut_reply (svz_socket_t *sock, nut_header_t *hdr, uint8_t *packet);
int nut_push_request (svz_socket_t *sock, nut_header_t *hdr, 
		      uint8_t *packet);
int nut_query (svz_socket_t *sock, nut_header_t *hdr, uint8_t *packet);
int nut_pong (svz_socket_t *sock, nut_header_t *hdr, uint8_t *packet);
int nut_ping (svz_socket_t *sock, nut_header_t *hdr, uint8_t *null);

#endif /* not __NUT_REQUEST_H__ */
