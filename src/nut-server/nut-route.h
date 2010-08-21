/*
 * nut-route.h - gnutella routing definitions
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
 * $Id: nut-route.h,v 1.6 2001/08/01 10:16:23 ela Exp $
 *
 */

#ifndef __NUT_ROUTE_H__
#define __NUT_ROUTE_H__

#include <config.h>

#define NUT_QUERY_TOO_RECENT 10 /* drop "unpatient" queries in seconds */
#define NUT_INVALID_PACKETS  20 /* close connection after x invalid packets */

/* routing function */
int nut_route (svz_socket_t *sock, nut_header_t *hdr, uint8_t *packet);

#endif /* __NUT_ROUTE_H__ */
