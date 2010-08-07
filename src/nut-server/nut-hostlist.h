/*
 * nut-hostlist.h - gnutella host list definitions
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
 * $Id: nut-hostlist.h,v 1.2 2001/05/19 23:04:58 ela Exp $
 *
 */

#ifndef __NUT_HOSTLIST_H__
#define __NUT_HOSTLIST_H__

#if HAVE_CONFIG_H
# include <config.h>
#endif

/* output format definitions */
#define NUT_HTTP_HEADER "HTTP 200 OK\r\n"                           \
		        "Server: Gnutella\r\n"                      \
		        "Content-type: text/html\r\n"               \
		        "\r\n"
#define NUT_HTML_HEADER "<html><body bgcolor=white text=black><br>" \
	                "<h1>%d Gnutella Hosts</h1>"                \
	                "<hr noshade><pre>"
#define NUT_HTML_FOOTER "</pre><hr noshade>"                        \
                         "<i>%s/%s server at %s port %d</i>"        \
	                 "</body></html>"

/* exported functions */
int nut_hosts_write (svz_socket_t *sock);
int nut_hosts_check (svz_socket_t *sock);
int nut_host_catcher (svz_socket_t *, unsigned long ip, unsigned short port);

#endif /* __NUT_HOSTLIST_H__ */
