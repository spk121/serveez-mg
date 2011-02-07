/*
 * http-cgi.h - http cgi header file
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __HTTP_CGI_H__
#define __HTTP_CGI_H__

#include <config.h>

#include "http-proto.h"

#define POST_METHOD 0            /* POST id */
#define GET_METHOD  1            /* GET id */
#define HTTP_NO_CGI ((char *)-1) /* 'no cgi' pointer */

#define CGI_VERSION "CGI/1.0"

char *http_check_cgi (svz_socket_t *sock, char *request);
int http_cgi_exec (svz_socket_t *, svz_t_handle, svz_t_handle,
                   char *, char *, int);
int http_post_response (svz_socket_t *sock, char *request, int flags);
int http_cgi_get_response (svz_socket_t *sock, char *request, int flags);
int http_cgi_write (svz_socket_t *sock);
int http_cgi_read (svz_socket_t *sock);
int http_cgi_disconnect (svz_socket_t *sock);
int http_cgi_died (svz_socket_t *sock);
void http_gen_cgi_apps (http_config_t *cfg);

#endif /* __HTTP_CGI_H__ */
