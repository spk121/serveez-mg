/*
 * guile-api.h - compatibility and miscellaneous guile functionality
 *
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __GUILE_API_H__
#define __GUILE_API_H__ 1

#include <libguile.h>

void guile_api_init (void);
void guile_api_finalize (void);
#define EXPORT __attribute__((visibility("default")))
SCM EXPORT guile_lookup (const char *name);
SCM EXPORT guile_sock_connect (SCM host, SCM proto, SCM port);
SCM EXPORT guile_sock_disconnected_socket (SCM sock, SCM proc);
SCM EXPORT guile_sock_kicked_socket (SCM sock, SCM proc);
SCM EXPORT guile_sock_trigger_cond (SCM sock, SCM proc);
SCM EXPORT guile_sock_trigger_func (SCM sock, SCM proc);
SCM EXPORT guile_sock_idle_func (SCM sock, SCM proc);
SCM EXPORT guile_sock_check_request_oob (SCM sock, SCM proc);
SCM EXPORT scm_portmap (SCM prognum, SCM versnum, SCM protocol, SCM port);
SCM EXPORT guile_coserver_dns (SCM host, SCM callback, SCM arg);
SCM EXPORT guile_coserver_rdns (SCM addr, SCM callback, SCM arg);
SCM EXPORT guile_coserver_ident (SCM sock, SCM callback, SCM arg);
SCM EXPORT guile_sock_find (SCM ident);
SCM EXPORT guile_sock_ident (SCM sock);


#undef EXPORT
#endif /* not __GUILE_API_H__ */
