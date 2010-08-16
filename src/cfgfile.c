/*
 * cfgfile.c - configuration file and left overs
 *
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * $Id: cfgfile.c,v 1.21 2001/10/27 14:12:16 ela Exp $
 *
 */

#include <config.h>
#include "libserveez.h"
#include "cfgfile.h"

/* 
 * Include headers of servers.
 */
#include "foo-server/foo-proto.h"
#if ENABLE_AWCS_PROTO
# include "awcs-server/awcs-proto.h"
#endif
#if ENABLE_HTTP_PROTO
# include "http-server/http-proto.h"
#endif
#if ENABLE_IRC_PROTO
# include "irc-server/irc-proto.h"
#endif
#if ENABLE_CONTROL_PROTO
# include "ctrl-server/control-proto.h"
#endif
#if ENABLE_SNTP_PROTO
# include "sntp-server/sntp-proto.h"
#endif
#if ENABLE_GNUTELLA
# include "nut-server/gnutella.h"
#endif
#if ENABLE_TUNNEL
# include "tunnel-server/tunnel.h"
#endif
#if ENABLE_FAKEIDENT
# include "fakeident-server/ident-proto.h"
#endif
#if ENABLE_PROG_SERVER
# include "prog-server/prog-server.h"
#endif

/* 
 * Initialize all static server definitions. 
 */
void
init_server_definitions (void)
{
  svz_servertype_add (&foo_server_definition);
#if ENABLE_AWCS_PROTO
  svz_servertype_add (&awcs_server_definition);
#endif
#if ENABLE_HTTP_PROTO
  svz_servertype_add (&http_server_definition);
#endif
#if ENABLE_IRC_PROTO
  svz_servertype_add (&irc_server_definition);
#endif
#if ENABLE_CONTROL_PROTO
  svz_servertype_add (&ctrl_server_definition);
#endif
#if ENABLE_SNTP_PROTO
  svz_servertype_add (&sntp_server_definition);
#endif
#if ENABLE_GNUTELLA
  svz_servertype_add (&nut_server_definition);
#endif
#if ENABLE_TUNNEL
  svz_servertype_add (&tnl_server_definition);
#endif
#if ENABLE_FAKEIDENT
  svz_servertype_add (&fakeident_server_definition);
#endif
#if ENABLE_PROG_SERVER
  svz_servertype_add (&prog_server_definition);
#endif
}
