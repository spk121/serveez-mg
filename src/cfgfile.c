/*
 * cfgfile.c - configuration file and left overs
 *
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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

#include <config.h>
#include "libserveez.h"
#include "cfgfile.h"

/*
 * Include headers of servers.
 */
#include "http-server/http-proto.h"
#include "ctrl-server/control-proto.h"
#include "prog-server/prog-server.h"

/*
 * Initialize all static server definitions. 
 */
void
init_server_definitions (void)
{
  svz_servertype_add (&http_server_definition);
  svz_servertype_add (&ctrl_server_definition);
  svz_servertype_add (&prog_server_definition);
}
