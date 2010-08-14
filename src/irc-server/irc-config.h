/*
 * irc-config.c - IRC server configuration definitions
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
 * $Id: irc-config.h,v 1.3 2000/09/08 07:45:17 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#ifndef __IRC_CONFIG_H__
#define __IRC_CONFIG_H__

/*
 * Export these routines.
 */
void irc_parse_config_lines (irc_config_t *cfg);
void irc_free_config_lines (irc_config_t *cfg);
int irc_client_valid (irc_client_t *client, irc_config_t *cfg);
int irc_oper_valid (irc_client_t *client, irc_config_t *cfg);

#endif /* __IRC_CONFIG_H__ */
