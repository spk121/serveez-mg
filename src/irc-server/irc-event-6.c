/*
 * irc-event-6.c - IRC events -- Miscellaneous messages
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
 * $Id: irc-event-6.c,v 1.10 2001/05/19 23:04:57 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_IRC_PROTO

#define _GNU_SOURCE
#include <string.h>
#include <stdlib.h>

#ifdef __MINGW32__
# include <winsock2.h>
#endif

#include <libserveez.h>
#include "irc-core/irc-core.h"
#include "irc-proto.h"
#include "irc-event.h"

/*
 *         Command: PING
 *      Parameters: <server1> [<server2>]
 * Numeric Replies: ERR_NOORIGIN  ERR_NOSUCHSERVER
 */
int
irc_ping_callback (svz_socket_t *sock, 
		   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  int n;

  /* ping origin given ? */
  if (request->paras < 1)
    {
      irc_printf (sock, ":%s %03d %s " ERR_NOORIGIN_TEXT "\n",
		  cfg->host, ERR_NOORIGIN, client->nick);
      return 0;
    }

  /* go through all paras and respond to the ping */
  for (n = 0; n < request->paras; n++)
    {
      irc_printf (sock, "PONG %s\n", request->para[n]);
    }

  return 0;
}

/*
 *         Command: PONG
 *      Parameters: <daemon> [<daemon2>]
 * Numeric Replies: ERR_NOORIGIN ERR_NOSUCHSERVER
 */
int
irc_pong_callback (svz_socket_t *sock, 
		   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  int n;

  /* pong origin given ? */
  if (request->paras < 1)
    {
      irc_printf (sock, ":%s %03d %s " ERR_NOORIGIN_TEXT "\n",
		  cfg->host, ERR_NOORIGIN, client->nick);
      return 0;
    }

  /* go through all targets */
  for (n = 0; n < request->paras; n++)
    {
      /* is the server origin valid ? */
      if (strcmp (request->para[n], cfg->host))
	{
	  irc_printf (sock, ":%s %03d %s " ERR_NOSUCHSERVER_TEXT "\n",
		      cfg->host, ERR_NOSUCHSERVER, client->nick,
		      request->para[n]);
	  return 0;
	}
      /* yes, count the ping reply */
      client->ping = 0;
    }
  return 0;
}

/*
 * 
 *    Command: ERROR
 * Parameters: <error message>
 */
int
irc_error_callback (svz_socket_t *sock, 
		    irc_client_t *client, irc_request_t *request)
{
  svz_log (LOG_ERROR, "irc: %s\n", request->para[0]);
  return 0;
}

/*
 *         Command: KILL
 *      Parameters: <nickname> <comment>
 * Numeric Replies: ERR_NOPRIVILEGES  ERR_NEEDMOREPARAMS
 *                  ERR_NOSUCHNICK    ERR_CANTKILLSERVER
 */
int
irc_kill_callback (svz_socket_t *sock, 
		   irc_client_t *client, irc_request_t *request)
{
  irc_config_t *cfg = sock->cfg;
  irc_client_t *cl;

  /* do you have enough paras ? */
  if (irc_check_args (sock, client, cfg, request, 2))
    return 0;

  /* are you an IRC operator ? */
  if (client && !(client->flag & UMODE_OPERATOR))
    {
      irc_printf (sock, ":%s %03d %s " ERR_NOPRIVILEGES_TEXT "\n",
		  cfg->host, ERR_NOPRIVILEGES, client->nick);
      return 0;
    }

  /* find the IRC client */
  if ((cl = irc_find_nick (cfg, request->para[0])) == NULL)
    {
      irc_printf (sock, ":%s %03d " ERR_NOSUCHNICK_TEXT "\n",
		  cfg->host, ERR_NOSUCHNICK, request->para[0]);
      return 0;
    }

  /* delete this client */
  /* TODO: read the RFC for what is happening if a nick collision occurs */
  
  return 0;
}

#else /* not ENABLE_IRC_PROTO */

int irc_event_6_dummy; /* Shut up compiler warnings. */

#endif /* not ENABLE_IRC_PROTO */
