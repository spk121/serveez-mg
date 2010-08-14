/*
 * irc-server.c - IRC server connection routines
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
 * $Id: irc-server.c,v 1.26 2001/10/25 10:15:25 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_IRC_PROTO

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>

#ifdef __MINGW32__
# include <winsock2.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <netinet/in.h>
# include <netdb.h>
# include <arpa/inet.h>
#endif

#include "libserveez.h"
#include "irc-proto.h"
#include "irc-event.h"
#include "irc-server.h"

#define DEFAULT_PORT 6667

irc_server_t *irc_server_list;  /* server list root */

#define MAX_HOST_LEN 256
#define MAX_PASS_LEN 256

/*
 * Parse one of the config lines in the IRC configuration.
 * This function has exactly the same syntax as sscanf() but
 * recognizes only %s and %d for string and integers. Strings
 * will be parsed until the next character in the format string.
 */
int
irc_parse_line (char *line, char *fmt, ...)
{
  va_list args;
  int *i;
  char *s;
  int ret;

  va_start (args, fmt);
  ret = 0;

  while (*fmt && *line)
    {
      /* next arg */
      if (*fmt == '%')
	{
	  /* check if this is a valid format identifier */
	  if (!*++fmt)
	    break;

	  /* a decimal */
	  if (*fmt == 'd')
	    {
	      i = va_arg (args, int *);
	      *i = 0;
	      fmt++;
	      while (*line && *line >= '0' && *line <= '9')
		{
		  *i *= 10;
		  *i += (*line - '0');
		  line++;
		}
	    }
	  /* a string */
	  else if (*fmt == 's')
	    {
	      s = va_arg (args, char *);
	      fmt++;
	      while (*line && *line != *fmt)
		{
		  *s++ = *line++;
		}
	      *s = 0;
	    }
	  ret++;
	}
      /* not an arg */
      else if (*fmt != *line)
	{
	  break;
	}
      if (*fmt)
	fmt++;
      if (*line)
	line++;
    }

  va_end (args);
  return ret;
}

/*
 * This will be called if a DNS lookup for a remote irc server has
 * been done. Here we connect to this server then. Return non-zero on
 * errors.
 */
int
irc_connect_server (char *ip, irc_server_t *server)
{
  irc_config_t *cfg = server->cfg;
  svz_socket_t *sock;
  irc_client_t **cl;
  irc_channel_t **ch;
  char nicklist[MAX_MSG_LEN];
  int n, i;

  /* check if dns lookup was successful */
  if (!ip)
    {
      svz_log (LOG_ERROR, "irc: cannot connect to %s\n", server->realhost);
      return -1;
    }
  
  /* try connecting */
  server->addr = inet_addr (ip);
  if ((sock = svz_tcp_connect (server->addr, server->port)) == NULL)
    {
      return -1;
    }

  svz_log (LOG_NOTICE, "irc: connecting to %s\n", server->realhost);
  sock->data = server;
  sock->cfg = cfg;
  server->id = sock->id;
  server->connected = 1;
  sock->userflags |= IRC_FLAG_SERVER;
  sock->check_request = irc_check_request;

  /* send initial requests introducing this server */
#ifndef ENABLE_TIMESTAMP
  irc_printf (sock, "PASS %s\n", server->pass);
#else /* ENABLE_TIMESTAMP */
  irc_printf (sock, "PASS %s %s\n", server->pass, TS_PASS);
#endif /* ENABLE_TIMESTAMP */
  irc_printf (sock, "SERVER %s 1 :%s\n", cfg->host, cfg->info);

#if ENABLE_TIMESTAMP
  irc_printf (sock, "SVINFO %d %d %d :%d\n",
	      TS_CURRENT, TS_MIN, 0, time (NULL) + cfg->tsdelta);
#endif /* ENABLE_TIMESTAMP */

  /* now propagate user information to this server */
  if ((cl = (irc_client_t **) svz_hash_values (cfg->clients)) != NULL)
    {
      for (n = 0; n < svz_hash_size (cfg->clients); n++)
	{
#if ENABLE_TIMESTAMP
	  irc_printf (sock, "NICK %s %d %d %s %s %s %s :%s\n",
		      cl[n]->nick, cl[n]->hopcount, cl[n]->since, 
		      irc_client_flag_string (cl[n]), 
		      cl[n]->user, cl[n]->host, cl[n]->server, "EFNet?");
#else /* not ENABLE_TIMESTAMP */
	  irc_printf (sock, "NICK %s\n", cl[n]->nick);
	  irc_printf (sock, "USER %s %s %s %s\n", 
		      cl[n]->user, cl[n]->host, cl[n]->server, cl[n]->real);
	  irc_printf (sock, "MODE %s %s\n", 
		      cl[n]->nick, irc_client_flag_string (cl[n]));
#endif /* not ENABLE_TIMESTAMP */
	}
      svz_hash_xfree (cl);
    }

  /* propagate all channel information to the server */
  if ((ch = (irc_channel_t **) svz_hash_values (cfg->channels)) != NULL)
    {
      for (i = 0; i < svz_hash_size (cfg->channels); i++)
	{
#if ENABLE_TIMESTAMP
	  /* create nick list */
	  for (nicklist[0] = 0, n = 0; n < ch[n]->clients; n++)
	    {
	      if (ch[i]->cflag[n] & MODE_OPERATOR)
		strcat (nicklist, "@");
	      else if (ch[i]->cflag[n] & MODE_VOICE)
		strcat (nicklist, "+");
	      strcat (nicklist, ch[i]->client[n]->nick);
	      strcat (nicklist, " ");
	    }
	}
      /* propagate one channel in one request */
      irc_printf (sock, "SJOIN %d %s %s :%s\n",
		  ch[i]->since, ch[i]->name, irc_channel_flag_string (ch[i]),
		  nicklist);
#else /* not ENABLE_TIMESTAMP */
      for (n = 0; n < ch[i]->clients; n++)
	{
	  irc_printf (sock, ":%s JOIN %s\n", 
		      ch[i]->client[n], ch[i]->name);
	}
      irc_printf (sock, "MODE %s %s\n", 
		  ch[i]->name, irc_channel_flag_string (ch[i]));
#endif /* not ENABLE_TIMESTAMP */

      svz_hash_xfree (ch);
    }
  
  return 0;
}

/*
 * Add an IRC server to the server list.
 */
static irc_server_t *
irc_add_server (irc_config_t *cfg, irc_server_t *server)
{
  server->next = cfg->servers;
  cfg->servers = server;
    
  return cfg->servers;
}

/*
 * Go through all C lines in the IRC server configuration
 * and resolve all hosts.
 */
void
irc_connect_servers (irc_config_t *cfg)
{
  char realhost[MAX_HOST_LEN];
  char pass[MAX_PASS_LEN];
  char host[MAX_NAME_LEN];
  int class, port;
  irc_server_t *ircserver;
  char *cline;
  int n;

  /* any C lines at all ? */
  if (!cfg->CLine)
    return;

  /* go through all C lines */
  svz_array_foreach (cfg->CLine, cline, n)
    {
      /* scan the actual C line */
      irc_parse_line (cline, "C:%s:%s:%s:%d:%d", 
		      realhost, pass, host, &port, &class);
      
      /* create new IRC server structure */
      ircserver = svz_malloc (sizeof (irc_server_t));
      ircserver->port = htons ((unsigned short) port);
      ircserver->class = class;
      ircserver->id = -1;
      ircserver->realhost = svz_malloc (strlen (realhost) + 1);
      strcpy (ircserver->realhost, realhost);
      ircserver->host = svz_malloc (strlen (host) + 1);
      strcpy (ircserver->host, host);
      ircserver->pass = svz_malloc (strlen (pass) + 1);
      strcpy (ircserver->pass, pass);
      ircserver->cfg = cfg;
      ircserver->next = NULL;
      ircserver->connected = 0;
      ircserver->connect = 1;

      /* add this server to the server list */
      svz_log (LOG_NOTICE, "irc: enqueuing %s\n", ircserver->realhost);
      irc_add_server (cfg, ircserver);
      svz_coserver_dns (realhost, irc_connect_server, ircserver, NULL);
    }
}

/*
 * Delete an IRC server of the current list.
 */
static void
irc_del_server (irc_config_t *cfg, irc_server_t *server)
{
  irc_server_t *srv;
  irc_server_t *prev;

  prev = srv = cfg->servers;
  while (srv)
    {
      if (srv == server)
	{
	  svz_free (server->realhost);
	  svz_free (server->host);
	  svz_free (server->pass);
	  if (prev == srv)
	    cfg->servers = server->next;
	  else
	    prev->next = server->next;
	  svz_free (server);
	  return;
	}
      prev = srv;
      srv = srv->next;
    }
}

/*
 * Delete all IRC servers.
 */
void
irc_delete_servers (irc_config_t *cfg)
{
  while (cfg->servers)
    {
      irc_del_server (cfg, cfg->servers);
    }
}

/*
 * Count the amount of currently connected IRC servers.
 */
int
irc_count_servers (irc_config_t *cfg)
{
  irc_server_t *server;
  int n = 0;
  
  for (server = cfg->servers; server; server = server->next)
    {
      if (server->connected)
	n++;
    }

  return n;
}

/*
 * Find an IRC server in the current list.
 */
irc_server_t *
irc_find_server (void)
{
  return NULL;
}

#else /* not ENABLE_IRC_PROTO */

int irc_server_dummy; /* Shut up compiler. */

#endif /* ENABLE_IRC_PROTO */
