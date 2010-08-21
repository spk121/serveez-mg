/*
 * awcs-proto.c - aWCS protocol implementation
 *
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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
 * $Id: awcs-proto.c,v 1.39 2003/06/14 14:57:59 ela Exp $
 *
 */

#include <config.h>

#if ENABLE_AWCS_PROTO

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef __MINGW32__
# include <winsock2.h>
#endif

#ifndef __MINGW32__
# include <netinet/in.h>
#endif

#include "libserveez.h"
#include "awcs-proto.h"

/*
 * The aWCS server instance configuration.
 */
awcs_config_t awcs_config =
{
  NULL,           /* aWCS Master server */
  0,              /* Was Master server detected ? */
  NULL            /* aWCS clients user base hash */
};

/*
 * Definition of the configuration items delivered by the 
 * configuration language.
 */
svz_key_value_pair_t awcs_config_prototype [] =
{
  SVZ_REGISTER_END ()
};

/*
 * The aWCS server definition.
 */
svz_servertype_t awcs_server_definition =
{
  "aWCS server",         /* server description */
  "aWCS",                /* server prefix used in the config file "aWCS?" */
  NULL,                  /* global initialization */
  awcs_init,             /* server instance initialization */
  awcs_detect_proto,     /* protocol detection routine */
  awcs_connect_socket,   /* callback when detected */
  awcs_finalize,         /* server instance finalization */
  NULL,                  /* global finalization */
  NULL,                  /* client info */
  NULL,                  /* server info */
  NULL,                  /* server timer */
  NULL,                  /* server reset */
  NULL,                  /* handle request callback */
  SVZ_CONFIG_DEFINE ("aWCS", awcs_config, awcs_config_prototype)
};

/*
 * These 3 (three) routines are for modifying the client hash key
 * processing. Because we are using unique IDs for identifying aWCS 
 * clients it is not necessary to have character strings here.
 */
static unsigned
awcs_hash_keylen (char *id)
{
  return 4;
}

static int 
awcs_hash_equals (char *id1, char *id2)
{
  return memcmp (id1, id2, 4);
}
 
static unsigned long 
awcs_hash_code (char *id)
{
  unsigned long code = SVZ_UINT32 (id);
  return code;
}

/*
 * Local aWCS server instance initialization routine.
 */
int
awcs_init (svz_server_t *server)
{
  awcs_config_t *cfg = server->cfg;

  /* initialize server instance */
  cfg->master = 0;
  cfg->clients = svz_hash_create (4, NULL);
  cfg->clients->code = awcs_hash_code;
  cfg->clients->keylen = awcs_hash_keylen;
  cfg->clients->equals = awcs_hash_equals;
  cfg->server = NULL;

  return 0;
}

/*
 * Local aWCS server instance finalizer.
 */
int
awcs_finalize (svz_server_t *server)
{
  awcs_config_t *cfg = server->cfg;
  
  svz_hash_destroy (cfg->clients);
  
  return 0;
}

/*
 * Gets called when a nslookup coserver has resolved a IP address
 * for socket SOCK to name and has been identified as an aWCS client.
 */
int
awcs_nslookup_done (char *host, int id, int version)
{
  awcs_config_t *cfg;
  svz_socket_t *sock = svz_sock_find (id, version);

  if (host && sock)
    {
      cfg = sock->cfg;

      if (!cfg->server)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "awcs: no master server (nslookup_done)\n");
#endif
	  return -1;
	}

#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "awcs: sending resolved ip to master\n");
#endif

      if (svz_sock_printf (cfg->server, AWCS_ID_FMT " %d " AWCS_ID_FMT " %s%c",
			   cfg->server->id,
			   STATUS_NSLOOKUP,
			   sock->id,
			   host, '\0'))
	{
	  svz_log (LOG_FATAL, "awcs: master write error\n");
	  svz_sock_schedule_for_shutdown (cfg->server);
	  cfg->server = NULL;
	  awcs_disconnect_clients (cfg);
	  return -1;
	}
    }

  return 0;
}

/*
 * Gets called when a ident coserver has resolved a IP address
 * for socket SOCK to name and has been identified as an aWCS client.
 */
int
awcs_ident_done (char *user, int id, int version)
{
  svz_socket_t *sock = svz_sock_find (id, version);
  awcs_config_t *cfg;

  if (user && sock)
    {
      cfg = sock->cfg;

      if (!cfg->server)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "awcs: no master server (ident_done)\n");
#endif
	  return -1;
	}

#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "awcs: sending identified client to master\n");
#endif

      if (svz_sock_printf (cfg->server, AWCS_ID_FMT " %d " AWCS_ID_FMT " %s%c",
			   cfg->server->id,
			   STATUS_IDENT,
			   sock->id,
			   user, '\0'))
	{
	  svz_log (LOG_FATAL, "awcs: master write error\n");
	  svz_sock_schedule_for_shutdown (cfg->server);
	  cfg->server = NULL;
	  awcs_disconnect_clients (cfg);
	  return -1;
	}
    }

  return 0;
}

/*
 * This is called when a valid aWCS client has been connected.
 * The routine reports this to the Master server and invokes
 * a reverse nslookup and a ident request.
 */
static int
awcs_status_connected (svz_socket_t *sock)
{
  unsigned short port;
  unsigned long addr;
  awcs_config_t *cfg = sock->cfg;

  if (!cfg->server)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "awcs: no master server (status_connected)\n");
#endif
      return -1;
    }

  addr = sock->remote_addr;
  port = sock->remote_port;

#if SVZ_ENABLE_DEBUG
  svz_log (LOG_DEBUG, "awcs: sending connect on socket id %d to master\n",
	   sock->id);
#endif

  if (svz_sock_printf (cfg->server, AWCS_ID_FMT " %d " AWCS_ID_FMT " %s:%u%c",
		       cfg->server->id,
		       STATUS_CONNECT,
		       sock->id,
		       svz_inet_ntoa (addr),
		       htons (port), '\0'))
    {
      svz_log (LOG_FATAL, "awcs: master write error\n");
      svz_sock_schedule_for_shutdown (cfg->server);
      cfg->server = NULL;
      awcs_disconnect_clients (cfg);
      return -1;
    }

  /*
   * Initiate a reverse nslookup and ident request for a real client.
   */
  if (sock != cfg->server)
    {
      svz_coserver_rdns (addr, awcs_nslookup_done, sock->id, sock->version);
      svz_coserver_ident (sock, awcs_ident_done, sock->id, sock->version);
    }

  return 0;
}

/*
 * Send a status message to the master server
 * telling it that client `client' has disconnected. `reason'
 * is an error code telling how the client got disconnected.
 */
static int
awcs_status_disconnected (svz_socket_t *sock, int reason)
{
  awcs_config_t *cfg = sock->cfg;

  if (!cfg->server)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "awcs: no master server (status_disconnected)\n");
#endif
      return -1;
    }

#if SVZ_ENABLE_DEBUG
  svz_log (LOG_DEBUG, "awcs: sending disconnect on socket %d to master\n",
	   sock->sock_desc);
#endif /* SVZ_ENABLE_DEBUG */

  if (svz_sock_printf (cfg->server, AWCS_ID_FMT " %d " AWCS_ID_FMT " %d%c", 
		       cfg->server->id,
		       STATUS_DISCONNECT,
		       sock->id, 
		       reason, '\0'))
    {
      svz_log (LOG_FATAL, "awcs: master write error\n");
      svz_sock_schedule_for_shutdown (cfg->server);
      cfg->server = NULL;
      awcs_disconnect_clients (cfg);
      return -1;
    }
  
  return 0;
}

/*
 * Send a status message to the master server
 * telling it that client `client' has been kicked. `reason'
 * is an error code telling why the client got kicked.
 */
static int
awcs_status_kicked (svz_socket_t *sock, int reason)
{
  awcs_config_t *cfg = sock->cfg;
  
  if (!cfg->server)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "awcs: no master server (status_kicked)\n");
#endif
      return -1;
    }

  if (svz_sock_printf (cfg->server, AWCS_ID_FMT " %d " AWCS_ID_FMT " %d%c",
		       cfg->server->id,
		       STATUS_KICK,
		       sock->id, 
		       reason, '\0'))
    {
      svz_log (LOG_FATAL, "awcs: master write error\n");
      svz_sock_schedule_for_shutdown (cfg->server);
      cfg->server = NULL;
      awcs_disconnect_clients (cfg);
      return -1;
    }

  return 0;
}

/*
 * Send a message to the master server indicating this server
 * is still alive.
 */
static int
awcs_status_alive (awcs_config_t *cfg)
{
  if (!cfg->server)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "awcs: no master server (status_alive)\n");
#endif /* SVZ_ENABLE_DEBUG */
      return -1;
    }

  if (svz_sock_printf (cfg->server, AWCS_ID_FMT " %d 42%c",
		       cfg->server->id,
		       STATUS_ALIVE, '\0'))
    {
      svz_log (LOG_FATAL, "awcs: master write error\n");
      svz_sock_schedule_for_shutdown (cfg->server);
      cfg->server = NULL;
      awcs_disconnect_clients (cfg);
      return -1;
    }
  
  return 0;
}

static int
awcs_status_notify (awcs_config_t *cfg) 
{
  if (!cfg->server)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "awcs: no master server (status_notify)\n");
#endif /* SVZ_ENABLE_DEBUG */
      return -1;
    }

  if (svz_sock_printf (cfg->server, AWCS_ID_FMT " %d 42%c",
		       cfg->server->id,
		       STATUS_NOTIFY, '\0'))
    {
      svz_log (LOG_FATAL, "awcs: master write error\n");
      svz_sock_schedule_for_shutdown (cfg->server);
      cfg->server = NULL;
      awcs_disconnect_clients (cfg);
      return -1;
    }

  return 0;
}

/*
 * Broadcast message `cmd' to all connected aWCS clients.
 */
static int
awcs_process_broadcast (awcs_config_t *cfg, char *cmd, int cmd_len)
{
  svz_socket_t **sock;
  int n;
  
#if SVZ_ENABLE_DEBUG
  svz_log (LOG_DEBUG, "awcs: broadcasting\n");
#endif /* SVZ_ENABLE_DEBUG */

  if ((sock = (svz_socket_t **) svz_hash_values (cfg->clients)) != NULL)
    {
      for (n = 0; n < svz_hash_size (cfg->clients); n++)
	{
	  if (svz_sock_write (sock[n], cmd, cmd_len))
	    {
	      svz_sock_schedule_for_shutdown (sock[n]);
	    }
	}
      svz_hash_xfree (sock);
    }
  return 0;
}

/*
 * This macro parse an aWCS id at the given memory location @var{ptr}. The
 * result will be stored in @var{id} and @var{ptr} points to the following
 * non numerical byte afterwards.
 */
#define awcs_process_id(id, ptr)           \
  (id) = 0;                                \
  while (*(ptr) >= '0' && *(ptr) <= '9') { \
    (id) *= 10;                            \
    (id) += *(ptr) - '0';                  \
    (ptr)++; }                             \
  if (*(ptr) == ',') (ptr)++

/*
 * Send message @var{cmd} to all clients in the client list at the start 
 * of @var{cmd}.
 */
static int
awcs_process_multicast (awcs_config_t *cfg, char *cmd, int cmd_len)
{
  char *msg;
  int address;
  svz_socket_t *sock;

  /* Parse the actual message first. */
  msg = cmd;
  while (*msg++ != ' ');
  cmd_len -= (msg - cmd);

  /* Go through the client list. */
  while (*cmd && *cmd != ' ')
    {
      awcs_process_id (address, cmd);
      sock = (svz_socket_t *) svz_hash_get (cfg->clients, (char *) &address);
      if (sock)
	{
	  if (svz_sock_write (sock, msg, cmd_len))
	    svz_sock_schedule_for_shutdown (sock);
	}
#if SVZ_ENABLE_DEBUG
      else
	{
	  svz_log (LOG_DEBUG, "awcs: master sent invalid id (multicast %d)\n", 
		   address);
	}
#endif /* SVZ_ENABLE_DEBUG */
    }
  return 0;
}

/*
 * Process a status request.
 */
static int
awcs_process_status (awcs_config_t *cfg, char *cmd, int cmd_len)
{
  svz_log (LOG_DEBUG, "awcs: sending status message\n");
  awcs_status_alive (cfg);
  return 0;
}

/*
 * Kick all clients in the client list at the beginning of @var{cmd}. 
 * The rest of @var{cmd} is the kicking reason.
 */
static int
awcs_process_kick (awcs_config_t *cfg, char *cmd, int cmd_len)
{
  svz_socket_t *sock;
  int address;

  while (*cmd && *cmd != ' ')
    {
      awcs_process_id (address, cmd);
      sock = (svz_socket_t *) svz_hash_get (cfg->clients, (char *) &address);
      if (sock)
	{
	  svz_log (LOG_DEBUG, "awcs: kicking socket %d\n", sock->sock_desc);
	  /*
	   * This is a hack. We set the handler to NULL to be sure
	   * that the master server will not get a KICKED status
	   * message for a kick he initiated.
	   */
	  sock->kicked_socket = NULL;
	  svz_sock_schedule_for_shutdown (sock);
	}
      else
	{
	  svz_log (LOG_DEBUG, "awcs: master sent invalid id (kick %d)\n",
		   address);
	}
    }
  return 0;
}

/*
 * Turn off flood protection for clients listed in command if flag is 
 * false, turn it on otherwise.
 */
static int
awcs_process_floodcmd (awcs_config_t *cfg, char *cmd, int cmd_len, int flag)
{
  svz_socket_t *sock;
  int address;

  while (*cmd && *cmd != ' ')
    {
      awcs_process_id (address, cmd);
      sock = (svz_socket_t *) svz_hash_get (cfg->clients, (char *) &address);
      if (sock)
	{
	  svz_log (LOG_DEBUG, "awcs: switching flood control for socket "
		   "%d %s\n", sock->sock_desc, flag ? "on" : "off");
	  if (flag)
	    sock->flags &= ~SOCK_FLAG_NOFLOOD;
	  else
	    sock->flags |= SOCK_FLAG_NOFLOOD;
	}
      else
	{
	  svz_log (LOG_DEBUG, "awcs: master sent invalid id (floodcmd %d)\n",
		   address);
	}
    }
  return 0;
}

/*
 * Handle a request from the master server.  Return a non-zero value
 * on error.
 */
static int
awcs_handle_master_request (awcs_config_t *cfg, char *request, int request_len)
{
  if (request_len <= 0)
    return -1;

#if 0
  svz_hexdump (stdout, "master request", cfg->server->sock_desc,
	       request, request_len, 0);
#endif

  request_len--;
  switch (*request++)
    {
    case '0':
      request++;
      request_len--;
      awcs_process_broadcast (cfg, request, request_len);
      break;
    case '1':
      request++;
      request_len--;
      awcs_process_multicast (cfg, request, request_len);
      break;
    case '2':
      request++;
      request_len--;
      awcs_process_status (cfg, request, request_len);
      break;
    case '3':
      request++;
      request_len--;
      awcs_process_kick (cfg, request, request_len);
      break;
    case '4':
      request++;
      request_len--;
      awcs_process_floodcmd (cfg, request, request_len, 0);
      break;
    case '5':
      request++;
      request_len--;
      awcs_process_floodcmd (cfg, request, request_len, 1);
      break;
    case '6':
      /* The following code should not be executed anymore. */
      svz_log (LOG_NOTICE, "awcs: skipping '6' ...\n");
      break;
    default:
      svz_log (LOG_ERROR, "awcs: bad master server request\n");
      break;
    }
  return 0;
}

/*
 * Schedule all aWCS clients for shutdown. Call this if the
 * connection to the master server has been lost.
 */
void
awcs_disconnect_clients (awcs_config_t *cfg)
{
  svz_socket_t **sock;
  int n;

  if ((sock = (svz_socket_t **) svz_hash_values (cfg->clients)) != NULL)
    {
      for (n = 0; n < svz_hash_size (cfg->clients); n++)
	svz_sock_schedule_for_shutdown (sock[n]);
      svz_hash_xfree (sock);
    }
}

/*
 * Handle the request REQUEST of length REQUEST_LEN from socket
 * SOCK.  Return a non-zero value on error.
 */
static int
awcs_handle_request (svz_socket_t *sock, char *request, int request_len)
{
  int ret;
  awcs_config_t *cfg = sock->cfg;

#if 0
  svz_hexdump (stdout, "awcs request", sock->sock_desc, request, 
	       request_len, 1000);
#endif

  if (!cfg->server)
    {
      svz_log (LOG_DEBUG, "awcs: no master server (awcs_handle_request)\n");
      awcs_disconnect_clients (cfg);
      return -1;
    }

  if (sock == cfg->server)
    {
      awcs_handle_master_request (cfg, request, request_len);
    }
  else
    {
      ret = svz_sock_printf (cfg->server, AWCS_ID_FMT " ", sock->id);
      if (ret == 0)
	{
	  ret = svz_sock_write (cfg->server, request, request_len);
	}
      if (ret)
	{
	  svz_log (LOG_FATAL, "awcs: master write error\n");
	  svz_sock_schedule_for_shutdown (cfg->server);
	  cfg->server = NULL;
	  awcs_disconnect_clients (cfg);
	  return -1;
	}
    }
  return 0;
}

/*
 * Checks whether a complete request has been accumulated in socket
 * @var{sock}'s receive queue.  If yes, then the request gets handled 
 * and removed from the queue.
 */
int
awcs_check_request (svz_socket_t *sock)
{
  int retval = 0;
  int request_len = 0;
  char * p, * packet;

  p = sock->recv_buffer;
  packet = p;

  do
    {
      while (p < sock->recv_buffer + sock->recv_buffer_fill && *p != '\0')
        p++;

      if (*p == '\0' && p < sock->recv_buffer + sock->recv_buffer_fill)
        {
          p++;
          request_len += (p - packet);
	  retval = awcs_handle_request (sock, packet, p - packet);
          packet = p;
        }
    }
  while (p < sock->recv_buffer + sock->recv_buffer_fill);
  
  if (request_len > 0)
    {
      memmove (sock->recv_buffer, packet,
	       sock->recv_buffer_fill - request_len);
    }
  sock->recv_buffer_fill -= request_len;

  return 0;
}

/*
 * Gets called when a client has connected as socket SOCK and has been
 * identified as an aWCS client. MASTER ist set if it was a master
 * server detection.
 */
int
awcs_connect_socket (svz_server_t *server, svz_socket_t *sock)
{
  awcs_config_t *cfg = server->cfg;

  /*
   * Don't allow clients without master server or multiple master servers.
   */
  if ((!cfg->server && !cfg->master) || (cfg->server && cfg->master))
    {
      if (!cfg->server && !cfg->master)
	{
	  svz_log (LOG_DEBUG,
		   "awcs: master not present, cannot connect socket %d\n",
		   sock->sock_desc);
	}
      if (cfg->server && cfg->master)
	{
	  svz_log (LOG_DEBUG,
		   "awcs: master is present, cannot connect socket %d\n",
		   sock->sock_desc);
	}

      return -1;
    }

  if (sock->flags & SOCK_FLAG_PIPE)
    {
      svz_log (LOG_DEBUG, "awcs: connection on pipe (%d-%d)\n",
	       sock->pipe_desc[READ], sock->pipe_desc[WRITE]);
    }
  else 
    {
      svz_log (LOG_DEBUG, "awcs: connection on socket %d\n",
	       sock->sock_desc);
    }

  sock->disconnected_socket = awcs_disconnected_socket;
  sock->check_request = awcs_check_request;

  if (cfg->master)
    {
      if (sock->flags & SOCK_FLAG_PIPE)
	{
	  svz_log (LOG_NOTICE, 
		   "awcs: master server connected on pipe (%d-%d)\n",
		   sock->pipe_desc[READ], sock->pipe_desc[WRITE]);
	}
      else
	{
	  svz_log (LOG_NOTICE, 
		   "awcs: master server connected on socket %d\n",
		   sock->sock_desc);
	}

      cfg->server = sock;
      sock->idle_func = awcs_idle_func;
      sock->idle_counter = 3;
      sock->flags |= (SOCK_FLAG_NOFLOOD | SOCK_FLAG_PRIORITY);
      svz_log (LOG_NOTICE,
	       "awcs: resizing master buffers: %d, %d\n",
	       MASTER_SEND_BUFSIZE, MASTER_RECV_BUFSIZE);
      svz_sock_resize_buffers (sock, MASTER_SEND_BUFSIZE, MASTER_RECV_BUFSIZE);
      sock->flags |= SOCK_FLAG_INITED;
    }
  else
    {
      svz_hash_put (cfg->clients, (char *) &sock->id, sock);
      sock->kicked_socket = awcs_kicked_socket;
    }

  /*
   * Tell the master server about the connection (even itself).
   */
  awcs_status_connected (sock);
  return 0;
}

/*
 * Gets called when the socket SOCK had disconnected.  All clients are
 * kicked when the socket was the master server, otherwise a diconnection
 * message is sent to the master server.
 */
int
awcs_disconnected_socket (svz_socket_t *sock)
{
  awcs_config_t *cfg = sock->cfg;
  
  if (sock == cfg->server)
    {
      svz_log (LOG_ERROR, "awcs: lost master server\n");
      cfg->server = NULL;
      awcs_disconnect_clients (cfg);
    }
  else
    {
      awcs_status_disconnected (sock, 1);
      svz_hash_delete (cfg->clients, (char *) &sock->id);
    }

  return 0;
}

/*
 * Gets called when the socket SOCK got kicked.  REASON is true when
 * a output buffer overflow occurred and false, when it was an input
 * buffer overflow or flooding.
 */
int
awcs_kicked_socket (svz_socket_t *sock, int reason)
{
  if (!reason)
    awcs_status_kicked (sock, KICK_FLOODING);
  else
    awcs_status_kicked (sock, KICK_CRAWLING);

  return 0;
}

/*
 * The socket for the master server gets set this function as the timer
 * function.  The function sends an idle message to the master server
 * and resets its timeout counter.
 */
int
awcs_idle_func (svz_socket_t *sock)
{
  awcs_config_t *cfg = sock->cfg;

  if (sock == cfg->server)
    {
      sock->idle_counter = 3;
      awcs_status_notify (cfg);
    }
  return 0;
}

/*
 * This function gets called for new sockets which are not yet
 * identified.  It returns a non-zero value when the contents in
 * the receive buffer looks like an aWCS identification request.
 */
int
awcs_detect_proto (svz_server_t *server, svz_socket_t *sock)
{
  awcs_config_t *cfg = server->cfg;
  int len = 0;

#if 0
  svz_hexdump (stdout, "detecting awcs", sock->sock_desc,
	       sock->recv_buffer, sock->recv_buffer_fill, 0);
#endif

  if (sock->recv_buffer_fill >= MASTER_DETECTION && 
      !memcmp (sock->recv_buffer, AWCS_MASTER, MASTER_DETECTION))
    {
      cfg->master = 1;
      svz_log (LOG_NOTICE, "awcs: master detected\n");
      len = MASTER_DETECTION;
    }
  else if (sock->recv_buffer_fill >= CLIENT_DETECTION && 
	   !memcmp (sock->recv_buffer, AWCS_CLIENT, CLIENT_DETECTION))
    {
      cfg->master = 0;
      svz_log (LOG_NOTICE, "awcs: client detected\n");
      len = CLIENT_DETECTION;
    }

  if (len == 0) return 0;

  if (sock->recv_buffer_fill > len)
    {
      memmove (sock->recv_buffer, 
	       sock->recv_buffer + len, 
	       sock->recv_buffer_fill - len);
    }
  sock->recv_buffer_fill -= len;
  return len;
}

int have_awcs = 1;

#else /* ENABLE_AWCS_PROTO */

int have_awcs = 0;	/* Shut compiler warnings up,
			   make runtime checking possible */

#endif /* not ENABLE_AWCS_PROTO */
