/*
 * nut-hostlist.c - gnutella host list implementation
 *
 * Copyright (C) 2000, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: nut-hostlist.c,v 1.10 2003/06/14 14:58:00 ela Exp $
 *
 */

#include <config.h>

#if ENABLE_GNUTELLA

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>


#include "libserveez.h"
#include "gnutella.h"
#include "nut-core.h"
#include "nut-hostlist.h"

/*
 * This routine is the write_socket callback when delivering the 
 * host catcher list. It just waits until the whole HTML has been
 * successfully sent and closes the connection afterwards.
 */
int
nut_hosts_write (svz_socket_t *sock)
{
  int num_written;

  /* write as much data as possible */
  num_written = send (sock->sock_desc, sock->send_buffer,
                      sock->send_buffer_fill, 0);

  /* some data has been written */
  if (num_written > 0)
    {
      sock->last_send = time (NULL);

      /* reduce send buffer */
      if (sock->send_buffer_fill > num_written)
        {
          memmove (sock->send_buffer, 
                   sock->send_buffer + num_written,
                   sock->send_buffer_fill - num_written);
        }
      sock->send_buffer_fill -= num_written;
    }
  /* seems like an error */
  else if (num_written < 0)
    {
      svz_log (LOG_ERROR, "nut: send: %s\n", NET_ERROR);
      if (errno == EAGAIN)
        {
          sock->unavailable = time (NULL) + RELAX_FD_TIME;
          num_written = 0;
        }
    }
  
  /* has all data been sent successfully ? */
  if (sock->send_buffer_fill <= 0 && !(sock->userflags & NUT_FLAG_HOSTS))
    {
      num_written = -1;
    }

  return (num_written < 0) ? -1 : 0;
}

/*
 * This is the check_request callback for the HTML host list output.
 */
int
nut_hosts_check (svz_socket_t *sock)
{
  nut_config_t *cfg = sock->cfg;
  nut_host_t **host;
  int n, t, day, hour, min, sec, now;

  /* do not enter this routine if you do not want to send something */
  if (!(sock->userflags & NUT_FLAG_HOSTS))
    return 0;

  /* send normal HTTP header */
  if (svz_sock_printf (sock, NUT_HTTP_HEADER) == -1)
    return -1;

  /* send HTML header */
  if (svz_sock_printf (sock, NUT_HTML_HEADER, svz_hash_size (cfg->net)) == -1)
    return -1;

  /* go through all caught gnutella hosts and print their info */
  if ((host = (nut_host_t **) svz_hash_values (cfg->net)) != NULL)
    {
      now = time (NULL);
      for (n = 0; n < svz_hash_size (cfg->net); n++)
	{
	  if (sock->send_buffer_fill > (NUT_SEND_BUFSIZE - 256))
	    {
	      /* send buffer queue overrun ... */
	      if (svz_sock_printf (sock, ".\n.\n.\n") == -1)
		{
		  svz_hash_xfree (host);
		  return -1;
		}
	    }
	  else
	    {
	      /* usual gnutella host output */
	      t = now - host[n]->last_reply;
	      day = t / (3600 * 24);
	      t %= (3600 * 24);
	      hour = t / 3600;
	      t %= 3600;
	      min = t / 60;
	      t %= 60;
	      sec = t;
	      if (svz_sock_printf (sock, "%-22s %d days %d:%02d:%02d\n",
				   nut_client_key (host[n]->ip, host[n]->port),
				   day, hour, min, sec) == -1)
		{
		  svz_hash_xfree (host);
		  return -1;
		}
	    }
	}
      svz_hash_xfree (host);
    }

  /* send HTML footer */
  if (svz_sock_printf (sock, NUT_HTML_FOOTER,
		       svz_library, svz_version,
		       svz_inet_ntoa (sock->local_addr), 
		       ntohs (sock->local_port)) == -1)
    return -1;

  /* state that we have sent all available data */
  sock->userflags &= ~NUT_FLAG_HOSTS;

  /* shutdown the socket if all data has been written */
  if (sock->send_buffer_fill <= 0)
    return -1;

  return 0;
}

/*
 * Within this routine we collect all available gnutella hosts. Thus
 * we might never ever lack of gnutella net connections. IP and PORT
 * must be both in network byte order.
 */
int
nut_host_catcher (svz_socket_t *sock, unsigned long ip, unsigned short port)
{
  nut_host_t *client;
  nut_config_t *cfg = sock->cfg;

  client = (nut_host_t *) svz_hash_get (cfg->net, nut_client_key (ip, port));

  /* not yet in host catcher hash */
  if (client == NULL)
    {
      /* check if it is a valid ip/host combination */
      if ((ip & 0xff000000) == 0 || (ip & 0x00ff0000) == 0 ||
	  (ip & 0x0000ff00) == 0 || (ip & 0x000000ff) == 0 ||
	  (ip & 0xff000000) == 0xff000000 || (ip & 0x00ff0000) == 0x00ff0000 ||
	  (ip & 0x0000ff00) == 0x0000ff00 || (ip & 0x000000ff) == 0x000000ff ||
	  ip == sock->local_addr ||
	  port == 0)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: invalid host: %s:%u\n", 
		   svz_inet_ntoa (ip), ntohs (port));
#endif
	  return -1;
	}

      client = svz_malloc (sizeof (nut_host_t));
      client->last_reply = time (NULL);
      client->ip = ip;
      client->port = port;
      memset (client->id, 0, NUT_GUID_SIZE);
      svz_hash_put (cfg->net, nut_client_key (ip, port), client);
    }
  
  /* just update last seen time stamp */
  else
    {
      client->last_reply = time (NULL);
    }
  return 0;
}

#else /* ENABLE_GNUTELLA */

int nut_hostlist_dummy;	/* Shut compiler warnings up. */

#endif /* not ENABLE_GNUTELLA */
