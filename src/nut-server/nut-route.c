/*
 * nut-route.c - gnutella routing table implementation
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
 * $Id: nut-route.c,v 1.20 2003/06/14 14:58:00 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_GNUTELLA

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef __MINGW32__
# include <winsock2.h>
#endif

#include "libserveez.h"
#include "gnutella.h"
#include "nut-route.h"
#include "nut-core.h"

/*
 * This function canonizes gnutella queries. Thus we prevent the network
 * from unpatient users and often repeated queries.
 */
static int
nut_canonize_query (nut_config_t *cfg, char *query)
{
  char *key, *p, *extract;
  time_t t;
  int ret = 0;

  /* not a valid query ? */
  if (!*query)
    return -1;

  /* extract alphanumerics only and pack them together as lowercase */
  key = extract = p = svz_strdup (query);
  while (*p)
    {
      if (isalnum ((svz_uint8_t) *p)) 
	*extract++ = (char) (isupper ((svz_uint8_t) *p) ? 
			     tolower ((svz_uint8_t) *p) : *p);
      p++;
    }
  *extract = '\0';

  /* check if it is in the recent query hash */
  if ((t = (time_t) (long) svz_hash_get (cfg->query, key)) != 0)
    {
      if (time (NULL) - t < NUT_QUERY_TOO_RECENT)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: dropping too recent query\n");
#endif
	  ret = -1;
	}
    }
  /* put the query extraction to the recent query hash */
  else
    {
      t = time (NULL);
      svz_hash_put (cfg->query, key, (void *) ((long) t));
    }

  svz_free (key);
  return ret;
}

/*
 * Gnutella packet validation. This is absolutely necessary. It protects 
 * the local client as well as offer an additional line of defense against 
 * broadcasting spam to other connections. Return values:
 *  1 = packet ok
 *  0 = packet is dropped, but can be processed
 * -1 = invalid packet, do not process at all
 */
int
nut_validate_packet (svz_socket_t *sock, nut_header_t *hdr, 
		     svz_uint8_t *packet)
{
  nut_config_t *cfg = sock->cfg;
  nut_client_t *client = sock->data;

#if 0
  fprintf (stdout, "validating packet 0x%02X (%s)\n", 
	   hdr->function, nut_print_guid (hdr->id));
#endif /* 1 */

  /* Packet size validation */
  switch (hdr->function)
    {
      /* Ping */
    case NUT_PING_REQ:
      if (hdr->length != 0)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: invalid ping payload\n");
#endif
	  return -1;
	}
      break;
      /* Pong */
    case NUT_PING_ACK:
      if (hdr->length != SIZEOF_NUT_PONG)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: invalid pong payload\n");
#endif
	  return -1;
	}
      break;
      /* Push Request */
    case NUT_PUSH_REQ:
      if (hdr->length != SIZEOF_NUT_PUSH)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: invalid push request payload\n");
#endif
	  return -1;
	}
      break;
      /* Query */
    case NUT_SEARCH_REQ:
      if (hdr->length > 257)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: payload of query too big\n");
#endif
	  return -1;
	}
      if (nut_canonize_query (cfg, (char *) packet + SIZEOF_NUT_QUERY) == -1)
	return -1;
      break;
      /* Query hits */
    case NUT_SEARCH_ACK:
      if (hdr->length > (SIZEOF_NUT_RECORD + 256) * 256)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: payload of query hits too big\n");
#endif
	  return -1;
	}
      break;
      /* Invalid */
    default:
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "nut: invalid request 0x%02X\n", hdr->function);
#endif
      if (client->invalid++ > NUT_INVALID_PACKETS)
	svz_sock_schedule_for_shutdown (sock);
      return -1;
    }

  /* Hops and TTLs */
  if (hdr->ttl <= 1)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "nut: packet died (zero TTL)\n", hdr->function);
#endif
      return 0;
    }

  hdr->hop++;
  hdr->ttl--;

  if (hdr->hop > cfg->max_ttl)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "nut: packet died (HOP > MaxTTL)\n",
	       hdr->function); 
#endif
      return 0;
    }

  if (hdr->ttl > 50)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "nut: packet dropped (TTL > 50)\n");
#endif
      return 0;
    }

  if (hdr->ttl > cfg->max_ttl)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "nut: decreasing packet TTL (%d -> %d)\n",
	       hdr->ttl, cfg->max_ttl);
#endif
      hdr->ttl = (svz_uint8_t) cfg->max_ttl;
    }

  if (hdr->ttl + hdr->hop > cfg->max_ttl)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "nut: decreasing packet TTL (%d -> %d)\n",
	       hdr->ttl, cfg->max_ttl - hdr->hop);
#endif
      hdr->ttl = (svz_uint8_t) (cfg->max_ttl - hdr->hop);
    }

  return 1;
}

/*
 * This is the routing routine for any incoming gnutella packet.
 * It return non-zero on routing errors and packet death. Otherwise
 * zero.
 */
int
nut_route (svz_socket_t *sock, nut_header_t *hdr, svz_uint8_t *packet)
{
  nut_config_t *cfg = sock->cfg;
  nut_packet_t *pkt;
  svz_socket_t *xsock;
  svz_socket_t **conn;
  svz_uint8_t *header;
  int n;

  /* packet validation */
  if ((n = nut_validate_packet (sock, hdr, packet)) == -1)
    return -1;
  else if (n == 0)
    return 0;

  /* route replies here */
  if (hdr->function & 0x01)
    {
      /* is the GUID in the routing hash ? */
      xsock = (svz_socket_t *) svz_hash_get (cfg->route, (char *) hdr->id);
      if (xsock == NULL)
	{
	  pkt = (nut_packet_t *) svz_hash_get (cfg->packet, (char *) hdr->id);
	  if (pkt == NULL)
	    {
	      svz_log (LOG_ERROR, "nut: error routing packet 0x%02X\n",
		       hdr->function);
	      cfg->errors++;
	      return -1;
	    }
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: packet 0x%02X reply received\n",
		   hdr->function);
#endif
	}
      /* yes, send it to the connection the original query came from */
      else
	{
	  /* try sending the header */
	  header = nut_put_header (hdr);
	  if (svz_sock_write (xsock, (char *) header, SIZEOF_NUT_HEADER) == -1)
	    {
	      svz_sock_schedule_for_shutdown (xsock);
	      return 0;
	    }
	  /* send the packet body if necessary */
	  if (hdr->length)
	    {
	      if (svz_sock_write (xsock, (char *) packet, hdr->length) == -1)
		{
		  svz_sock_schedule_for_shutdown (xsock);
		  return 0;
		}
	    }
	}
    }
  /* 
   * route queries here (hdr->function & 0x01 == 0x00), push request 
   * will be handle later.
   */
  else if (hdr->function != NUT_PUSH_REQ)
    {
      /* check if this query has been seen already */
      xsock = (svz_socket_t *) svz_hash_get (cfg->route, (char *) hdr->id);
      if (xsock != NULL)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: dropping duplicate packet 0x%02X\n",
		   hdr->function);
#endif
	  return -1;
	}

      /* check if this query has been sent by ourselves */
      pkt = (nut_packet_t *) svz_hash_get (cfg->packet, (char *) hdr->id);
      if (pkt != NULL)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "nut: dropping native packet 0x%02X\n",
		   hdr->function);
#endif
	  return -1;
	}

      /* add the query to routing table */
      svz_hash_put (cfg->route, (char *) hdr->id, sock);

      /* 
       * Forward this query to all connections except the connection
       * the server got it from.
       */
      if ((conn = (svz_socket_t **) svz_hash_values (cfg->conn)) != NULL)
	{
	  header = nut_put_header (hdr);
	  for (n = 0; n < svz_hash_size (cfg->conn); n++)
	    {
	      xsock = conn[n];
	      if (xsock == sock)
		continue;
	      if (svz_sock_write (xsock, (char *) header, 
				  SIZEOF_NUT_HEADER) == -1)
		{
		  svz_sock_schedule_for_shutdown (xsock);
		  svz_hash_xfree (conn);
		  return 0;
		}
	      if (hdr->length)
		{
		  if (svz_sock_write (xsock, (char *) packet, 
				      hdr->length) == -1)
		    {
		      svz_sock_schedule_for_shutdown (xsock);
		      svz_hash_xfree (conn);
		      return 0;
		    }
		}
	    }
	  svz_hash_xfree (conn);
	}
    }
  return 0;
}

#else /* ENABLE_GNUTELLA */

int nut_route_dummy; /* Shut compiler warnings up. */

#endif /* not ENABLE_GNUTELLA */
