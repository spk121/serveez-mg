/*
 * sntp-proto.c - simple network time protocol implementation
 *
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
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
 * $Id: sntp-proto.c,v 1.12 2002/12/05 16:57:56 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_SNTP_PROTO

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef __MINGW32__
# include <winsock2.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <netinet/in.h>
#endif

#include "libserveez.h"
#include "sntp-proto.h"

/*
 * Simple network time server configuration.
 */
sntp_config_t sntp_config = 
{
  0, /* default nothing */
};

/*
 * Defining configuration file associations with key-value-pairs.
 */
svz_key_value_pair_t sntp_config_prototype [] = 
{
  SVZ_REGISTER_END ()
};

/*
 * Definition of this server.
 */
svz_servertype_t sntp_server_definition =
{
  "Simple Network Time Protocol server",
  "sntp",
  NULL,
  sntp_init,
  sntp_detect_proto,
  sntp_connect_socket,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  sntp_handle_request,
  SVZ_CONFIG_DEFINE ("sntp", sntp_config, sntp_config_prototype)
};

/*
 * Initialize a SNTP server instance.
 */
int
sntp_init (svz_server_t *server)
{
  return 0;
}

/*
 * No protocol detection for TCP/PIPE needed.
 */
int
sntp_detect_proto (svz_server_t *server, svz_socket_t *sock)
{
  return -1;
}

/* Time offset constant. */
#define SNTP_TIME_CONSTANT 2208988800u

/*
 * Produces the SNTP reply and returns the actual size of it.
 */
static int
sntp_create_reply (unsigned char *reply)
{
  unsigned long date;

  struct timeval t;
  gettimeofday (&t, NULL);
  date = htonl (SNTP_TIME_CONSTANT + t.tv_sec);
  memcpy (reply, &date, 4);
  date = htonl (t.tv_usec);
  memcpy (&reply[4], &date, 4);
  return 8;
}

/*
 * Send our reply immediately for TCP/PIPE bindings and schedule this
 * connection for shutdown.
 */
int
sntp_connect_socket (svz_server_t *server, svz_socket_t *sock)
{
  int ret;
  unsigned char reply[8];

  sock->check_request = NULL;

  /* Simple SNTP. */
  if ((ret = sntp_create_reply (reply)) == 4)
    {
      sock->flags |= SOCK_FLAG_FINAL_WRITE;
      return svz_sock_printf (sock, "%c%c%c%c", 
			      reply[0], reply[1], reply[2], reply[3]);
    }

  /* Extended SNTP. */
  svz_sock_printf (sock, "%c%c%c%c", 
		   reply[0], reply[1], reply[2], reply[3]);
  sock->flags |= SOCK_FLAG_FINAL_WRITE;
  return svz_sock_printf (sock, "%c%c%c%c", 
			  reply[4], reply[5], reply[6], reply[7]);
}

/*
 * The packet processor for the SNTP server.
 */
int
sntp_handle_request (svz_socket_t *sock, char *packet, int len)
{
  int ret;
  unsigned char reply[8];

  if ((ret = sntp_create_reply (reply)) == 4)
    {
      svz_udp_printf (sock, "%c%c%c%c", 
		      reply[0], reply[1], reply[2], reply[3]);
      return 0;
    }

  svz_udp_printf (sock, "%c%c%c%c", 
		  reply[0], reply[1], reply[2], reply[3]);
  svz_udp_printf (sock, "%c%c%c%c", 
		  reply[4], reply[5], reply[6], reply[7]);
  return 0;
}

#else /* not ENABLE_SNTP_PROTO */

int sntp_dummy; /* Shut up compiler. */

#endif /* not ENABLE_SNTP_PROTO */
