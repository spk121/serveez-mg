/*
 * dns.c - DNS lookup coserver implementation
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: dns.c,v 1.7 2003/06/14 14:58:00 ela Exp $
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

#include "util.h"
#include "core.h"
#include "coserver/coserver.h"
#include "coserver/dns.h"

/*
 * Proceed a single DNS lookup. 
 */
char *
dns_handle_request (char *inbuf)
{
  unsigned long addr;
  struct hostent *host;
  static char resolved[COSERVER_BUFSIZE];

  if ((1 == sscanf (inbuf, "%s", resolved)))
    {
      /* find the host by its name */
      if ((host = gethostbyname (resolved)) == NULL)
        {
          svz_log (LOG_ERROR, "dns: gethostbyname: %s (%s)\n", 
		   H_NET_ERROR, resolved);
	  return NULL;
        }

      /* get the inet address in network byte order */
      if (host->h_addrtype == AF_INET)
        {
          memcpy (&addr, host->h_addr_list[0], host->h_length);

#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, "dns: %s is %s\n",
		   host->h_name, svz_inet_ntoa (addr));
#endif /* SVZ_ENABLE_DEBUG */
	  sprintf (resolved, "%s", svz_inet_ntoa (addr));
	  return resolved;
	}
    } 
  else 
    {
      svz_log (LOG_ERROR, "dns: protocol error\n");
      return NULL;
    }
  
  return NULL;
}
