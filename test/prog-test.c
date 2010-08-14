/*
 * test/prog-test.c - program passthrough test program
 *
 * Copyright (C) 2002, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: prog-test.c,v 1.4 2004/03/20 10:43:32 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#if HAVE_FLOSS_H
# include <floss.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef __MINGW32__
# include <sys/types.h>
# include <sys/socket.h>
# include <netinet/in.h>
# include <arpa/inet.h>
# include <netdb.h>
#else
# define sleep(x) Sleep ((x) * 1000)
# include <winsock2.h>
# include <io.h>
#endif

/*
 * Main entry point for test.
 */
int
main (int argc, char **argv)
{
  int s;
  struct sockaddr_in addr;
  socklen_t len = sizeof (struct sockaddr_in);
  char *buf1 = "write(): Hello\r\n";
  char *buf2 = "send(): Hello\r\n";

#ifdef __MINGW32__
  WSADATA WSAData;
  WSAStartup (0x0202, &WSAData);
#endif /* __MINGW32__ */

  fprintf (stderr, "start...\r\n");

  /* Obtain output descriptor. */
#ifdef __MINGW32__
  if (getenv ("SEND_HANDLE") != NULL)
    s = atoi (getenv ("SEND_HANDLE"));
  else
    s = fileno (stdout);
#else
  s = fileno (stdout);
#endif

  /* Determine remote connection. */
  if (getpeername ((svz_t_socket) s, (struct sockaddr *) &addr, &len) < 0)
    {
      fprintf (stderr, "getpeername: %s\n", strerror (errno));
      fflush (stderr);
    }
  else
    {
      /* Try using `fprintf()'. */
      fprintf (stdout, "fprintf(): %s:%d\r\n",
	       inet_ntoa ((* ((struct in_addr *) &addr.sin_addr))),
	       ntohs (addr.sin_port));
      fflush (stdout);
    }

  /* Try using `write()'. */
  if (write (s, buf1, strlen (buf1)) < 0)
    {
      fprintf (stderr, "write: %s\n", strerror (errno));
      fflush (stderr);
    }
  /* Try using `send()'. */
  if (send (s, buf2, strlen (buf2), 0) < 0)
    {
      fprintf (stderr, "send: %s\n", strerror (errno));
      fflush (stderr);
    }

  fflush (stdout);
  sleep (3);

#ifdef __MINGW32__
  shutdown (s, 2);
  closesocket (s);
  WSACleanup();
#endif /* __MINGW32__ */

  fprintf (stderr, "...end\r\n");
  return 0;
}
