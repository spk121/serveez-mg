/*
 * udp-socket.c - udp socket implementations
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: udp-socket.c,v 1.19 2003/06/14 14:57:59 ela Exp $
 *
 */

#include <stdio.h>              /* vsnprintf */
#include <string.h>             /* memcpy */
#include <errno.h>              /* EAGAIN, errno */
#include <time.h>               /* time */
#include <stdarg.h>             /* va_list */
#include <unistd.h>             /* close */
#include <sys/socket.h>         /* send, sendto, AF_INET, recvfrom, 
                                   struct sockaddr, socklen_t */
#include <arpa/inet.h>          /* ntohs */

#include "alloc.h"
#include "util.h"
#include "socket.h"
#include "core.h"
#include "server-core.h"
#include "server.h"
#include "portcfg.h"
#include "binding.h"
#include "udp-socket.h"

/*
 * This routine is the default reader for UDP sockets. Whenever the socket
 * descriptor is @code{select()}'ed for reading it is called by default and 
 * reads as much data as possible (whole packets only) and saves the sender 
 * into the @code{sock->remote_addr} field. The packet load is written into 
 * @code{sock->recv_buffer}.
 */
int
svz_udp_read_socket (svz_socket_t *sock)
{
  int do_read, num_read;
  socklen_t len;
  struct sockaddr_in sender;

  len = sizeof (struct sockaddr_in);

  /* Check if there is enough space to save the packet. */
  do_read = sock->recv_buffer_size - sock->recv_buffer_fill;
  if (do_read <= 0)
    {
      svz_log (LOG_ERROR, "receive buffer overflow on udp socket %d\n",
	       sock->sock_desc);
      return -1;
    }
  
  /* Receive data. */
  if (!(sock->flags & SOCK_FLAG_CONNECTED))
    {
      num_read = recvfrom (sock->sock_desc,
			   sock->recv_buffer + sock->recv_buffer_fill,
			   do_read, 0, (struct sockaddr *) &sender, &len);
    }
  else
    {
      num_read = recv (sock->sock_desc,
		       sock->recv_buffer + sock->recv_buffer_fill,
		       do_read, 0);
    }

  /* Valid packet data arrived. */
  if (num_read > 0)
    {
      sock->last_recv = time (NULL);
      sock->recv_buffer_fill += num_read;

      /* Save sender in socket structure. */
      if (!(sock->flags & SOCK_FLAG_FIXED))
	{
	  sock->remote_port = sender.sin_port;
	  sock->remote_addr = sender.sin_addr.s_addr;
	}

      svz_log (LOG_DEBUG, "udp: recv%s: %s:%u (%d bytes)\n",
	       sock->flags & SOCK_FLAG_CONNECTED ? "" : "from",
	       svz_inet_ntoa (sock->remote_addr),
	       ntohs (sock->remote_port), num_read);

      /* Check access lists. */
      if (svz_sock_check_access (sock, sock) < 0)
	return 0;

      /* Handle packet. */
      if (sock->check_request)
        if (sock->check_request (sock))
	  return -1;
    }
  /* Some error occurred. */
  else
    {
      svz_log (LOG_ERROR, "udp: recv%s: %s\n",
	       sock->flags & SOCK_FLAG_CONNECTED ? "" : "from", NET_ERROR);
      if (errno != EAGAIN)
	return -1;
    }
  return 0;
}

/*
 * This routine is the default reader for UDP server sockets. It allocates
 * necessary buffers (that's why it's called lazy) and reverts to the default
 * @code{svz_udp_read_socket()}.
 */
int
svz_udp_lazy_read_socket (svz_socket_t *sock)
{
  svz_portcfg_t *port = sock->port;

  svz_sock_resize_buffers (sock, port->send_buffer_size,
			   port->recv_buffer_size);
  sock->read_socket = svz_udp_read_socket;

  return sock->read_socket (sock);
}

/*
 * The @code{svz_udp_write_socket()} callback should be called whenever 
 * the UDP socket descriptor is ready for sending. It sends a single packet 
 * within the @code{sock->send_buffer} to the destination address specified 
 * by @code{sock->remote_addr} and @code{sock->remote_port}.
 */
int
svz_udp_write_socket (svz_socket_t *sock)
{
  int num_written;
  unsigned do_write;
  char *p;
  socklen_t len;
  struct sockaddr_in receiver;

  /* return here if there is nothing to send */
  if (sock->send_buffer_fill <= 0)
    return 0;

  len = sizeof (struct sockaddr_in);
  receiver.sin_family = AF_INET;

  /* get destination address, port and data length from buffer */
  p = sock->send_buffer;
  memcpy (&do_write, p, sizeof (do_write));
  p += sizeof (do_write);
  memcpy (&receiver.sin_addr.s_addr, p, sizeof (sock->remote_addr));
  p += sizeof (sock->remote_addr);
  memcpy (&receiver.sin_port, p, sizeof (sock->remote_port));
  p += sizeof (sock->remote_port);

  /* if socket is `connect ()' ed use `send ()' instead of `sendto ()' */
  if (!(sock->flags & SOCK_FLAG_CONNECTED))
    {
      num_written = sendto (sock->sock_desc, p,
			    do_write - (p - sock->send_buffer),
			    0, (struct sockaddr *) &receiver, len);
    }
  else
    {
      num_written = send (sock->sock_desc, p,
			  do_write - (p - sock->send_buffer), 0);
    }

  /* some error occurred while sending */
  if (num_written < 0)
    {
      svz_log (LOG_ERROR, "udp: send%s: %s\n", 
	       sock->flags & SOCK_FLAG_CONNECTED ? "" : "to", NET_ERROR);
      if (errno == EAGAIN)
	num_written = 0;
    }
  /* packet data could be transmitted */
  else
    {
      sock->last_send = time (NULL);
      svz_sock_reduce_send (sock, (int) do_write);
    }

  svz_log (LOG_DEBUG, "udp: send%s: %s:%u (%u bytes)\n",
	   sock->flags & SOCK_FLAG_CONNECTED ? "" : "to", 
	   svz_inet_ntoa (receiver.sin_addr.s_addr),
	   ntohs (receiver.sin_port), do_write - (p - sock->send_buffer));

  return num_written < 0 ? -1 : 0;
}

/*
 * This is the default @code{check_request()} routine for UDP servers. 
 * Whenever new data arrived at an UDP server socket we call this function to
 * process the packet data. Any given @code{handle_request()} callback MUST 
 * return zero if it successfully processed the data and non-zero if it 
 * could not.
 */
int
svz_udp_check_request (svz_socket_t *sock)
{
  int n;
  svz_server_t *server;
  svz_array_t *bindings;
  svz_binding_t *binding;

  if (sock->data == NULL && sock->handle_request == NULL)
    return -1;

  /* 
   * If there is a valid `handle_request ()' callback (dedicated udp 
   * connection) call it. This kind of behaviour is due to a socket creation 
   * via `udp_connect ()' and setting up a static `handle_request ()'
   * callback.
   */
  if (sock->handle_request)
    {
      if (sock->handle_request (sock, sock->recv_buffer,
				sock->recv_buffer_fill))
	return -1;
      sock->recv_buffer_fill = 0;
      return 0;
    }

  /* go through all udp servers on this server socket */
  bindings = svz_binding_filter (sock);
  svz_array_foreach (bindings, binding, n)
    {
      server = binding->server;
      sock->cfg = server->cfg;

      if (server->handle_request)
	{
	  if (!server->handle_request (sock, sock->recv_buffer,
				       sock->recv_buffer_fill))
	    {
	      sock->recv_buffer_fill = 0;
	      break;
	    }
        }
    }
  svz_array_destroy (bindings);

  /* check if any server processed this packet */
  if (sock->recv_buffer_fill)
    {
      svz_log (LOG_DEBUG, "rejecting udp packet on socket %d\n",
	       sock->sock_desc);
      sock->recv_buffer_fill = 0;
    }

  sock->cfg = NULL;
  return 0;
}

/*
 * Write the given @var{buf} into the send queue of the UDP socket. If the
 * length argument supersedes the maximum length for UDP messages it
 * is split into smaller packets.
 */
int
svz_udp_write (svz_socket_t *sock, char *buf, int length)
{
  char *buffer;
  unsigned len, size;
  int ret = 0;

  /* return if the socket has already been killed */
  if (sock->flags & SOCK_FLAG_KILLED)
    return 0;

  /* allocate memory block */
  buffer = svz_malloc ((length > UDP_MSG_SIZE ? UDP_MSG_SIZE : length) + 
		       sizeof (len) + sizeof (sock->remote_addr) +
		       sizeof (sock->remote_port));

  while (length)
    {
      /* 
       * Put the data length, destination address and port in front
       * of each data packet.
       */
      len = sizeof (len);
      memcpy (&buffer[len], &sock->remote_addr, sizeof (sock->remote_addr));
      len += sizeof (sock->remote_addr);
      memcpy (&buffer[len], &sock->remote_port, sizeof (sock->remote_port));
      len += sizeof (sock->remote_port);

      /* copy the given buffer */
      if ((size = length) > UDP_MSG_SIZE)
	size = UDP_MSG_SIZE;
      memcpy (&buffer[len], buf, size);
      len += size;
      memcpy (buffer, &len, sizeof (len));
      buf += size;
      length -= size;

      /* actually send the data or put it into the send buffer queue */
      if ((ret = svz_sock_write (sock, buffer, len)) == -1)
	{
	  sock->flags |= SOCK_FLAG_KILLED;
	  break;
	}
    }

  /* release memory block */
  svz_free (buffer);
  return ret;
}

/*
 * Print a formatted string on the UDP socket @var{sock}. @var{fmt} is 
 * the printf()-style format string, which describes how to format the 
 * optional arguments. See the printf(3) manual page for details. The 
 * destination address and port is saved for sending. This you might 
 * specify them in @code{sock->remote_addr} and @code{sock->remote_port}.
 */
int
svz_udp_printf (svz_socket_t *sock, const char *fmt, ...)
{
  va_list args;
  static char buffer[VSNPRINTF_BUF_SIZE];
  int len;

  if (sock->flags & SOCK_FLAG_KILLED)
    return 0;

  va_start (args, fmt);
  len = vsnprintf (buffer, VSNPRINTF_BUF_SIZE, fmt, args);
  va_end (args);

  return svz_udp_write (sock, buffer, len);
}

/*
 * Create a UDP connection to @var{host} and set the socket descriptor in
 * structure @var{sock} to the resulting socket. Return a @code{NULL} value 
 * on errors. This function can be used for port bouncing. If you assign the
 * @code{handle_request} callback to something server specific and the 
 * @var{cfg} field to the server's configuration to the returned socket 
 * structure this socket is able to handle a dedicated UDP connection to 
 * some other UDP server.
 */
svz_socket_t *
svz_udp_connect (unsigned long host, unsigned short port)
{
  svz_t_socket sockfd;
  svz_socket_t *sock;

  /* Create a client socket. */
  if ((sockfd = svz_socket_create (PROTO_UDP)) == (svz_t_socket) -1)
    return NULL;

  /* Try to connect to the server. Does it make sense for ICMP ? */
  if (svz_socket_connect (sockfd, host, port) == -1)
    return NULL;

  /* Create socket structure and enqueue it. */
  if ((sock = svz_sock_alloc ()) == NULL)
    {
      close (sockfd);
      return NULL;
    }

  svz_sock_resize_buffers (sock, UDP_BUF_SIZE, UDP_BUF_SIZE);
  svz_sock_unique_id (sock);
  sock->sock_desc = sockfd;
  sock->proto = PROTO_UDP;
  sock->flags |= (SOCK_FLAG_SOCK | SOCK_FLAG_CONNECTED | SOCK_FLAG_FIXED);
  svz_sock_enqueue (sock);
  svz_sock_intern_connection_info (sock);

  sock->read_socket = svz_udp_read_socket;
  sock->write_socket = svz_udp_write_socket;
  sock->check_request = svz_udp_check_request;

  svz_sock_connections++;
  return sock;
}
