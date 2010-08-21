/*
 * socket.c - socket management implementation
 *
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
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
 * $Id: socket.c,v 1.24 2003/06/14 14:57:59 ela Exp $
 *
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <signal.h>
#include <time.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "alloc.h"
#include "util.h"
#include "socket.h"
#include "core.h"
#include "pipe-socket.h"
#include "tcp-socket.h"
#include "server-core.h"
#include "server.h"
#include "binding.h"

/*
 * Count the number of currently connected sockets.
 */
int svz_sock_connections = 0;

/*
 * This routine can be called if flood protection is wished for
 * socket readers. Return non-zero if the socket should be kicked
 * because of flood.
 */
int
svz_sock_flood_protect (svz_socket_t *sock, int num_read)
{
  if (!(sock->flags & SOCK_FLAG_NOFLOOD))
    {
      /* 
       * Since the default flood limit is 100 a reader can produce 
       * 5000 bytes per second before it gets kicked.
       */
      sock->flood_points += 1 + (num_read / 50);
	  
      if (sock->flood_points > sock->flood_limit)
	{
	  if (sock->kicked_socket)
	    sock->kicked_socket (sock, 0);
	  return -1;
	}
    }
  return 0;
}

/*
 * The default function which gets called when a client shuts down
 * its socket. @var{sock} is the socket which was closed.
 */
static int
svz_sock_default_disconnect (svz_socket_t *sock)
{
  svz_log (LOG_DEBUG, "socket id %d disconnected\n", sock->id);

  return 0;
}

/*
 * This routine gets called whenever data is read from a client socket
 * accepted by any connection oriented protocol layer (TCP or PIPE). We
 * try to detect the data streams protocol here.
 */
int
svz_sock_detect_proto (svz_socket_t *sock)
{
  int n;
  svz_server_t *server;
  svz_binding_t *binding;
  svz_portcfg_t *port;
  svz_array_t *bindings;

  /* return if there are no servers bound to this socket */
  if (sock->data == NULL)
    return -1;

  /* get port configuration of parent */
  port = svz_sock_portcfg (sock);

  /* go through each server stored in the data field of this socket */
  bindings = svz_binding_filter (sock);
  svz_array_foreach (bindings, binding, n)
    {
      server = binding->server;

      /* can occur if it is actually a packet oriented server */
      if (server->detect_proto == NULL)
	{
	  svz_log (LOG_ERROR, "%s: no detect-proto routine\n", 
		   server->type->prefix);
	}
      /* call protocol detection routine of the server */
      else if (server->detect_proto (server, sock))
	{
	  svz_array_destroy (bindings);
	  sock->idle_func = NULL;
	  sock->data = NULL;
	  sock->cfg = server->cfg;
	  sock->port = binding->port;
	  if (!server->connect_socket)
	    return -1;
	  if (server->connect_socket (server, sock))
	    return -1;
	  if (sock->check_request == svz_sock_detect_proto)
	    {
	      svz_log (LOG_ERROR, "%s: check-request callback unchanged\n", 
		       server->type->prefix);
	      sock->check_request = NULL;
	    }
	  if (sock->check_request)
	    return sock->check_request (sock);
	  return 0;
	}
    }
  svz_array_destroy (bindings);

  /*
   * Discard this socket if there were not any valid protocol
   * detected and its receive buffer fill exceeds a maximum value.
   */
  if (sock->recv_buffer_fill > port->detection_fill)
    {
      svz_log (LOG_DEBUG, "socket id %d detection failed\n", sock->id);
      return -1;
    }

  return 0;
}

/*
 * Default idle function. This routine simply checks for "dead" 
 * (non-receiving) sockets (connection oriented protocols only) and rejects 
 * them by return a non-zero value.
 */
int
svz_sock_idle_protect (svz_socket_t *sock)
{
  svz_portcfg_t *port = svz_sock_portcfg (sock);

  if (time (NULL) - sock->last_recv > port->detection_wait)
    {
      svz_log (LOG_DEBUG, "socket id %d detection failed\n", sock->id);
      return -1;
    }

  sock->idle_counter = 1;
  return 0;
}

/*
 * This @code{check_request()} routine could be used by any protocol to 
 * detect and finally handle packets depending on a specific packet 
 * boundary. The appropriate @code{handle_request()} is called for each packet
 * explicitly with the packet length inclusive the packet boundary.
 */
static int
svz_sock_check_request_array (svz_socket_t *sock)
{
  int len = 0;
  char *p, *packet, *end;

  packet = p = sock->recv_buffer;
  end = p + sock->recv_buffer_fill - sock->boundary_size + 1;

  do
    {
      /* Find packet boundary in the receive buffer. */
      while (p < end && memcmp (p, sock->boundary, sock->boundary_size))
        p++;

      /* Found ? */
      if (p < end && !memcmp (p, sock->boundary, sock->boundary_size))
        {
          p += sock->boundary_size;
          len += (p - packet);

	  /* Call the handle request callback. */
	  if (sock->handle_request)
	    {
	      if (sock->handle_request (sock, packet, p - packet))
		return -1;
	    }
	  packet = p;
        }
    }
  while (p < end);
  
  /* Shuffle data in the receive buffer around. */
  svz_sock_reduce_recv (sock, len);
  
  return 0;
}

/*
 * This is just the same routine as above, but optimized for one byte
 * packet delimiters.
 */
static int
svz_sock_check_request_byte (svz_socket_t *sock)
{
  int len = 0;
  char *p, *packet, *end;

  packet = p = sock->recv_buffer;
  end = p + sock->recv_buffer_fill;

  do
    {
      /* Find packet boundary in the receive buffer. */
      while (p < end && *p != *sock->boundary)
        p++;

      /* Found ? */
      if (p < end && *p == *sock->boundary)
        {
          p++;
          len += (p - packet);

	  /* Call the handle request callback. */
	  if (sock->handle_request)
	    {
	      if (sock->handle_request (sock, packet, p - packet))
		return -1;
	    }
	  packet = p;
        }
    }
  while (p < end);
  
  /* Shuffle data in the receive buffer around. */
  svz_sock_reduce_recv (sock, len);
  
  return 0;
}

/*
 * The following routine checks for fixed size packets in the receive queue 
 * of the socket structure @var{sock} and calls the @code{handle_request()} 
 * callback if so. It is possible to change the fixed packet size in the
 * @code{handle_request()} callback dynamically.
 */
static int
svz_sock_check_request_size (svz_socket_t *sock)
{
  int len = 0;
  char *p, *packet, *end;

  packet = p = sock->recv_buffer;
  end = p + sock->recv_buffer_fill;

  while (p + sock->boundary_size < end)
    {
      len += sock->boundary_size;
      p += sock->boundary_size;

      /* Call the handle request callback. */
      if (sock->handle_request)
	{
	  if (sock->handle_request (sock, packet, sock->boundary_size))
	    return -1;
	}
      packet = p;
    }

  /* Shuffle data in the receive buffer around. */
  svz_sock_reduce_recv (sock, len);
  
  return 0;
}

/*
 * This function simply checks for the kind of packet delimiter within the 
 * given socket structure and and assigns one of the default 
 * @code{check_request()} routines (one or more byte delimiters or a fixed
 * size). Afterwards this routine will never ever be called again because 
 * the callback gets overwritten here.
 */
int
svz_sock_check_request (svz_socket_t *sock)
{
  if (sock->boundary_size <= 0)
    {
      svz_log (LOG_ERROR, "invalid boundary size: %d\n", sock->boundary_size);
      return -1;
    }

  if (sock->boundary == NULL)
    sock->check_request = svz_sock_check_request_size;
  else if (sock->boundary_size > 1)
    sock->check_request = svz_sock_check_request_array;
  else
    sock->check_request = svz_sock_check_request_byte;

  return sock->check_request (sock);
}

/*
 * Allocate a structure of type @code{svz_socket_t} and initialize its data
 * fields. Assign some of the default callbacks for TCP connections.
 */
svz_socket_t *
svz_sock_alloc (void)
{
  svz_socket_t *sock;
  char *in;
  char *out;

  sock = svz_malloc (sizeof (svz_socket_t));
  memset (sock, 0, sizeof (svz_socket_t));
  in = svz_malloc (RECV_BUF_SIZE);
  out = svz_malloc (SEND_BUF_SIZE);

  sock->proto = SOCK_FLAG_INIT;
  sock->flags = SOCK_FLAG_INIT | SOCK_FLAG_INBUF | SOCK_FLAG_OUTBUF;
  sock->userflags = SOCK_FLAG_INIT;
  sock->file_desc = -1;
  sock->sock_desc = (svz_t_socket) -1;
  sock->pipe_desc[READ] = INVALID_HANDLE;
  sock->pipe_desc[WRITE] = INVALID_HANDLE;
  sock->pid = INVALID_HANDLE;

  sock->read_socket = svz_tcp_read_socket;
  sock->read_socket_oob = svz_tcp_recv_oob;
  sock->write_socket = svz_tcp_write_socket;
  sock->check_request = svz_sock_detect_proto;
  sock->disconnected_socket = svz_sock_default_disconnect;

  sock->recv_buffer = in;
  sock->recv_buffer_size = RECV_BUF_SIZE;
  sock->send_buffer = out;
  sock->send_buffer_size = SEND_BUF_SIZE;
  sock->last_send = time (NULL);
  sock->last_recv = time (NULL);

  sock->flood_limit = 100;

  return sock;
}

/*
 * Resize the send and receive buffers for the socket @var{sock}. 
 * @var{send_buf_size} is the new size for the send buffer, 
 * @var{recv_buf_size} for the receive buffer. Note that data may be lost 
 * when the buffers shrink. For a new buffer size of 0 the buffer is
 * freed and the pointer set to NULL.
 */
int 
svz_sock_resize_buffers (svz_socket_t *sock, 
			 int send_buf_size, int recv_buf_size)
{
  char *send, *recv;

  if (send_buf_size == 0)
    {
      svz_free (sock->send_buffer);
      send = NULL;
    }
  else if (sock->send_buffer_size != send_buf_size)
    send = svz_realloc (sock->send_buffer, send_buf_size);
  else
    send = sock->send_buffer;

  if (recv_buf_size == 0)
    {
      svz_free (sock->recv_buffer);
      recv = NULL;
    }
  else if (sock->recv_buffer_size != recv_buf_size)
    recv = svz_realloc (sock->recv_buffer, recv_buf_size);
  else
    recv = sock->recv_buffer;

  sock->send_buffer = send;
  sock->recv_buffer = recv;
  sock->send_buffer_size = send_buf_size;
  sock->recv_buffer_size = recv_buf_size;

  return 0;
}

/*
 * Free the socket structure @var{sock}. Return a non-zero value on error.
 */
int
svz_sock_free (svz_socket_t *sock)
{
  if (sock->recv_buffer)
    svz_free (sock->recv_buffer);
  if (sock->send_buffer)
    svz_free (sock->send_buffer);
  if (sock->flags & SOCK_FLAG_LISTENING)
    {
      if (sock->data)
	svz_array_destroy (sock->data);
    }
  if (sock->recv_pipe)
    svz_free (sock->recv_pipe);
  if (sock->send_pipe)
    svz_free (sock->send_pipe);

  svz_free (sock);

  return 0;
}

/*
 * Get local and remote addresses and ports of socket @var{sock} and save 
 * them into the socket structure.
 */
int
svz_sock_intern_connection_info (svz_socket_t *sock)
{
  struct sockaddr_in s;
  socklen_t size = sizeof (s);
  unsigned short port;
  unsigned long addr;

  if (!getpeername (sock->sock_desc, (struct sockaddr *) &s, &size))
    {
      addr = s.sin_addr.s_addr;
      port = s.sin_port;
    }
  else
    {
      addr = INADDR_ANY;
      port = 0;
    }
  sock->remote_port = port;
  sock->remote_addr = addr;

  size = sizeof (s);
  if (!getsockname (sock->sock_desc, (struct sockaddr *) &s, &size))
    {
      addr = s.sin_addr.s_addr;
      port = s.sin_port;
    }
  else
    {
      addr = INADDR_ANY;
      port = 0;
    }
  sock->local_port = port;
  sock->local_addr = addr;

  return 0;
}

/*
 * This function returns the local network address and port for the given
 * client socket structure @var{sock}.  It returns non-zero if there no 
 * connection established.
 */
int
svz_sock_local_info (svz_socket_t *sock, 
		     unsigned long *addr, unsigned short *port)
{
  struct sockaddr_in s;
  socklen_t size = sizeof (s);

  if (!getsockname (sock->sock_desc, (struct sockaddr *) &s, &size))
    {
      if (addr)
	*addr = s.sin_addr.s_addr;
      if (port)
	*port = s.sin_port;
      return 0;
    }
  return -1;
}

/*
 * Get and clear the pending socket error of a given socket. Print
 * the result to the log file.
 */
int
svz_sock_error_info (svz_socket_t *sock)
{
  int error;
  socklen_t optlen = sizeof (int);

  if (getsockopt (sock->sock_desc, SOL_SOCKET, SO_ERROR,
                  (void *) &error, &optlen) < 0)
    {
      svz_log (LOG_ERROR, "getsockopt: %s\n", NET_ERROR);
      return -1;
    }
  if (error)
    {
      errno = error;
      svz_log (LOG_ERROR, "%s\n", NET_ERROR);
      return -1;
    }
  return 0;
}

/*
 * Check if a given socket is still valid. Return non-zero if it is
 * not.
 */
int
svz_sock_valid (svz_socket_t *sock)
{
  if (!(sock->flags & (SOCK_FLAG_LISTENING | 
		       SOCK_FLAG_CONNECTED | SOCK_FLAG_CONNECTING)))
    return -1;

  if (sock->sock_desc == INVALID_SOCKET)
    return -1;

  return 0;
}

/*
 * Create a socket structure from the file descriptor @var{fd}. Set the 
 * socket descriptor to non-blocking I/O. Return @code{NULL} on errors.
 */
svz_socket_t *
svz_sock_create (int fd)
{
  svz_socket_t *sock;

  if (svz_fd_nonblock (fd) != 0)
    return NULL;
  if (svz_fd_cloexec (fd) != 0)
    return NULL;

  if ((sock = svz_sock_alloc ()) != NULL)
    {
      svz_sock_unique_id (sock);
      sock->sock_desc = fd;
      sock->flags |= (SOCK_FLAG_CONNECTED | SOCK_FLAG_SOCK);
      svz_sock_intern_connection_info (sock);
    }
  return sock;
}

/*
 * Disconnect the socket @var{sock} from the network and calls the disconnect 
 * function for the socket if set. Return a non-zero value on errors.
 */
int
svz_sock_disconnect (svz_socket_t *sock)
{
  /* shutdown client connection */
  if (sock->flags & SOCK_FLAG_CONNECTED)
    {
      if (!(sock->flags & SOCK_FLAG_NOSHUTDOWN))
	{
	  if (shutdown (sock->sock_desc, 2) < 0)
	    svz_log (LOG_ERROR, "shutdown: %s\n", NET_ERROR);
	}
      svz_sock_connections--;
    }

  /* close the server/client socket */
  if (close (sock->sock_desc) < 0)
    svz_log (LOG_ERROR, "close: %s\n", NET_ERROR);

  svz_log (LOG_DEBUG, "socket %d disconnected\n", sock->sock_desc);

  sock->sock_desc = INVALID_SOCKET;
  return 0;
}

/*
 * Write @var{len} bytes from the memory location pointed to by @var{buf}
 * to the output buffer of the socket @var{sock}. Also try to flush the 
 * buffer to the socket of @var{sock} if possible.  Return a non-zero value 
 * on error, which normally means a buffer overflow.
 */
int
svz_sock_write (svz_socket_t *sock, char *buf, int len)
{
  int ret;
  int space;

  if (sock->flags & SOCK_FLAG_KILLED)
    return 0;

  while (len > 0)
    {
      /* Try to flush the queue of this socket. */
      if (sock->write_socket && !sock->unavailable && 
	  sock->flags & SOCK_FLAG_CONNECTED && sock->send_buffer_fill)
	{
	  if ((ret = sock->write_socket (sock)) != 0)
	    return ret;
	}

      if (sock->send_buffer_fill >= sock->send_buffer_size)
	{
	  /* Queue is full, unlucky socket or pipe ... */
	  if (sock->flags & SOCK_FLAG_SEND_PIPE)
	    svz_log (LOG_ERROR,
		     "send buffer overflow on pipe (%d-%d) (id %d)\n",
		     sock->pipe_desc[READ], sock->pipe_desc[WRITE], sock->id);
	  else
	    svz_log (LOG_ERROR,
		     "send buffer overflow on socket %d (id %d)\n",
		     sock->sock_desc, sock->id);
	
	  if (sock->kicked_socket)
	    sock->kicked_socket (sock, 1);

	  return -1;
	}
    
      /* Now move as much of BUF into the send queue. */
      if (sock->send_buffer_fill + len < sock->send_buffer_size)
	{
	  memcpy (sock->send_buffer + sock->send_buffer_fill, buf, len);
	  sock->send_buffer_fill += len;
	  len = 0;
	}
      else
	{
	  space = sock->send_buffer_size - sock->send_buffer_fill;
	  memcpy (sock->send_buffer + sock->send_buffer_fill, buf, space);
	  sock->send_buffer_fill += space;
	  len -= space;
	  buf += space;
	}
    }

  return 0;
}

/*
 * Print a formatted string on the socket @var{sock}. @var{fmt} is the 
 * printf()-style format string, which describes how to format the optional
 * arguments. See the printf(3) manual page for details.
 */
int
svz_sock_printf (svz_socket_t *sock, const char *fmt, ...)
{
  va_list args;
  static char buffer[VSNPRINTF_BUF_SIZE];
  unsigned len;

  if (sock->flags & SOCK_FLAG_KILLED)
    return 0;

  va_start (args, fmt);
  len = vsnprintf (buffer, VSNPRINTF_BUF_SIZE, fmt, args);
  va_end (args);

  /* Just to be sure... */
  if (len > sizeof (buffer))
    len = sizeof (buffer);

  return svz_sock_write (sock, buffer, len);
}
