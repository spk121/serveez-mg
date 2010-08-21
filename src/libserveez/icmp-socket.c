/*
 * icmp-socket.c - ICMP socket implementations
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
 * $Id: icmp-socket.c,v 1.21 2003/06/14 14:57:59 ela Exp $
 *
 */

#include <assert.h>             /* assert */
#include <stdio.h>              /* vsnprint */
#include <string.h>             /* memmove, memcpy */
#include <time.h>               /* time */
#include <stdarg.h>             /* va_start */
#include <stdint.h>             /* uint8_t */

# include <unistd.h>            /* getpid */

# include <sys/socket.h>        /* recv, recvfrom */
# include <netinet/in.h>        /* sockaddr_in */

#include "util.h"
#include "socket.h"
#include "core.h"
#include "server-core.h"
#include "icmp-socket.h"
#include "raw-socket.h"
#include "server.h"
#include "binding.h"

/* Text representation of ICMP type codes. */
static char *svz_icmp_request[] = {
  "echo reply",
  NULL,
  NULL,
  "destination unreachable",
  "source quench",
  "redirect (change route)",
  NULL,
  NULL,
  "echo request",
  NULL,
  NULL,
  "time exceeded",
  "parameter problem",
  "timestamp request",
  "timestamp reply",
  "information request",
  "information reply",
  "address mask request",
  "address mask reply"
};

/* Static buffer for ip packets. */
static char svz_icmp_buffer[IP_HEADER_SIZE + ICMP_HEADER_SIZE + ICMP_MSG_SIZE];

/*
 * Get ICMP header from plain data.
 */
static svz_icmp_header_t *
svz_icmp_get_header (uint8_t *data)
{
  static svz_icmp_header_t hdr;
  unsigned short uint16;

  hdr.type = *data++;
  hdr.code = *data++;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.checksum = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.ident = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.sequence = ntohs (uint16);
  data += SIZEOF_UINT16;
  memcpy (&uint16, data, SIZEOF_UINT16);
  hdr.port = uint16;

  return &hdr;
}

/*
 * Create ICMP header (data block) from given structure.
 */
static uint8_t *
svz_icmp_put_header (svz_icmp_header_t *hdr)
{
  static uint8_t buffer[ICMP_HEADER_SIZE];
  uint8_t *data = buffer;
  unsigned short uint16;

  *data++ = hdr->type;
  *data++ = hdr->code;
  uint16 = htons (hdr->checksum);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  uint16 = htons (hdr->ident);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  uint16 = htons (hdr->sequence);
  memcpy (data, &uint16, SIZEOF_UINT16);
  data += SIZEOF_UINT16;
  uint16 = hdr->port;
  memcpy (data, &uint16, SIZEOF_UINT16);

  return buffer;
}

#define ICMP_ERROR      -1
#define ICMP_DISCONNECT -2

/*
 * Parse and check IP and ICMP header. Return the amount of leading bytes 
 * to be truncated. Return ICMP_ERROR on packet errors and return 
 * ICMP_DISCONNECT when we received an disconnection signal.
 */
static int
svz_icmp_check_packet (svz_socket_t *sock, uint8_t *data, int len)
{
  int length;
  uint8_t *p = data;
  svz_icmp_header_t *header;

  /* First check the IP header. */
  if ((length = svz_raw_check_ip_header (p, len)) == -1)
    return ICMP_ERROR;

  /* Get the actual ICMP header. */
  header = svz_icmp_get_header (p + length);
  p += length + ICMP_HEADER_SIZE;
  len -= length + ICMP_HEADER_SIZE;

  /* Do these checks only if it is the right kind of packet. */
  if (header->type == sock->itype)
    {
      /* validate the ICMP data checksum */
      if (header->checksum != svz_raw_ip_checksum (p, len))
	{
	  svz_log (LOG_DEBUG, "icmp: invalid data checksum\n");
	  return ICMP_ERROR;
	}

      /* check the ICMP header identification */
      if (header->ident == getpid () + sock->id)
	{
	  svz_log (LOG_DEBUG, "icmp: rejecting native packet\n");
	  return ICMP_ERROR;
	}

      /* check ICMP remote port */
      if ((header->port != sock->remote_port) && 
	  !(sock->flags & SOCK_FLAG_LISTENING))
	{
	  svz_log (LOG_DEBUG, "icmp: rejecting filtered packet\n");
	  return ICMP_ERROR;
	}
      sock->remote_port = header->port;
    }

  /* What kind of packet is this ? */
  else if (header->type <= ICMP_MAX_TYPE)
    {
      if (svz_icmp_request[header->type])
	svz_log (LOG_DEBUG, "icmp: %s received\n", 
		 svz_icmp_request[header->type]);
      else
	svz_log (LOG_DEBUG, "unsupported protocol 0x%02X received\n", 
		 header->type);
      return ICMP_ERROR;
    }

  if (header->type == sock->itype)
    {
      if (header->code == ICMP_SERVEEZ_CONNECT && 
	  sock->flags & SOCK_FLAG_LISTENING)
	{
	  svz_log (LOG_NOTICE, "icmp: accepting connection\n");
	}
      else if (header->code == ICMP_SERVEEZ_CLOSE)
	{
	  svz_log (LOG_NOTICE, "icmp: closing connection\n");
	  return ICMP_DISCONNECT;
	}
      return (length + ICMP_HEADER_SIZE);
    }
  else
    {
      svz_log (LOG_DEBUG, "unsupported protocol 0x%02X received\n", 
	       header->type);
    }

  return ICMP_ERROR;
}

/*
 * Default reader for ICMP sockets. The sender is stored within 
 * @code{sock->remote_addr} and @code{sock->remote_port} afterwards.
 */
int
svz_icmp_read_socket (svz_socket_t *sock)
{
  int num_read;
  socklen_t len;
  struct sockaddr_in sender;
  int trunc;

  len = sizeof (struct sockaddr_in);

  /* Receive data. */
  if (!(sock->flags & SOCK_FLAG_CONNECTED))
    {
      num_read = recvfrom (sock->sock_desc, svz_icmp_buffer, 
			   sizeof (svz_icmp_buffer), 0, 
			   (struct sockaddr *) &sender, &len);
    }
  else
    {
      num_read = recv (sock->sock_desc, svz_icmp_buffer, 
		       sizeof (svz_icmp_buffer), 0);
    }

  /* Valid packet data arrived. */
  if (num_read > 0)
    {
#if 0
      svz_hexdump (stdout, "icmp packet received", sock->sock_desc,
		   svz_icmp_buffer, num_read, 0);
#endif
      sock->last_recv = time (NULL);
      if (!(sock->flags & SOCK_FLAG_FIXED))
        {
	  sock->remote_addr = sender.sin_addr.s_addr;
	}
      svz_log (LOG_DEBUG, "icmp: recv%s: %s (%u bytes)\n",
	       sock->flags & SOCK_FLAG_CONNECTED ? "" : "from",
	       svz_inet_ntoa (sock->remote_addr), num_read);

      /* 
       * Check the ICMP packet and put the packet load only into the
       * receive buffer of the socket structure.
       */
      trunc = svz_icmp_check_packet (sock, (uint8_t *) svz_icmp_buffer, 
				     num_read);
      if (trunc >= 0)
	{
	  num_read -= trunc;
	  if (num_read > sock->recv_buffer_size - sock->recv_buffer_fill)
	    {
	      svz_log (LOG_ERROR, 
		       "receive buffer overflow on icmp socket %d\n",
		       sock->sock_desc);
	      return -1;
	    }
  
	  memcpy (sock->recv_buffer + sock->recv_buffer_fill,
		  svz_icmp_buffer + trunc, num_read);
	  sock->recv_buffer_fill += num_read;

	  /* Check access lists. */
	  if (svz_sock_check_access (sock, sock) < 0)
	    return 0;

	  if (sock->check_request)
	    sock->check_request (sock);
	}
      else if (trunc == ICMP_DISCONNECT)
	{
	  return -1;
	}
    }
  /* Some error occurred. */
  else
    {
      svz_log (LOG_ERROR, "icmp: recv%s: %s\n", 
	       sock->flags & SOCK_FLAG_CONNECTED ? "" : "from", NET_ERROR);
      if (errno != EAGAIN)
	return -1;
    }
  return 0;
}

/*
 * Default reader for ICMP server sockets. Allocates necessary buffers and
 * reverts to @code{svz_icmp_read_socket()}.
 */
int
svz_icmp_lazy_read_socket (svz_socket_t *sock)
{
  svz_portcfg_t *port = sock->port;

  svz_sock_resize_buffers (sock, port->send_buffer_size,
			   port->recv_buffer_size);
  sock->read_socket = svz_icmp_read_socket;

  return sock->read_socket (sock);
}

/*
 * The default ICMP write callback is called whenever the socket 
 * descriptor has been @code{select()}'ed or @code{poll()}'ed to be ready for 
 * sending.
 */
int
svz_icmp_write_socket (svz_socket_t *sock)
{
  int num_written;
  unsigned do_write;
  char *p;
  socklen_t len;
  struct sockaddr_in receiver;

  /* Return here if there is nothing to do. */
  if (sock->send_buffer_fill <= 0)
    return 0;

  len = sizeof (struct sockaddr_in);
  receiver.sin_family = AF_INET;

  /* Get destination address and data length from send buffer. */
  p = sock->send_buffer;
  memcpy (&do_write, p, sizeof (do_write));
  p += sizeof (do_write);
  memcpy (&receiver.sin_addr.s_addr, p, sizeof (sock->remote_addr));
  p += sizeof (sock->remote_addr);
  memcpy (&receiver.sin_port, p, sizeof (sock->remote_port));
  p += sizeof (sock->remote_port);
  assert ((int) do_write <= sock->send_buffer_fill);

  /* If socket is `connect ()'ed use `send ()' instead of `sendto ()'. */
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

  /* Some error occurred while sending. */
  if (num_written < 0)
    {
      svz_log (LOG_ERROR, "icmp: send%s: %s\n", 
	       sock->flags & SOCK_FLAG_CONNECTED ? "" : "to", NET_ERROR);
      if (errno == EAGAIN)
        num_written = 0;
    }
  /* Packet data could be transmitted. */
  else
    {
      sock->last_send = time (NULL);
      if ((unsigned) sock->send_buffer_fill > do_write)
        {
          memmove (sock->send_buffer, 
                   sock->send_buffer + do_write,
                   sock->send_buffer_fill - do_write);
        }
      sock->send_buffer_fill -= do_write;
    }

  svz_log (LOG_DEBUG, "icmp: send%s: %s (%u bytes)\n",
	   sock->flags & SOCK_FLAG_CONNECTED ? "" : "to",
	   svz_inet_ntoa (receiver.sin_addr.s_addr),
	   do_write - (p - sock->send_buffer));

  return num_written < 0 ? -1 : 0;
}

/*
 * If you are calling this function we will send an empty ICMP packet
 * signaling that this connection is going down soon.
 */
int
svz_icmp_send_control (svz_socket_t *sock, uint8_t type)
{
  static char *buffer = svz_icmp_buffer;
  svz_icmp_header_t hdr;
  unsigned len;
  int ret = 0;

  len = sizeof (len);
  memcpy (&buffer[len], &sock->remote_addr, sizeof (sock->remote_addr));
  len += sizeof (sock->remote_addr);
  memcpy (&buffer[len], &sock->remote_port, sizeof (sock->remote_port));
  len += sizeof (sock->remote_port);

  hdr.type = sock->itype;
  hdr.code = type;
  hdr.checksum = svz_raw_ip_checksum (NULL, 0);
  hdr.ident = (unsigned short) (getpid () + sock->id);
  hdr.sequence = sock->send_seq;
  hdr.port = sock->remote_port;
  memcpy (&buffer[len], svz_icmp_put_header (&hdr), ICMP_HEADER_SIZE);
  len += ICMP_HEADER_SIZE;
  memcpy (buffer, &len, sizeof (len));

  if ((ret = svz_sock_write (sock, buffer, len)) == -1)
    {
      sock->flags |= SOCK_FLAG_KILLED;
    }
  return ret;
}

/*
 * Send a given buffer @var{buf} with length @var{length} via this ICMP 
 * socket. If the length argument supersedes the maximum ICMP message
 * size the buffer is split into smaller packets.
 */
int
svz_icmp_write (svz_socket_t *sock, char *buf, int length)
{
  static char *buffer = svz_icmp_buffer;
  svz_icmp_header_t hdr;
  unsigned len, size;
  int ret = 0;

  /* Return if the socket has already been killed. */
  if (sock->flags & SOCK_FLAG_KILLED)
    return 0;

  while (length)
    {
      /* 
       * Put the data length and destination address in front 
       * of each packet. 
       */
      len = sizeof (len);
      memcpy (&buffer[len], &sock->remote_addr, sizeof (sock->remote_addr));
      len += sizeof (sock->remote_addr);
      memcpy (&buffer[len], &sock->remote_port, sizeof (sock->remote_port));
      len += sizeof (sock->remote_port);
      if ((size = length) > ICMP_MSG_SIZE)
	size = ICMP_MSG_SIZE;

      /* Create ICMP header and put it in front of packet load. */
      hdr.type = sock->itype;
      hdr.code = ICMP_SERVEEZ_DATA;
      hdr.checksum = svz_raw_ip_checksum ((uint8_t *) buf, size);
      hdr.ident = (unsigned short) (getpid () + sock->id);
      hdr.sequence = sock->send_seq++;
      hdr.port = sock->remote_port;
      memcpy (&buffer[len], svz_icmp_put_header (&hdr), ICMP_HEADER_SIZE);
      len += ICMP_HEADER_SIZE;

      /* Copy the given buffer. */
      memcpy (&buffer[len], buf, size);
      len += size;

      /* Put chunk length to buffer. */
      memcpy (buffer, &len, sizeof (len));
      buf += size;
      length -= size;

      /* Actually send the data or put it into the send buffer queue. */
      if ((ret = svz_sock_write (sock, buffer, len)) == -1)
        {
          sock->flags |= SOCK_FLAG_KILLED;
          break;
        }
    }

  return ret;
}

/*
 * Put a formatted string to the icmp socket @var{sock}. Packet length and
 * destination address are additionally saved to the send buffer. The
 * destination is taken from @code{sock->remote_addr}. Furthermore a valid
 * icmp header is stored in front of the actual packet data.
 */
int
svz_icmp_printf (svz_socket_t *sock, const char *fmt, ...)
{
  va_list args;
  static char buffer[VSNPRINTF_BUF_SIZE];
  unsigned len;

  /* return if there is nothing to do */
  if (sock->flags & SOCK_FLAG_KILLED)
    return 0;

  /* save actual packet load */
  va_start (args, fmt);
  len = vsnprintf (buffer, VSNPRINTF_BUF_SIZE, fmt, args);
  va_end (args);
  
  return svz_icmp_write (sock, buffer, len);
}

/*
 * Default @code{check_request()} callback for ICMP sockets.
 */
int
svz_icmp_check_request (svz_socket_t *sock)
{
  int n;
  svz_server_t *server;
  svz_array_t *bindings;
  svz_binding_t *binding;

  if (sock->data == NULL && sock->handle_request == NULL)
    return -1;

  /* 
   * If there is a valid `handle_request' callback (dedicated icmp 
   * connection) call it. This kind of behaviour is due to a socket 
   * creation via 'icmp_connect' (s.b.) and setting up a static 
   * `handle_request' callback.
   */
  if (sock->handle_request)
    {
      if (sock->handle_request (sock, sock->recv_buffer,
                                sock->recv_buffer_fill))
        return -1;
      sock->recv_buffer_fill = 0;
      return 0;
    }

  /* Go through all icmp servers on this server socket. */
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

  /* Check if any server processed this packet. */
  if (sock->recv_buffer_fill)
    {
      svz_log (LOG_DEBUG, "rejecting icmp packet on socket %d\n",
	       sock->sock_desc);
      sock->recv_buffer_fill = 0;
    }

  sock->cfg = NULL;
  return 0;
}

/*
 * This function creates an ICMP socket for receiving and sending.
 * Return @code{NULL} on errors, otherwise an enqueued socket structure.
 */
svz_socket_t *
svz_icmp_connect (unsigned long host, unsigned short port,
		  unsigned char type)
{
  svz_t_socket sockfd;
  svz_socket_t *sock;

  /* Create a client socket. */
  if ((sockfd = svz_socket_create (PROTO_ICMP)) == (svz_t_socket) -1)
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

  svz_sock_resize_buffers (sock, ICMP_BUF_SIZE, ICMP_BUF_SIZE);
  svz_sock_unique_id (sock);
  sock->sock_desc = sockfd;
  sock->proto = PROTO_ICMP;
  sock->flags |= (SOCK_FLAG_SOCK | SOCK_FLAG_CONNECTED | SOCK_FLAG_FIXED);
  sock->itype = type;
  svz_sock_enqueue (sock);
  svz_sock_intern_connection_info (sock);

  /* Put foreign address here. */
  sock->remote_addr = host;
  sock->remote_port = (unsigned short) sock->id;

  sock->read_socket = svz_icmp_read_socket;
  sock->write_socket = svz_icmp_write_socket;
  sock->check_request = svz_icmp_check_request;

  /* Finally send a connection message. */
  svz_icmp_send_control (sock, ICMP_SERVEEZ_CONNECT);
  svz_sock_connections++;
  return sock;
}
