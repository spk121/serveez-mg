/*
 * guile-api.c - export additional Serveez functionality to Guile
 *
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: guile-api.c,v 1.32 2003/06/01 12:57:07 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_GUILE_SERVER

#include <string.h>
#include <errno.h>

#ifndef __MINGW32__
# include <sys/socket.h>
# include <netdb.h>
# include <netinet/in.h>
#endif

#if HAVE_RPC_RPCENT_H
# include <rpc/rpcent.h>
#endif
#if HAVE_RPC_RPC_H
# include <rpc/rpc.h>
#endif
#if HAVE_RPC_CLNT_SOC_H
# include <rpc/clnt_soc.h>
#endif
#if HAVE_RPC_PMAP_CLNT_H
# include <rpc/pmap_clnt.h>
#endif
#if HAVE_RPC_PMAP_PROT_H
# include <rpc/pmap_prot.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_IO_H
# include <io.h>
#endif

#if GUILE_SOURCE
# include <libguile/gh.h>
#else
# include <guile/gh.h>
#endif

#include "libserveez.h"

/* Converts the given hostname @var{host} into a Internet address in host
   byte order and stores it into @var{addr}. Returns zero on success. This
   is a blocking operation. */
static int
guile_resolve (char *host, unsigned long *addr)
{
  struct hostent *ent;

  if ((ent = gethostbyname (host)) == NULL)
    return -1;
  if (ent->h_addrtype == AF_INET)
    {
      memcpy (addr, ent->h_addr_list[0], ent->h_length);
      return 0;
    }
  return -1;
}

/* Establishes a network connection to the given @var{host} [ :@var{port} ].
   If @var{proto} equals @code{PROTO_ICMP} the @var{port} argument is
   ignored. Valid identifiers for @var{proto} are @code{PROTO_TCP},
   @code{PROTO_UDP} and @code{PROTO_ICMP}. The @var{host} argument must be 
   either a string in dotted decimal form, a valid hostname or an exact number
   in host byte order. When giving a hostname this operation might be
   blocking. The @var{port} argument must be an exact number in the range from 
   0 to 65535, also in host byte order. Returns a valid @code{#<svz-socket>} 
   or @code{#f} on failure. */
#define FUNC_NAME "svz:sock:connect"
SCM
guile_sock_connect (SCM host, SCM proto, SCM port)
{
  svz_socket_t *sock;
  uint32_t xhost;
  unsigned short xport = 0;
  uint16_t p;
  int xproto;
  char *str;
  struct sockaddr_in addr;
  SCM ret = SCM_BOOL_F;

  SCM_ASSERT (scm_is_integer (host) || scm_is_string (host), host, SCM_ARG1, 
              FUNC_NAME);
  SCM_ASSERT (scm_is_integer (proto), proto, SCM_ARG2, FUNC_NAME);

  /* Extract host to connect to. */
  if (scm_is_integer (host))
    xhost = htonl (scm_to_uint32 (host));
  else
    {
      str = guile_to_string (host);
      if (svz_inet_aton (str, &addr) == -1)
	{
	  if (guile_resolve (str, &xhost) == -1)
	    {
	      guile_error ("%s: IP in dotted decimals or hostname expected", 
			   FUNC_NAME);
	      free (str);
	      return ret;
	    }
	}
      else
	xhost = addr.sin_addr.s_addr;
      free (str);
    }

  /* Extract protocol to use. */
  xproto = scm_to_int (proto);

  /* Find out about given port. */
  if (!SCM_UNBNDP (port))
    {
      SCM_ASSERT (scm_is_integer (port), port, SCM_ARG3, FUNC_NAME);
      p = scm_to_uint16 (port);
      xport = htons ((unsigned short) p);
    }

  /* Depending on the requested protocol; create different kinds of
     socket structures. */
  switch (xproto)
    {
    case PROTO_TCP:
      sock = svz_tcp_connect (xhost, xport);
      break;
    case PROTO_UDP:
      sock = svz_udp_connect (xhost, xport);
      break;
    case PROTO_ICMP:
      sock = svz_icmp_connect (xhost, xport, ICMP_SERVEEZ);
      break;
    default:
      scm_out_of_range (FUNC_NAME, proto);
    }

  if (sock == NULL)
    return ret;

  sock->disconnected_socket = guile_func_disconnected_socket;
  SCM_RETURN_NEWSMOB (guile_svz_socket_tag, sock);
}
#undef FUNC_NAME

/* Return the receive buffer of the socket @var{sock} as a binary smob. */
#define FUNC_NAME "svz:sock:receive-buffer"
static SCM
guile_sock_receive_buffer (SCM sock)
{
  svz_socket_t *xsock;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  return scm_c_take_bytevector (xsock->recv_buffer, xsock->recv_buffer_fill);
}
#undef FUNC_NAME

/* Returns the current receive buffers size and fill status in bytes of
   the socket @var{sock} as a pair of exact numbers.  If the optional
   argument @var{size} is given the receive buffer will be set to the
   specified size in bytes. */
#define FUNC_NAME "svz:sock:receive-buffer-size"
static SCM
guile_sock_receive_buffer_size (SCM sock, SCM size)
{
  svz_socket_t *xsock;
  int len;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  if (!SCM_UNBNDP (size))
    {
      SCM_ASSERT (scm_is_integer (size), size, SCM_ARG2, FUNC_NAME);
      len = scm_to_int (size);
      svz_sock_resize_buffers (xsock, xsock->send_buffer_size, len);
    }
  return scm_cons (scm_from_int (xsock->recv_buffer_size), 
		   scm_from_int (xsock->recv_buffer_fill));
}
#undef FUNC_NAME

/* Return the send buffer of the socket @var{sock} as a binary smob. */
#define FUNC_NAME "svz:sock:send-buffer"
static SCM
guile_sock_send_buffer (SCM sock)
{
  svz_socket_t *xsock;
  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  return scm_c_take_bytevector (xsock->send_buffer, xsock->send_buffer_fill);
}
#undef FUNC_NAME

/* This procedure returns the current send buffer size and fill status in
   bytes of the socket @var{sock} as a pair of exact numbers.  If the
   optional argument @var{size} is given the send buffer will be set to
   the specified size in bytes. */
#define FUNC_NAME "svz:sock:send-buffer-size"
static SCM
guile_sock_send_buffer_size (SCM sock, SCM size)
{
  svz_socket_t *xsock;
  int len;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  if (!SCM_UNBNDP (size))
    {
      SCM_ASSERT (scm_is_integer (size), size, SCM_ARG2, FUNC_NAME);
      len = scm_to_int (size);
      svz_sock_resize_buffers (xsock, len, xsock->recv_buffer_size);
    }
  return scm_cons (scm_from_int (xsock->send_buffer_size), 
		   scm_from_int (xsock->send_buffer_fill));
}
#undef FUNC_NAME

/* Dequeue @var{length} bytes from the receive buffer of the socket 
   @var{sock} which must be a valid @code{#<svz-socket>}. If the user omits
   the optional @var{length} argument, all of the data in the receive buffer
   gets dequeued. Returns the number of bytes actually shuffled away. */
#define FUNC_NAME "svz:sock:receive-buffer-reduce"
static SCM
guile_sock_receive_buffer_reduce (SCM sock, SCM length)
{
  svz_socket_t *xsock;
  int len;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);

  /* Check if second length argument is given. */
  if (!SCM_UNBNDP (length))
    {
      SCM_ASSERT (scm_is_integer (length), length, SCM_ARG2, FUNC_NAME);
      len = scm_to_signed_integer (length, 0, xsock->recv_buffer_fill);
    }
  else
    {
      len = xsock->recv_buffer_fill;
    }
  svz_sock_reduce_recv (xsock, len);
  return scm_from_int (len);
}
#undef FUNC_NAME

/* This procedure returns the current remote address as a pair like 
   @code{(host . port)} with both entries in network byte order. If you pass
   the optional argument @var{address}, you can set the remote address of 
   the socket @var{sock}. */
#define FUNC_NAME "svz:sock:remote-address"
static SCM
guile_sock_remote_address (SCM sock, SCM address)
{
  svz_socket_t *xsock;
  uint16_t port;
  SCM pair;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  pair = scm_cons (scm_from_ulong (xsock->remote_addr), 
		   scm_from_int ((int) xsock->remote_port));
  if (!SCM_UNBNDP (address))
    {
      SCM_ASSERT (scm_is_pair (address) && scm_is_integer (SCM_CAR (address))
                  && scm_is_integer (SCM_CDR (address)), address, SCM_ARG2, 
                  FUNC_NAME);
      port = scm_to_uint16 (SCM_CDR (address));
      xsock->remote_addr = scm_to_ulong (SCM_CAR (address));
      xsock->remote_port = (unsigned short) port;
    }
  return pair;
}
#undef FUNC_NAME

/* This procedure returns the current local address as a pair like 
   @code{(host . port)} with both entries in network byte order. If you pass
   the optional argument @var{address}, you can set the local address of 
   the socket @var{sock}. */
#define FUNC_NAME "svz:sock:local-address"
static SCM
guile_sock_local_address (SCM sock, SCM address)
{
  svz_socket_t *xsock;
  uint16_t port;
  SCM pair;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  pair = scm_cons (scm_from_ulong (xsock->local_addr), 
		   scm_from_int ((int) xsock->local_port));
  if (!SCM_UNBNDP (address))
    {
      SCM_ASSERT (scm_is_pair (address) && scm_is_integer (SCM_CAR (address))
                  && scm_is_integer (SCM_CDR (address)), address, SCM_ARG2, 
                  FUNC_NAME);
      port = scm_to_uint16 (SCM_CDR (address));
      xsock->local_addr = scm_to_ulong (SCM_CAR (address));
      xsock->local_port = (unsigned short) port;
    }
  return pair;
}
#undef FUNC_NAME

/* Return the given socket's @var{sock} parent and optionally set it to the
   socket @var{parent}. The procedure returns either a valid 
   @code{#<svz-socket>} object or an empty list. */
#define FUNC_NAME "svz:sock:parent"
static SCM
guile_sock_parent (SCM sock, SCM parent)
{
  SCM oparent = SCM_EOL;
  svz_socket_t *xsock, *xparent;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  if ((xparent = svz_sock_getparent (xsock)) != NULL)
    SCM_NEWSMOB (oparent, guile_svz_socket_tag, xparent);
  if (!SCM_UNBNDP (parent))
    {
      scm_assert_smob_type (guile_svz_socket_tag, parent);
      xparent = (svz_socket_t *) SCM_SMOB_DATA (parent);
      svz_sock_setparent (xsock, xparent);
    }
  return oparent;
}
#undef FUNC_NAME

/* Return the given socket's @var{sock} referrer and optionally set it to the
   socket @var{referrer}. The procedure returns either a valid 
   @code{#<svz-socket>} or an empty list. */
#define FUNC_NAME "svz:sock:referrer"
static SCM
guile_sock_referrer (SCM sock, SCM referrer)
{
  SCM oreferrer = SCM_EOL;
  svz_socket_t *xsock, *xreferrer;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  if ((xreferrer = svz_sock_getreferrer (xsock)) != NULL)
    SCM_NEWSMOB (oreferrer, guile_svz_socket_tag, xreferrer);
  if (!SCM_UNBNDP (referrer))
    {
      scm_assert_smob_type (guile_svz_socket_tag, referrer);
      xreferrer = (svz_socket_t *) SCM_SMOB_DATA (referrer);
      svz_sock_setreferrer (xsock, xreferrer);
    }
  return oreferrer;
}
#undef FUNC_NAME

/* This procedure returns the @code{#<svz-server>} object associated with the
   given argument @var{sock}. The optional argument @var{server} can be used
   to redefine this association and must be a valid @code{#<svz-server>}
   object. For a usual socket callback like @code{connect-socket} or
   @code{handle-request}, the association is already in place. But for sockets
   created by @code{(svz:sock:connect)}, you can use it in order to make the
   returned socket object part of a server. */
#define FUNC_NAME "svz:sock:server"
static SCM
guile_sock_server (SCM sock, SCM server)
{
  SCM oserver = SCM_EOL;
  svz_socket_t *xsock;
  svz_server_t *xserver;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  
  if ((xserver = svz_server_find (xsock->cfg)) != NULL)
    SCM_NEWSMOB (oserver, guile_svz_server_tag, xserver);
  if (!SCM_UNBNDP (server))
    {
      scm_assert_smob_type (guile_svz_server_tag, server);
      xserver = (svz_server_t *) SCM_SMOB_DATA (server);
      xsock->cfg = xserver->cfg;
    }
  return oserver;
}
#undef FUNC_NAME

/* Returns one of the @code{PROTO_TCP}, @code{PROTO_UDP}, @code{PROTO_ICMP}, 
   @code{PROTO_RAW} or @code{PROTO_PIPE} constants indicating the type of 
   the socket structure @var{sock}. If there is no protocol information 
   available the procedure returns @code{#f}. */
#define FUNC_NAME "svz:sock:protocol"
static SCM
guile_sock_protocol (SCM sock)
{
  svz_socket_t *xsock;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  return scm_from_int (xsock->proto);
}
#undef FUNC_NAME

/* This procedure schedules the socket @var{sock} for shutdown after all data
   within the send buffer queue has been sent. The user should issue this
   procedure call right *before* the last call to @code{(svz:sock:print)}. */
#define FUNC_NAME "svz:sock:final-print"
static SCM
guile_sock_final_print (SCM sock)
{
  svz_socket_t *xsock;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  xsock->flags |= SOCK_FLAG_FINAL_WRITE;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Turns the Nagle algorithm for the TCP socket @var{sock} on or off depending
   on the optional @var{enable} argument. Returns the previous state of this
   flag (@code{#f} if Nagle is active, @code{#t} otherwise). By default this 
   flag is switched off. This socket option is useful when dealing with small
   packet transfer in order to disable unnecessary delays. */
#define FUNC_NAME "svz:sock:no-delay"
static SCM
guile_sock_no_delay (SCM sock, SCM enable)
{
  svz_socket_t *xsock;
  int old = 0, set = 0;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  if (xsock->proto & PROTO_TCP)
    {
      if (!SCM_UNBNDP (enable))
        {
          SCM_ASSERT (scm_is_bool (enable) || scm_is_integer (enable), enable,
                      SCM_ARG2, FUNC_NAME);
          if ((scm_is_bool (enable) && scm_is_true (enable) != 0) ||
              (scm_is_integer (enable) && scm_to_int (enable) != 0))
	    set = 1;
	}
      if (svz_tcp_nodelay (xsock->sock_desc, set, &old) < 0)
	old = 0;
      else if (SCM_UNBNDP (enable))
	svz_tcp_nodelay (xsock->sock_desc, old, NULL);
    }
  return SCM_BOOL (old);
}
#undef FUNC_NAME

/* Returns @code{#t} if the given cell @var{sock} is an instance of a valid
   @code{#<svz-socket>}, otherwise @code{#f}. */
#define FUNC_NAME "svz:sock?"
static SCM
guile_sock_p (SCM sock)
{
  if (SCM_SMOB_PREDICATE (guile_svz_socket_tag, sock))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Returns @code{#t} if the given cell @var{server} is an instance of a valid
   @code{#<svz-server>}, otherwise @code{#f}. */
#define FUNC_NAME "svz:server?"
static SCM
guile_server_p (SCM server)
{
  if (SCM_SMOB_PREDICATE (guile_svz_server_tag, server))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Set the @code{disconnected-socket} member of the socket structure 
   @var{sock} to the Guile procedure @var{proc}. The given callback
   runs whenever the socket is lost for some external reason. The procedure 
   returns the previously set handler if there is one. */
#define FUNC_NAME "svz:sock:disconnected"
MAKE_SOCK_CALLBACK (guile_sock_disconnected_socket, 
                    guile_func_disconnected_socket,
                    disconnected_socket,
                    "disconnected")
#undef FUNC_NAME

/* Sets the @code{kicked-socket} callback of the given socket structure
   @var{sock} to the Guile procedure @var{proc} and returns any previously 
   set procedure.  This callback gets called whenever the socket gets 
   closed by Serveez intentionally.  */
#define FUNC_NAME "svz:sock:kicked"
MAKE_SOCK_CALLBACK (guile_sock_kicked_socket,
                    guile_func_kicked_socket,
                    kicked_socket, "kicked")
#undef FUNC_NAME

/* This procedure sets the @code{trigger-condition} callback for the socket
   structure @var{sock} to the Guile procedure @var{proc}.  It returns the
   previously set procedure if available.  The callback is run once every
   server loop indicating whether the @code{trigger} callback should be
   run or not. */
#define FUNC_NAME "svz:sock:trigger-condition"
MAKE_SOCK_CALLBACK (guile_sock_trigger_cond,
                    guile_func_trigger_cond,
                    trigger_cond, "trigger-condition")
#undef FUNC_NAME

/* Sets the @code{trigger} callback of the socket structure @var{sock} to
   the Guile procedure @var{proc} and returns any previously set procedure.
   The callback is run when the @code{trigger-condition} callback returned
   @code{#t}. */
#define FUNC_NAME "svz:sock:trigger"
MAKE_SOCK_CALLBACK (guile_sock_trigger_func,
                    guile_func_trigger_func,
                    trigger_func, "trigger")
#undef FUNC_NAME

/* This procedure sets the @code{idle} callback of the socket structure
   @var{sock} to the Guile procedure @var{proc}. It returns any previously
   set procedure. The callback is run by the periodic task scheduler when the
   @code{idle-counter} of the socket structure drops to zero. If this counter
   is not zero it gets decremented once a second. The @code{idle}
   callback can reset @code{idle-counter} to some value and thus can 
   re-schedule itself for a later task. */
#define FUNC_NAME "svz:sock:idle"
MAKE_SOCK_CALLBACK (guile_sock_idle_func,
                    guile_func_idle_func,
                    idle_func, "idle")
#undef FUNC_NAME

/* With this procedure you can setup the @code{check-oob-request} callback
   of the given socket structure @var{sock}.  The previous callback is 
   replaced by the @var{proc} procedure and will be returned if there was
   set any before.  The callback is run whenever urgent data (out-of-band)
   has been detected on the socket. */
#define FUNC_NAME "svz:sock:check-oob-request"
MAKE_SOCK_CALLBACK (guile_sock_check_request_oob,
                    guile_func_check_request_oob,
                    check_request_oob,
                    "check-oob-request")
#undef FUNC_NAME

/* This functions returns the socket structure @var{sock}'s current 
   @code{idle-counter} value. If the optional argument @var{counter} is
   given, the function sets the @code{idle-counter}. Please have a look at the
   @code{(svz:sock:idle)} procedure for the exact meaning of this value. */
#define FUNC_NAME "svz:sock:idle-counter"
static SCM
guile_sock_idle_counter (SCM sock, SCM counter)
{
  svz_socket_t *xsock;
  int ocounter;

  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  ocounter = xsock->idle_counter;
  if (!SCM_UNBNDP (counter))
    {
      SCM_ASSERT (scm_is_integer (counter), counter, SCM_ARG2, FUNC_NAME);
      xsock->idle_counter = scm_to_int (counter);
    }
  return scm_from_int (ocounter);
}
#undef FUNC_NAME

/* Returns a list of listening @code{#<svz-socket>} smobs to which the 
   given server instance @var{server} is currently bound, or an empty list
   if there is no such binding yet. */
#define FUNC_NAME "svz:server:listeners"
static SCM
guile_server_listeners (SCM server)
{
  svz_server_t *xserver = NULL;
  svz_array_t *listeners;
  char *str;
  unsigned long i;
  SCM list = SCM_EOL;

  /* Server instance name given ? */
  if ((str = guile_to_string (server)) != NULL)
    {
      xserver = svz_server_get (str);
      free (str);
    }
  /* Maybe server smob given. */
  if (xserver == NULL)
    {
      scm_assert_smob_type (guile_svz_server_tag, server);
      xserver = (svz_server_t *) SCM_SMOB_DATA (server);
    }

  /* Create a list of socket smobs for the server. */
  if ((listeners = svz_server_listeners (xserver)) != NULL)
    {
      SCM smob;
      for (i = 0; i < svz_array_size (listeners); i++)
        {
          SCM_NEWSMOB (smob, guile_svz_socket_tag, 
                       (svz_socket_t *) svz_array_get (listeners, i));
          list = scm_cons (smob, list);
        }
      svz_array_destroy (listeners);
    }
  return scm_reverse (list);
}
#undef FUNC_NAME

/* Returns a list of @code{#<svz-socket>} client smobs associated with
   the given server instance @var{server} in arbitrary order, or an
   empty list if there is no such client. */
#define FUNC_NAME "svz:server:clients"
static SCM
guile_server_clients (SCM server)
{
  svz_server_t *xserver = NULL;
  svz_array_t *clients;
  svz_socket_t *sock;
  char *str;
  unsigned long i;
  SCM list = SCM_EOL;

  /* If the server instance name is given, try to translate it. */
  if ((str = guile_to_string (server)) != NULL)
    {
      xserver = svz_server_get (str);
      free (str);
    }
  /* If the above failed it is possibly a real server smob. */
  if (xserver == NULL)
    {
      scm_assert_smob_type (guile_svz_server_tag, server);
      xserver = (svz_server_t *) SCM_SMOB_DATA (server);
    }

  /* Create a list of socket smobs for the server. */
  if ((clients = svz_server_clients (xserver)) != NULL)
    {
      SCM socket_smob;
      svz_array_foreach (clients, sock, i)
        {
          SCM_NEWSMOB (socket_smob, guile_svz_socket_tag, sock);
          list = scm_cons (socket_smob, list);
        }
      svz_array_destroy (clients);
    }
  return list;
}
#undef FUNC_NAME

/* A user interface to the portmap service, which establishes a mapping
   between the triple [@var{prognum},@var{versnum},@var{protocol}] and 
   @var{port} on the machine's portmap service. The value of @var{protocol}
   is most likely @code{IPPROTO_UDP} or @code{IPPROTO_TCP}.
   If the user omits @var{protocol} and @var{port}, the procedure destroys
   all mapping between the triple [@var{prognum},@var{versnum},*] and ports
   on the machine's portmap service. */
#define FUNC_NAME "portmap"
SCM
scm_portmap (SCM prognum, SCM versnum, SCM protocol, SCM port)
{
  SCM_ASSERT (scm_is_integer (prognum), prognum, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_integer (versnum), prognum, SCM_ARG2, FUNC_NAME);

  if (SCM_UNBNDP (protocol) && SCM_UNBNDP (port))
    {
      if (!pmap_unset (scm_to_ulong(prognum), scm_to_ulong (versnum)))
	scm_syserror_msg (FUNC_NAME, "~A: pmap_unset ~A ~A",
                          scm_list_n (scm_from_locale_string (strerror (errno)),
				      prognum, versnum, SCM_UNDEFINED), 
			  errno);
    }
  else
    {
      SCM_ASSERT (scm_is_integer (protocol), protocol, SCM_ARG3, FUNC_NAME);
      SCM_ASSERT (scm_is_integer (port), port, SCM_ARG4, FUNC_NAME);

      if (!pmap_set (scm_to_ulong (prognum), scm_to_ulong (versnum), 
                     scm_to_int (protocol), scm_to_ushort (port)))
	scm_syserror_msg (FUNC_NAME, "~A: pmap_set ~A ~A ~A ~A",
                          scm_list_n (scm_from_locale_string (strerror (errno)),
				      prognum, versnum, protocol, port, 
				      SCM_UNDEFINED), errno);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Callback wrapper for coserver responses. */
static int
guile_coserver_callback (char *res, SCM callback, SCM arg)
{
  int ret = -1;

  /* If successfully done, run the given Guile procedure. */
  if (res != NULL)
    {
      /* Run procedure with one argument only. */
      if (SCM_UNBNDP (arg))
	guile_call (callback, 1, scm_makfrom0str (res));
      /* Run procedure with both arguments. */
      else
	guile_call (callback, 2, scm_makfrom0str (res), arg);
      ret = 0;
    }

  /* Pass the values to garbage collection again. */
  if (!SCM_UNBNDP (arg))
    scm_gc_unprotect_object (arg);
  scm_gc_unprotect_object (callback);
  return ret;
}

/* This procedure enqueues the @var{host} string argument into the internal
   DNS coserver queue. When the coserver responds, the Guile procedure
   @var{callback} is run as @code{(callback addr arg)}. The @var{addr} 
   argument passed to the callback is a string representing the appropriate 
   IP address for the given hostname @var{host}. If you omit the optional 
   argument @var{arg} it is run as @code{(callback addr)} only. The @var{arg}
   argument may be necessary if you need to have the callback procedure 
   in a certain context. */ 
#define FUNC_NAME "svz:coserver:dns"
SCM
guile_coserver_dns (SCM host, SCM callback, SCM arg)
{
  char *request;

  /* Check argument list first. */
  SCM_ASSERT (scm_is_string (host), host, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_PROCEDUREP (callback), callback, SCM_ARG2, FUNC_NAME);

  /* Convert hostname into C string. */
  request = guile_to_string (host);

  /* Protect callback (Guile procedure) and arg (any Guile cell) from
     garbage collection meanwhile. */
  scm_gc_protect_object (callback);
  if (!SCM_UNBNDP (arg))
    scm_gc_protect_object (arg);

  /* Enqueue coserver request. */
  svz_coserver_dns (request, guile_coserver_callback, callback, arg);
  free (request);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* This Guile procedure enqueues the given @var{addr} argument which must be
   an IP address in network byte order into the internal reverse DNS coserver
   queue. When the coserver responds, the Guile procedure @var{callback} is
   run as @code{(callback host arg)} where @var{host} is the hostname of the
   requested IP address @var{addr}. The last argument @var{arg} is 
   optional. */
#define FUNC_NAME "svz:coserver:reverse-dns"
SCM
guile_coserver_rdns (SCM addr, SCM callback, SCM arg)
{
  unsigned long ip;

  /* Check argument list first. */
  SCM_ASSERT (SCM_INUMP (addr), addr, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_PROCEDUREP (callback), callback, SCM_ARG2, FUNC_NAME);

  /* Convert IP address into C long value. */
  ip = scm_to_ulong (addr);

  /* Protect callback (Guile procedure) and arg (any Guile cell) from
     garbage collection meanwhile. */
  scm_gc_protect_object (callback);
  if (!SCM_UNBNDP (arg))
    scm_gc_protect_object (arg);

  /* Enqueue coserver request. */
  svz_coserver_rdns (ip, guile_coserver_callback, callback, arg);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* This procedure enqueues the given @code{#<svz-socket>} @var{sock} into the
   internal ident coserver queue. When the coserver responds, it runs the
   Guile procedure @var{callback} as @code{(callback user arg)} where
   @var{user} is the corresponding username for the client connection 
   @var{sock}. The @var{arg} argument is optional. */
#define FUNC_NAME "svz:coserver:ident"
SCM
guile_coserver_ident (SCM sock, SCM callback, SCM arg)
{
  svz_socket_t *xsock;

  /* Check argument list first. */
  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  SCM_ASSERT (SCM_PROCEDUREP (callback), callback, SCM_ARG2, FUNC_NAME);

  /* Protect callback (Guile procedure) and arg (any Guile cell) from
     garbage collection meanwhile. */
  scm_gc_protect_object (callback);
  if (!SCM_UNBNDP (arg))
    scm_gc_protect_object (arg);

  /* Enqueue coserver request. */
  svz_coserver_ident (xsock, guile_coserver_callback, callback, arg);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* The given argument @var{ident} must be a pair of numbers where the
   car is a @code{#<svz-socket>}'s identification number and the cdr the 
   version number. The procedure returns either the identified 
   @code{#<svz-socket>} or @code{#f} if the given combination is not 
   valid anymore. */
#define FUNC_NAME "svz:sock:find"
SCM
guile_sock_find (SCM ident)
{
  int version, id;
  svz_socket_t *sock;

  SCM_ASSERT (scm_is_pair (ident) && SCM_INUMP (SCM_CAR (ident)) 
              && SCM_INUMP (SCM_CDR (ident)), ident, SCM_ARG1, FUNC_NAME);
  id = scm_to_int (SCM_CAR (ident));
  version = scm_to_int (SCM_CDR (ident));
  if ((sock = svz_sock_find (id, version)) != NULL)
    SCM_RETURN_NEWSMOB (guile_svz_socket_tag, sock);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

/* This procedure returns a pair of numbers identifying the given 
   @code{#<svz-socket>} @var{sock} which can be passed to 
   @code{(svz:sock:find)}. This may be necessary when you are passing
   a @code{#<svz-socket>} through coserver callback arguments in order to
   verify that the passed @code{#<svz-socket>} is still valid when the
   coserver callback runs. */
#define FUNC_NAME "svz:sock:ident"
SCM
guile_sock_ident (SCM sock)
{
  svz_socket_t *xsock;
  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  return scm_cons (scm_from_int (xsock->id), scm_from_int (xsock->version));
}
#undef FUNC_NAME

/* This procedure returns either a binary smob containing a data block read
   from the open input port @var{port} with a maximum number of @var{size}
   bytes or the end-of-file object if the underlying ports end has been 
   reached.  The size of the returned binary smob may be less than the 
   requested size @var{size} if it exceed the current size of the given port
   @var{port}.  The procedure throws an exception if an error occurred while
   reading from the port. */
#define FUNC_NAME "svz:read-file"
static SCM
guile_read_file (SCM port, SCM size)
{
  int fdes, len, ret;
  void *data;

  /* Check argument list. */
  SCM_ASSERT (SCM_NIMP (port) && SCM_FPORTP (port) && SCM_OPINFPORTP (port),
              port, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_integer (size), size, SCM_ARG2, FUNC_NAME);

  /* Get underlying file descriptor. */
  fdes = SCM_FPORT_FDES (port);

  if ((len = scm_to_int (size)) <= 0)
    scm_out_of_range (FUNC_NAME, size);

  /* Allocate necessary data. */
  data = (unsigned char *) scm_gc_malloc (len, "svz-binary-data");

  /* Read from file descriptor and evaluate return value. */
  if ((ret = read (fdes, data, len)) < 0)
    {
      scm_gc_free (data, len, "svz-binary-data");
      scm_syserror_msg (FUNC_NAME, "~A: read ~A ~A",
			scm_list_n (scm_makfrom0str (strerror (errno)),
				    scm_from_int (fdes), size, SCM_UNDEFINED),
			errno);
    }
  else if (ret == 0)
    {
      scm_gc_free (data, len, "svz-binary-data");
      return SCM_EOF_VAL;
    }
  else if (ret != len)
    {
      data = (unsigned char *) 
	scm_gc_realloc (data, len, ret, "svz-binary-data");
    }

  /* Finally return binary smob. */
  return guile_garbage_to_bin (data, ret);
}
#undef FUNC_NAME

/* This procedure expects a TCP @code{#<svz-socket>} in @var{sock} and an 
   exact number or single character in @var{oob}.  The byte in @var{oob} 
   is sent as urgent (out-of-band) data through the underlying TCP stream.
   The procedure returns @code{#t} on successful completion and otherwise
   (either it failed to send the byte or the passed socket is not a TCP 
   socket) @code{#f}. */
#define FUNC_NAME "svz:sock:send-oob"
static SCM
guile_sock_send_oob (SCM sock, SCM oob)
{
  svz_socket_t *xsock;
  int ret = -1;

  /* Check the arguments. */
  scm_assert_smob_type (guile_svz_socket_tag, sock);
  xsock = (svz_socket_t *) SCM_SMOB_DATA (sock);
  SCM_ASSERT (scm_is_integer (oob) || SCM_CHARP (oob), oob, SCM_ARG2,
              FUNC_NAME);

  /* Send the oob byte through TCP sockets only. */
  if (xsock->proto & PROTO_TCP)
    {
      xsock->oob = (unsigned char)
	(SCM_CHARP (oob) ? SCM_CHAR (oob) :
	 (unsigned char) scm_to_uint8 (oob));
      ret = svz_tcp_send_oob (xsock);
    }
  return ((ret < 0) ? SCM_BOOL_F : SCM_BOOL_T);
}
#undef FUNC_NAME

/* Initialize the API function calls supported by Guile. */
void
guile_api_init (void)
{
  scm_c_define_gsubr ("portmap", 2, 2, 0, scm_portmap);
  scm_c_define ("IPPROTO_UDP", scm_from_int (IPPROTO_UDP));
  scm_c_define ("IPPROTO_TCP", scm_from_int (IPPROTO_TCP));
  scm_c_define ("PROTO_TCP", scm_from_int (PROTO_TCP));
  scm_c_define ("PROTO_UDP", scm_from_int (PROTO_UDP));
  scm_c_define ("PROTO_ICMP", scm_from_int (PROTO_ICMP));
  scm_c_define ("PROTO_RAW", scm_from_int (PROTO_RAW));
  scm_c_define ("PROTO_PIPE", scm_from_int (PROTO_PIPE));
  scm_c_define ("KICK_FLOOD", scm_from_int (0));
  scm_c_define ("KICK_QUEUE", scm_from_int (1));
  scm_c_define_gsubr ("svz:sock:connect", 2, 1, 0, guile_sock_connect);
  scm_c_define_gsubr ("svz:sock:parent", 1, 1, 0, guile_sock_parent);
  scm_c_define_gsubr ("svz:sock:referrer", 1, 1, 0, guile_sock_referrer);
  scm_c_define_gsubr ("svz:sock:server", 1, 1, 0, guile_sock_server);
  scm_c_define_gsubr ("svz:sock:protocol", 1, 0, 0, guile_sock_protocol);
  scm_c_define_gsubr ("svz:sock:final-print", 1, 0, 0, guile_sock_final_print);
  scm_c_define_gsubr ("svz:sock:no-delay", 1, 1, 0, guile_sock_no_delay);
  scm_c_define_gsubr ("svz:sock:ident", 1, 0, 0, guile_sock_ident);
  scm_c_define_gsubr ("svz:sock:find", 1, 0, 0, guile_sock_find);
  scm_c_define_gsubr ("svz:sock?", 1, 0, 0, guile_sock_p);
  scm_c_define_gsubr ("svz:server?", 1, 0, 0, guile_server_p);
  scm_c_define_gsubr ("svz:server:listeners", 1, 0, 0, guile_server_listeners);
  scm_c_define_gsubr ("svz:server:clients", 1, 0, 0, guile_server_clients);
  scm_c_define_gsubr ("svz:sock:send-buffer", 1, 0, 0, guile_sock_send_buffer);
  scm_c_define_gsubr ("svz:sock:send-buffer-size",
		      1, 1, 0, guile_sock_send_buffer_size);
  scm_c_define_gsubr ("svz:sock:receive-buffer",
		      1, 0, 0, guile_sock_receive_buffer);
  scm_c_define_gsubr ("svz:sock:receive-buffer-size",
		      1, 1, 0, guile_sock_receive_buffer_size);
  scm_c_define_gsubr ("svz:sock:receive-buffer-reduce",
		      1, 1, 0, guile_sock_receive_buffer_reduce);
  scm_c_define_gsubr ("svz:sock:remote-address",
		      1, 1, 0, guile_sock_remote_address);
  scm_c_define_gsubr ("svz:sock:local-address",
		      1, 1, 0, guile_sock_local_address);
  scm_c_define_gsubr ("svz:sock:idle-counter",
		      1, 1, 0, guile_sock_idle_counter);
  scm_c_define_gsubr ("svz:coserver:dns",
		      2, 1, 0, guile_coserver_dns);
  scm_c_define_gsubr ("svz:coserver:reverse-dns",
		      2, 1, 0, guile_coserver_rdns);
  scm_c_define_gsubr ("svz:coserver:ident",
		      2, 1, 0, guile_coserver_ident);
  scm_c_define_gsubr ("svz:read-file", 2, 0, 0, guile_read_file);
  scm_c_define_gsubr ("svz:sock:send-oob", 2, 0, 0, guile_sock_send_oob);
  DEFINE_SOCK_CALLBACK ("svz:sock:trigger", guile_sock_trigger_func);
  DEFINE_SOCK_CALLBACK ("svz:sock:disconnected", 
                        guile_sock_disconnected_socket);
  DEFINE_SOCK_CALLBACK ("svz:sock:kicked", guile_sock_kicked_socket);
  DEFINE_SOCK_CALLBACK ("svz:sock:idle", guile_sock_idle_func);
  DEFINE_SOCK_CALLBACK ("svz:sock:check-oob-request", 
                        guile_sock_check_request_oob);
}

/* Finalize the API functions. */
void
guile_api_finalize (void)
{
}

#endif /* ENABLE_GUILE_SERVER */
