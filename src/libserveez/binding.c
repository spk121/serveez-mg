/*
 * binding.c - server to port binding implementation
 *
 * Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: binding.c,v 1.22 2002/01/24 18:15:58 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#define _GNU_SOURCE
#include <assert.h>
#include <string.h>
#include <sys/types.h>

#ifndef __MINGW32__
# include <sys/socket.h>
#endif

#ifdef __MINGW32__
# include <winsock2.h>
#endif

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/core.h"
#include "libserveez/server.h"
#include "libserveez/server-core.h"
#include "libserveez/portcfg.h"
#include "libserveez/server-socket.h"
#include "libserveez/binding.h"

/*
 * Return a static text representation of the server instance's @var{server}
 * current port configuration bindings.
 */
char *
svz_server_bindings (svz_server_t *server)
{
  static char text[256];
  svz_socket_t *sock;
  svz_array_t *bindings;
  svz_binding_t *binding;
  int i;

  /* Clear text. */
  text[0] = '\0'; 

  /* Go through the list of socket structures. */
  svz_sock_foreach_listener (sock)
    {
      /* The server in the array of servers ? */
      if ((bindings = svz_binding_find_server (sock, server)) != NULL)
	{
	  /* Yes. Get port configurations. */
	  svz_array_foreach (bindings, binding, i)
	    strcat (text, svz_portcfg_text (binding->port));
	  svz_array_destroy (bindings);

	  /* Append a white space. */
	  strcat (text, " ");
	}
    }

  /* Remove trailing white space. */
  if (strlen (text))
    text[strlen (text) - 1] = '\0';
  return text;
}

/*
 * Return an array of port configurations to which the given server instance
 * @var{server} is currently bound to or @code{NULL} if there is no such 
 * binding. The caller is responsible for freeing the returned array by 
 * running @code{svz_array_destroy()}.
 */
svz_array_t *
svz_server_portcfgs (svz_server_t *server)
{
  svz_array_t *ports = svz_array_create (1, NULL);
  svz_binding_t *binding;
  svz_array_t *bindings;
  svz_socket_t *sock;
  int i;

  svz_sock_foreach_listener (sock)
    if ((bindings = svz_binding_find_server (sock, server)) != NULL)
      {
	svz_array_foreach (bindings, binding, i)
	  svz_array_add (ports, binding->port);
	svz_array_destroy (bindings);
      }
  return svz_array_destroy_zero (ports);
}

/*
 * Return an array of listening socket structures to which the given server
 * instance @var{server} is currently bound to or @code{NULL} if there is 
 * no such binding. The calling function is reponsible for destroying the
 * returned array via @code{svz_array_destroy()}.
 */
svz_array_t *
svz_server_listeners (svz_server_t *server)
{
  svz_array_t *listeners = svz_array_create (1, NULL);
  svz_socket_t *sock;

  svz_sock_foreach_listener (sock)
    if (svz_binding_contains_server (sock, server))
      svz_array_add (listeners, sock);
  return svz_array_destroy_zero (listeners);
}

/*
 * This function checks if the given server instance @var{server} is
 * bound to the listening socket structure @var{sock} and returns non-zero 
 * if it is the only server instance bound to this socket. Otherwise
 * the routine returns zero.
 */
int
svz_server_single_listener (svz_server_t *server, svz_socket_t *sock)
{
  if (server != NULL && sock != NULL &&          /* validate arguments       */
      sock->flags & SOCK_FLAG_LISTENING &&       /* is a listener ?          */
      sock->port != NULL &&                      /* has port configuration ? */
      svz_binding_contains_server (sock, server) && /* bound to ?            */
      svz_array_size (sock->data) == 1)          /* the only one ?           */
    return 1;
  return 0;
}

/*
 * Returns a socket structure representing a listening server socket with 
 * the port configuration @var{port}. If there is no such socket with this
 * kind of port configuration yet then @code{NULL} is returned.
 */
svz_socket_t *
svz_sock_find_portcfg (svz_portcfg_t *port)
{
  svz_socket_t *sock;
  
  svz_sock_foreach_listener (sock)
    if (svz_portcfg_equal (sock->port, port) & (PORTCFG_EQUAL | PORTCFG_MATCH))
      return sock;
  return NULL;
}

/*
 * This functions goes through the list of listening server socket 
 * structures and returns an array of matching socket structures for the
 * given port configuration @var{port}. The caller is responsible for
 * freeing the array by running @code{svz_array_destroy()}. If there are
 * no such listening server socket structures @code{NULL} is returned.
 */
svz_array_t *
svz_sock_find_portcfgs (svz_portcfg_t *port)
{
  svz_array_t *listeners = svz_array_create (1, NULL);
  svz_socket_t *sock;
  
  svz_sock_foreach_listener (sock)
    if (svz_portcfg_equal (sock->port, port) & (PORTCFG_EQUAL | PORTCFG_MATCH))
      svz_array_add (listeners, sock);
  return svz_array_destroy_zero (listeners);
}

/*
 * Creates and returns a listening server socket structure. The kind of
 * listener which gets created depends on the given port configuration 
 * @var{port} which must be a duplicated copy of one out of the list of
 * known port configurations. On success the function enqueues the
 * returned socket structure and assigns the port configuration. Initially
 * there are no bindings. In case of an error the given port configuration
 * is freed and @code{NULL} is returned.
 */
svz_socket_t *
svz_sock_bind_port (svz_portcfg_t *port)
{
  svz_socket_t *sock;

  /* Try creating a server socket. */
  if ((sock = svz_server_create (port)) != NULL)
    {
      /* Enqueue the server socket and put the port configuration into
	 the socket structure. */
      svz_sock_enqueue (sock);
      sock->port = port;
      return sock;
    }

  /* Could not create this port configuration listener. */
  svz_portcfg_free (port);
  return NULL;
}

/*
 * Bind the server instance @var{server} to the port configuration 
 * @var{port} if possible. Return non-zero on errors otherwise zero. It
 * might occur that a single server is bound to more than one network port 
 * if e.g. the TCP/IP address is specified by "*" since this gets expanded
 * to the known list of interfaces.
 */
int
svz_server_bind (svz_server_t *server, svz_portcfg_t *port)
{
  svz_array_t *ports;
  svz_socket_t *sock;
  svz_portcfg_t *copy, *portcfg;
  unsigned long n, i;

  /* First expand the given port configuration. */
  ports = svz_portcfg_expand (port);
  svz_array_foreach (ports, copy, n)
    {
      /* Prepare port configuration. */
      svz_portcfg_prepare (copy);

      /* Find appropriate socket structure for this port configuration. */
      if ((sock = svz_sock_find_portcfg (copy)) == NULL)
	{
	  if ((sock = svz_sock_bind_port (copy)) != NULL)
	    svz_sock_add_server (sock, server, copy);
	}
      /* Port configuration already exists. */
      else
	{
	  /* Is this a more general network port? Is the new port an 
	     INADDR_ANY binding and the picked one not? */
	  portcfg = sock->port;
	  if ((copy->flags & PORTCFG_FLAG_ANY) &&
	      !(portcfg->flags & PORTCFG_FLAG_ANY))
	    {
	      svz_array_t *sockets = svz_sock_find_portcfgs (port);
	      svz_array_t *bindings = NULL;
	      svz_socket_t *xsock;

	      /* Join the bindings of the previous listeners and destroy
		 these at once. */
	      svz_log (LOG_NOTICE, "destroying previous bindings\n");
	      svz_array_foreach (sockets, xsock, i)
		{
		  bindings = svz_binding_join (bindings, xsock);
		  svz_sock_shutdown (xsock);
		}
	      svz_array_destroy (sockets);

	      /* Create a fresh listener. */
	      if ((sock = svz_sock_bind_port (copy)) != NULL)
		{
		  sock->data = bindings;
		  svz_sock_add_server (sock, server, copy);
		}
	      else
		{
		  svz_array_destroy (bindings);
		}
	    }
	  /* No. This is either a specific network interface or both have
	     an INADDR_ANY binding. */
	  else
	    svz_sock_add_server (sock, server, copy);
	}
    }

  /* Now we can destroy the expanded port configuration array. */
  svz_array_destroy (ports);
  return 0;
}

/*
 * Remove the given server instance @var{server} entirely from the list
 * of enqueued sockets. This means to delete it from each server socket on
 * the one hand and to shutdown every child client spawned from this server
 * on the other hand.
 */
void
svz_server_unbind (svz_server_t *server)
{
  svz_socket_t *sock, *parent;

  /* Go through all enqueued sockets. */
  svz_sock_foreach (sock)
    {
      /* Client structures. */
      if (!(sock->flags & SOCK_FLAG_LISTENING) && 
	  (parent = svz_sock_getparent (sock)) != NULL)
	{
	  /* If the parent of a client is the given servers child
	     then also shutdown this client. */
	  if (parent->flags & SOCK_FLAG_LISTENING && parent->port && 
	      parent->data && svz_binding_contains_server (parent, server))
	    svz_sock_schedule_for_shutdown (sock);
	}
    }

  /* Go through all enqueued sockets once more. */
  svz_sock_foreach_listener (sock)
    {
      /* Delete the server and shutdown the socket structure if
	 there are no more servers left. */
      if (svz_sock_del_server (sock, server) == 0)
	svz_sock_schedule_for_shutdown (sock);
    }
}

/*
 * Creates a bind structure. The binding contains the given server instance
 * @var{server} and the port configuration @var{port}. The caller is 
 * responsible for freeing the returned pointer.
 */
svz_binding_t *
svz_binding_create (svz_server_t *server, svz_portcfg_t *port)
{
  svz_binding_t *binding = svz_malloc (sizeof (svz_binding_t));
  binding->server = server;
  binding->port = port;
  return binding;
}

/*
 * Destroys the given binding @var{binding}. This includes the explicit 
 * destruction of the port configuration.  If @var{binding} is @code{NULL}
 * no operation is performed.
 */
void
svz_binding_destroy (svz_binding_t *binding)
{
  if (binding != NULL)
    {
      svz_portcfg_free (binding->port);
      svz_free (binding);
    }
}

/*
 * This function checks whether the server instance binding @var{binding}
 * is part of one of the bindings in the array @var{bindings} and returns 
 * non-zero if so. Otherwise zero is returned.
 */
int
svz_binding_contains (svz_array_t *bindings, svz_binding_t *binding)
{
  svz_binding_t *search;
  unsigned long i;

  svz_array_foreach (bindings, search, i)
    if (search->server == binding->server)
      if (svz_portcfg_equal (search->port, binding->port) == PORTCFG_EQUAL)
	return 1;
  return 0;
}

/*
 * This function adds the bindings stored in the listening server socket 
 * structure @var{sock} to the binding array @var{bindings} and returns the
 * resulting array. If @var{bindings} is @code{NULL} a new array is created.
 * If the socket structure @var{sock} is not a listening server socket 
 * structure no operation is performed.
 */
svz_array_t *
svz_binding_join (svz_array_t *bindings, svz_socket_t *sock)
{
  svz_array_t *old = svz_sock_bindings (sock);
  svz_binding_t *binding;
  unsigned long i;

  /* Is this a listening server socket ? */
  if (!((sock->flags & SOCK_FLAG_LISTENING) && (sock->port != NULL)))
    return bindings;

  /* Create an array if necessary. */
  if (bindings == NULL)
    bindings = svz_array_create (1, (svz_free_func_t) svz_binding_destroy);

  /* Join both arrays. */
  svz_array_foreach (old, binding, i)
    if (!svz_binding_contains (bindings, binding))
      {
	svz_server_t *server = binding->server;
	svz_portcfg_t *port = svz_portcfg_dup (binding->port);
	svz_array_add (bindings, svz_binding_create (server, port));
      }

  /* Destroy the old bindings. */
  svz_array_destroy (old);

  /* Invalidate the binding array. */
  sock->data = NULL;
  return bindings;
}

/*
 * Searches through the bindings of the given listening server socket 
 * structure @var{sock} and checks whether the server instance @var{server}
 * is bound to this socket structure. The function returns these binding and
 * returns @code{NULL} otherwise. The caller is responsible for freeing
 * the returned array.
 */
svz_array_t *
svz_binding_find_server (svz_socket_t *sock, svz_server_t *server)
{
  svz_array_t *bindings = svz_array_create (1, NULL);
  svz_binding_t *binding;
  unsigned long i;

  svz_array_foreach (sock->data, binding, i)
    if (binding->server == server)
      svz_array_add (bindings, binding);
  return svz_array_destroy_zero (bindings);
}

/*
 * This function checks whether the given server instance @var{server} is
 * bound to the server socket structure @var{sock}. Returns zero if not and
 * non-zero otherwise.
 */
int
svz_binding_contains_server (svz_socket_t *sock, svz_server_t *server)
{
  svz_binding_t *binding;
  unsigned long i;

  svz_array_foreach (sock->data, binding, i)
    if (binding->server == server)
      return 1;
  return 0;
}

/*
 * Goes through the listening server sockets @var{sock} bindings and checks
 * whether it contains the given binding consisting of @var{server} and
 * @var{port}. If there is no such binding yet @code{NULL} is returned
 * otherwise the appropriate binding.
 */
svz_binding_t *
svz_binding_find (svz_socket_t *sock, 
		  svz_server_t *server, svz_portcfg_t *port)
{
  svz_binding_t *binding;
  unsigned long i;

  svz_array_foreach (sock->data, binding, i)
    if (binding->server == server)
      if (svz_portcfg_equal (binding->port, port) == PORTCFG_EQUAL)
	return binding;
  return NULL;
}

/*
 * This function attaches the given server instance @var{server} to the
 * listening socket structure @var{sock}.  It returns zero on success and
 * non-zero if the server is already bound to the socket.
 */
int
svz_sock_add_server (svz_socket_t *sock, 
		     svz_server_t *server, svz_portcfg_t *port)
{
  svz_binding_t *binding = svz_binding_create (server, port);

  /* Create server array if necessary. */
  if (sock->data == NULL)
    {
      sock->data = svz_array_create (1, (svz_free_func_t) svz_binding_destroy);
      svz_array_add (sock->data, binding);
      return 0;
    }
  /* Attach a server/port binding to a single listener only once. */
  else if (svz_binding_find (sock, server, port) == NULL)
    {
      /* Extend the server array. */
      svz_array_add (sock->data, binding);
      return 0;
    }
  /* Binding already done. */
  svz_log (LOG_WARNING, "skipped duplicate binding of `%s'\n", server->name);
  svz_binding_destroy (binding);
  return -1;
}

/*
 * Removes the server instance @var{server} from the listening socket 
 * structure @var{sock} and returns the remaining number of servers bound
 * to the socket structure.
 */
int
svz_sock_del_server (svz_socket_t *sock, svz_server_t *server)
{
  svz_binding_t *binding;
  unsigned long i;

  svz_array_foreach (sock->data, binding, i)
    if (binding->server == server)
      {
	svz_binding_destroy (binding);
	svz_array_del (sock->data, i);
	i--;
      }
  return svz_array_size (sock->data);
}

/*
 * Returns the binding array of the listening server socket structure 
 * @var{sock} or @code{NULL} if there are no such bindings.
 */
svz_array_t *
svz_sock_bindings (svz_socket_t *sock)
{
  if (sock && sock->flags & SOCK_FLAG_LISTENING && sock->port != NULL)
    return sock->data;
  return NULL;
}

/*
 * Returns the array of server instances bound to the listening socket 
 * structure @var{sock} or @code{NULL} if there are no bindings. The caller
 * is responsible for freeing the returned array by running 
 * @code{svz_array_destroy()}.
 */
svz_array_t *
svz_sock_servers (svz_socket_t *sock)
{
  svz_array_t *servers = svz_array_create (1, NULL);
  svz_array_t *bindings = svz_sock_bindings (sock);
  svz_binding_t *binding;
  unsigned long i;

  svz_array_foreach (bindings, binding, i)
    svz_array_add (servers, binding->server);
  return svz_array_destroy_zero (servers);
}

/*
 * This is the accept filter for the listening server socket structures 
 * @var{sock} with pipe port configurations. It returns all bindings. The 
 * caller is responsible for freeing the returned array by running 
 * @code{svz_array_destroy()}.
 */
svz_array_t *
svz_binding_filter_pipe (svz_socket_t *sock) 
{
  svz_array_t *filter = svz_array_create (1, NULL);
  svz_array_t *bindings = sock->data;
  svz_binding_t *binding;
  unsigned long i;

  svz_array_foreach (bindings, binding, i)
    svz_array_add (filter, binding);
  return svz_array_destroy_zero (filter);
}

/*
 * This is the accept filter for the listening server socket structures 
 * @var{sock} with network port configurations. It returns the bindings
 * allowed to be accepted. The caller is responsible for freeing the returned 
 * array by running @code{svz_array_destroy()}. Which of the bindings are 
 * allowed depends on the network interface address @var{addr} and the 
 * network port @var{port}.
 */
svz_array_t *
svz_binding_filter_net (svz_socket_t *sock, 
			unsigned long addr, unsigned short port)
{
  svz_array_t *filter = svz_array_create (1, NULL);
  svz_array_t *bindings = sock->data;
  struct sockaddr_in *portaddr;
  svz_binding_t *binding;
  unsigned long i;

  /* Go through all bindings. */
  svz_array_foreach (bindings, binding, i)
    {
      portaddr = svz_portcfg_addr (binding->port);
#if DEVEL
      printf ("portaddr: %s == ", svz_inet_ntoa (portaddr->sin_addr.s_addr));
      printf ("%s\n", svz_inet_ntoa (addr));
      printf ("port: %u == %u\n", ntohs (portaddr->sin_port), ntohs (port));
#endif
      if (portaddr->sin_addr.s_addr == addr ||
	  binding->port->flags & (PORTCFG_FLAG_ANY | PORTCFG_FLAG_DEVICE)) 
	{
#if DEVEL
	  printf ("addr ok\n");
#endif
	  if (binding->port->proto & (PROTO_RAW | PROTO_ICMP) || 
	      portaddr->sin_port == port) 
	    {
#if DEVEL
	      printf ("port ok\n");
#endif
	      svz_array_add (filter, binding);
	    }
	}
    }
  return svz_array_destroy_zero (filter);
}

/*
 * This is the main filter routine running either 
 * @code{svz_binding_filter_net()} or @code{svz_binding_filter_pipe()} 
 * depending on the type of port configuration the given socket @var{sock}
 * contains.
 */
svz_array_t *
svz_binding_filter (svz_socket_t *sock)
{
  unsigned long addr;
  unsigned short port;

  if (sock->proto & PROTO_PIPE)
    return svz_binding_filter_pipe (sock);
  if (svz_sock_local_info (sock, &addr, &port))
    return NULL;
  return svz_binding_filter_net (sock, addr, port);
}
