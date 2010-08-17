/*
 * coserver.c - basic internal coserver routines
 *
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: coserver.c,v 1.31 2003/06/14 14:58:00 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>             /* assert */
#include <stdio.h>              /* snprintf, FILE *, fdopen, fgets, fflush, 
                                   fclose */
#include <stdlib.h>             /* exit */
#include <string.h>             /* strcpy, memmove */
#include <time.h>               /* time */
#include <unistd.h>             /* close, pipe, fork, dup2 */
#include <signal.h>             /* kill, signal, SIGKILL, SIGTERM, SIGINT */
#include <arpa/inet.h>          /* ntohs */
#include <sys/wait.h>           /* waitpid, WNOHANG */

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/core.h"
#include "libserveez/hash.h"
#include "libserveez/array.h"
#include "libserveez/pipe-socket.h"
#include "libserveez/server-core.h"
#include "libserveez/coserver/coserver.h"

/* coserver-TODO: include header here */
#include "dns.h"
#include "reverse-dns.h"
#include "ident.h"

#define COSERVER_PACKET_BOUNDARY '\n' /* packet boundary */
#define COSERVER_ID_BOUNDARY     ':'  /* id boundary */

/*
 * Both of these variables are for storing the given callbacks which get
 * called when the coservers delivered some result.
 */
static unsigned svz_coserver_callback_id = 1;
static svz_hash_t *svz_coserver_callbacks = NULL;

/* coserver-TODO: 
   place an appropiate wrapper function here */

/*
 * This is a wrapper function for the reverse DNS lookup coserver.
 */
void
svz_coserver_rdns_invoke (unsigned long ip, 
			  svz_coserver_handle_result_t cb, 
			  svz_coserver_args_t)
{
  svz_coserver_send_request (COSERVER_REVERSE_DNS, 
			     svz_inet_ntoa (ip), cb, arg0, arg1);
}

/*
 * Wrapper for the DNS coserver.
 */
void
svz_coserver_dns_invoke (char *host, 
			 svz_coserver_handle_result_t cb, 
			 svz_coserver_args_t)
{
  svz_coserver_send_request (COSERVER_DNS, host, cb, arg0, arg1);
}

/*
 * Wrapper for the ident coserver.
 */
void
svz_coserver_ident_invoke (svz_socket_t *sock, 
			   svz_coserver_handle_result_t cb, 
			   svz_coserver_args_t)
{
  char buffer[COSERVER_BUFSIZE];
  snprintf (buffer, COSERVER_BUFSIZE, "%s:%u:%u",
            svz_inet_ntoa (sock->remote_addr),
            ntohs (sock->remote_port), ntohs (sock->local_port));
  svz_coserver_send_request (COSERVER_IDENT, buffer, cb, arg0, arg1);
}

/*
 * This static array contains the coserver structure for each type of
 * internal coserver the core library provides.
 */
svz_coservertype_t svz_coservertypes[] =
{
  /* coserver-TODO: 
     place coserver callbacks and identification here */

  { COSERVER_REVERSE_DNS, "reverse dns", 
    reverse_dns_handle_request, 1, reverse_dns_init, 0 },

  { COSERVER_IDENT, "ident", 
    ident_handle_request, 1, NULL, 0},

  { COSERVER_DNS, "dns", 
    dns_handle_request, 1, NULL, 0 }
};

/*
 * Internal coserver instances.
 */
svz_array_t *svz_coservers = NULL;

/*
 * This routine gets the coserver hash id from a given response and 
 * cuts it from the given response buffer.
 */
static unsigned
svz_coserver_get_id (char *response)
{
  char *p = response;
  unsigned id = 0;

  while (*p >= '0' && *p <= '9')
    {
      id *= 10;
      id += *p - '0';
      p++;
    }
  if (*p != COSERVER_ID_BOUNDARY)
    {
      svz_log (LOG_WARNING,
	       "coserver: invalid protocol character (0x%02x)\n", *p);
      return 0;
    }
  p++;
  
  while (*p != COSERVER_PACKET_BOUNDARY)
    {
      *response++ = *p++;
    }
  *response = '\0';
  return id;
}

/*
 * This function adds a given coserver hash id to the response.
 */
static void
svz_coserver_put_id (unsigned id, char *response)
{
  char buffer[COSERVER_BUFSIZE];

  snprintf (buffer, COSERVER_BUFSIZE, "%u:%s\n", id, response);
  strcpy (response, buffer);
}

/*************************************************************************/
/*            This is part of the coserver process / thread.             */
/*************************************************************************/

/*
 * Unices:
 * svz_coserver_loop() is a infinite loop in a separate process. It reads
 * blocking from a receive pipe, processes the request and puts the
 * result to a sending pipe to the server.
 */

/* Debug info Macro. */
#if SVZ_ENABLE_DEBUG
# define COSERVER_REQUEST_INFO() \
  svz_log (LOG_DEBUG, "%s: coserver request occurred\n",   \
	   svz_coservertypes[coserver->type].name);
#else
# define COSERVER_REQUEST_INFO()
#endif

/* Post-Processing Macro. */
#if SVZ_ENABLE_DEBUG
# define COSERVER_RESULT() \
  svz_log (LOG_DEBUG, "%s: coserver request processed\n", \
	   svz_coservertypes[coserver->type].name);
#else
# define COSERVER_RESULT()
#endif

/* Pre-Processing Macro. */
#define COSERVER_REQUEST()                                   \
  COSERVER_REQUEST_INFO ();                                  \
  /* Process the request here. Might be blocking indeed ! */ \
  if ((id = svz_coserver_get_id (request)) != 0)             \
    {                                                        \
      if ((result = coserver->callback (request)) == NULL)   \
        {                                                    \
          result = request;                                  \
          *result = '\0';                                    \
        }                                                    \
      svz_coserver_put_id (id, result);                      \
    }                                                        \


static void
svz_coserver_loop (svz_coserver_t *coserver, int in_pipe, int out_pipe)
{
  FILE *in, *out;
  char request[COSERVER_BUFSIZE];
  char *result = NULL;
  unsigned id;

  if ((in = fdopen (in_pipe, "r")) == NULL)
    {
      svz_log (LOG_ERROR, "coserver: fdopen (%d): %s\n", in_pipe, SYS_ERROR);
      return;
    }
  if ((out = fdopen (out_pipe, "w")) == NULL)
    {
      svz_log (LOG_ERROR, "coserver: fdopen (%d): %s\n", out_pipe, SYS_ERROR);
      return;
    }

  while (NULL != fgets (request, COSERVER_BUFSIZE, in))
    {
      
      COSERVER_REQUEST ();
	  
      if (id && result)
	{
	  fprintf (out, "%s", result);
	  fflush (out);
	  COSERVER_RESULT ();
	}
    }

  /* error in reading pipe */
  if (fclose (in))
    svz_log (LOG_ERROR, "fclose: %s\n", SYS_ERROR);
  if (fclose (out))
    svz_log (LOG_ERROR, "fclose: %s\n", SYS_ERROR);
}


/*************************************************************************/
/*                   This is part of the server process.                 */
/*************************************************************************/

/*
 * Return the number of currently running coservers with the type @var{type}.
 */
static int
svz_coserver_count (int type)
{
  int n, count = 0;
  svz_coserver_t *coserver;

  svz_array_foreach (svz_coservers, coserver, n)
    if (coserver->type == type)
      count++;
  return count;
}

/*
 * Delete the n'th internal coserver from coserver array.
 */
static void
svz_coserver_delete (int n)
{
  svz_coserver_t *coserver;

  if ((coserver = svz_array_get (svz_coservers, n)) != NULL)
    {
      svz_free (coserver);
      svz_array_del (svz_coservers, n);
    }
  if (svz_array_size (svz_coservers) == 0)
    {
      svz_array_destroy (svz_coservers);
      svz_coservers = NULL;
    }
}

/*
 * Disconnects a internal coserver. This is the callback routine for the
 * socket structure entry `disconnected_socket'.
 */
static int
svz_coserver_disconnect (svz_socket_t *sock)
{
  int n;
  svz_coserver_t *coserver;

  svz_array_foreach (svz_coservers, coserver, n)
    {
      if (coserver->sock == sock)
	{
#if SVZ_ENABLE_DEBUG
	  svz_log (LOG_DEBUG, 
		   "%s: killing coserver pid %d\n",
		   svz_coservertypes[coserver->type].name, coserver->pid);
#endif /* SVZ_ENABLE_DEBUG */
	  if (kill (coserver->pid, SIGKILL) == -1)
	    svz_log (LOG_ERROR, "kill: %s\n", SYS_ERROR);
	  /* cleanup coserver child process */
	  else if (waitpid (coserver->pid, NULL, WNOHANG) == -1)
	    svz_log (LOG_ERROR, "waitpid: %s\n", SYS_ERROR);
	  /* re-arrange the internal coserver array */
	  svz_coserver_delete (n);
	  break;
	}
    }
  return 0;
}

/*
 * This routine has to be called for coservers requests. It is the default 
 * @code{check_request()} routine for coservers detecting full responses as 
 * lines (trailing '\n').
 */
static int
svz_coserver_check_request (svz_socket_t *sock)
{
  char *packet = sock->recv_buffer;
  char *p = packet;
  int request_len;
  int len = 0;
  svz_coserver_t *coserver = sock->data;

  assert (coserver);
  do
    {
      /* find a line (trailing '\n') */
      while (*p != COSERVER_PACKET_BOUNDARY &&
	     p < sock->recv_buffer + sock->recv_buffer_fill)	     
	p++;

      if (*p == COSERVER_PACKET_BOUNDARY && 
	  p < sock->recv_buffer + sock->recv_buffer_fill)
	{
	  coserver->busy--;
	  p++;
	  request_len = p - packet;
	  len += request_len;
	  if (sock->handle_request)
	    sock->handle_request (sock, packet, request_len);
	  packet = p;
	}
    }
  while (p < sock->recv_buffer + sock->recv_buffer_fill);
      
#if SVZ_ENABLE_DEBUG
  svz_log (LOG_DEBUG, "%s: %d byte response\n", 
	   svz_coservertypes[coserver->type].name, len);
#endif

  /* remove data from receive buffer if necessary */
  if (len > 0 && sock->recv_buffer_fill > len)
    {
      memmove (sock->recv_buffer, packet, sock->recv_buffer_fill - len);
    }
  sock->recv_buffer_fill -= len;

  return 0;
}

/*
 * The standard coserver @code{handle_request()} routine is called whenever
 * the standard @code{check_request()} detected a full packet by any coserver.
 */
static int
svz_coserver_handle_request (svz_socket_t *sock __attribute__((unused)),
                             char *request, int len)
{
  int ret;
  unsigned id;
  char *p, *end, *data;
  svz_coserver_callback_t *cb;
  
  /* Search for coserver hash id. */
  id = 0;
  p = request;
  end = p + len;
  while (*p != COSERVER_ID_BOUNDARY && p < end) 
    {
      if (*p < '0' || *p > '9')
	{
	  svz_log (LOG_WARNING, 
		   "coserver: invalid character in id (0x%02X)\n", *p);
	  return -1;
	}
      id *= 10;
      id += *p - '0';
      p++;
    }
  if (p == end)
    {
      svz_log (LOG_WARNING, "coserver: invalid coserver response (no id)\n");
      return -1;
    }
  data = ++p;

  /* Search for packet end. */
  while (*p != COSERVER_PACKET_BOUNDARY && p < end)
    p++;
  if (p == end)
    {
      svz_log (LOG_WARNING, 
	       "coserver: invalid coserver response (no data)\n");
      return -1;
    }
  *p = '\0';

  /* Have a look at the coserver callback hash. */
  if (NULL == (cb = svz_hash_get (svz_coserver_callbacks, svz_itoa (id))))
    {
      svz_log (LOG_ERROR, "coserver: invalid callback for id %u\n", id);
      return -1;
    }

  /* 
   * Run the callback inclusive its arg. Second arg is either NULL for
   * error detection or the actual result string. Afterwards free the 
   * callback structure and delete it from the coserver callback hash.
   */
  ret = cb->handle_result (*data ? data : NULL, cb->arg[0], cb->arg[1]);
  svz_hash_delete (svz_coserver_callbacks, svz_itoa (id));
  svz_free (cb);

  return ret;
}

/*
 * This function closes the pipes (incoming and outgoing) of all coservers
 * inherited to a newly instantiated coserver. These pipe descriptors are 
 * part of server process and are inherited when we call @code{fork()} in 
 * order to create another coserver sub process. Since this coserver process
 * should not access these pipes we are closing them.
 */
static void
svz_coserver_close_pipes (svz_coserver_t *self)
{
  int n;
  svz_coserver_t *coserver;

  /* go through all coservers except itself */
  svz_array_foreach (svz_coservers, coserver, n)
    {
      if (coserver != self)
	{
	  close (coserver->sock->pipe_desc[READ]);
	  close (coserver->sock->pipe_desc[WRITE]);
	}
    }
}

/* 
 * Iterate each socket object and close its file/socket/pipe 
 * descriptors. Also frees the (cloned) queues.
 * Note: Duplicate memory for everything else, including server private data.
 *       We cannot take care of all that because the servers do not know
 *       that they may get cloned. We therefore waste memory in the coservers.
 */
static void
svz_coserver_closeall (svz_socket_t *self)
{
  svz_socket_t *sock, *next;

  for (sock = svz_sock_root; sock != NULL; sock = next)
    {
      if (sock->flags & SOCK_FLAG_SOCK)
	if (sock->sock_desc >= 2)
	  close (sock->sock_desc);
      if (sock->flags & SOCK_FLAG_FILE)
	if (sock->file_desc >= 2)
	  close (sock->file_desc);
      if (sock->flags & SOCK_FLAG_PIPE)
	{
	  if (sock->pipe_desc[READ] >= 2)
	    close (sock->pipe_desc[READ]);
	  if (sock->pipe_desc[WRITE] >= 2)
	    close (sock->pipe_desc[WRITE]);
	}
      next = sock->next;
      if (sock != self)
	{
	  svz_sock_resize_buffers (sock, 0, 0);
	  svz_free (sock);
	}
    }
  svz_file_closeall ();
}

/*
 * Setup signaling for a coserver process. This is necessary since 
 * the original signal handlers get confused about signals raised by its
 * children.
 */
static void
svz_coserver_signals (void)
{
  signal (SIGTERM, SIG_IGN);
  signal (SIGINT, SIG_IGN);
  signal (SIGHUP, SIG_IGN);
  signal (SIGPIPE, SIG_IGN);
  signal (SIGQUIT, SIG_IGN);
}

/*
 * Destroy specific coservers with the type @var{type}. This works for 
 * Win32 and Unices. All instances of this coserver type will be stopped.
 */
void
svz_coserver_destroy (int type)
{
  int n, count = 0;
  svz_coserver_t *coserver;
  
  svz_array_foreach (svz_coservers, coserver, n)
    {
      if (coserver->type == type)
	{
	  if (kill (coserver->pid, SIGKILL) == -1)
	    svz_log (LOG_ERROR, "kill: %s\n", SYS_ERROR);
	  /* cleanup coserver child process */
	  else if (waitpid (coserver->pid, NULL, WNOHANG) == -1)
	    svz_log (LOG_ERROR, "waitpid: %s\n", SYS_ERROR);
	  svz_coserver_delete (n);
	  n--;
	  count++;
	}
    }

#ifdef SVZ_ENABLE_DEBUG
  if (count > 0)
    {
      svz_log (LOG_DEBUG, "%d internal %s coserver destroyed\n", 
	       count, svz_coservertypes[type].name);
    }
#endif /* SVZ_ENABLE_DEBUG */
}

/*
 * Start a specific internal coserver. This works for Win32 and
 * Unices. Whereas in Unix a process is @code{fork()}ed and in Win32
 * a thread gets started.
 */
static svz_socket_t *
svz_coserver_start (int type) 
{
  svz_socket_t *sock;
  svz_coserver_t *coserver;
  
  int s2c[2];
  int c2s[2];
  int pid;

  svz_log (LOG_NOTICE, "starting internal %s coserver\n", 
	   svz_coservertypes[type].name);

  coserver = svz_malloc (sizeof (svz_coserver_t));
  coserver->type = type;
  coserver->busy = 0;
  coserver->sock = NULL;

  if (svz_coservers == NULL)
    svz_coservers = svz_array_create (MAX_COSERVER_TYPES, NULL);
  svz_array_add (svz_coservers, coserver);

  /* fill in the actual coserver callback */
  coserver->callback = svz_coservertypes[type].callback;

  /* create pipes for process communication */
  if (pipe (s2c) < 0)
    {
      svz_log (LOG_ERROR, "pipe server-coserver: %s\n", SYS_ERROR);
      svz_coserver_delete (svz_array_size (svz_coservers) - 1);
      return NULL;
    }
  if (pipe (c2s) < 0)
    {
      close (s2c[READ]);
      close (s2c[WRITE]);
      svz_log (LOG_ERROR, "pipe coserver-server: %s\n", SYS_ERROR);
      svz_coserver_delete (svz_array_size (svz_coservers) - 1);
      return NULL;
    }

  /* fork() us here */
  if ((pid = fork ()) == 0)
    {
      int in = s2c[READ], out = c2s[WRITE];

      /* close the servers pipe descriptors */
      if (close (s2c[WRITE]) < 0)
	svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);
      if (close (c2s[READ]) < 0)
	svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);

#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "coserver pipes: %d-%d\n", in, out);
#endif

      /* check if the pipes are 0, 1 or 2 already */
      if (in > 2 && out > 2)
	{
	  /* reassign the pipes to stdout and stdin */
	  if (dup2 (in, 0) != 0)
	    svz_log (LOG_ERROR, "dup2: %s\n", SYS_ERROR);
	  if (dup2 (out, 1) != 1)
	    svz_log (LOG_ERROR, "dup2: %s\n", SYS_ERROR);
	  /* close the old pipe descriptors */
	  close (in);
	  close (out);
	  close (2);
	  in = 0;
	  out = 1;
	}
      else
	{
	  if (in != 2 && out != 2)
	    close (2);
	  if (in != 1 && out != 1)
	    close (1);
	  if (in != 0 && out != 0)
	    close (0);
	}

      /* close all other coserver pipes except its own */
      svz_coserver_close_pipes (coserver);
      svz_coserver_closeall (coserver->sock);
      svz_coserver_signals ();

      /* start the internal coserver */
      svz_coserver_loop (coserver, in, out);
      exit (0);
    }
  else if (pid == -1)
    {
      svz_log (LOG_ERROR, "fork: %s\n", SYS_ERROR);
      close (s2c[READ]);
      close (s2c[WRITE]);
      close (c2s[READ]);
      close (c2s[WRITE]);
      svz_coserver_delete (svz_array_size (svz_coservers) - 1);
      return NULL;
    }

  /* the old server process continues here */
  
#ifdef SVZ_ENABLE_DEBUG
  svz_log (LOG_DEBUG, "coserver process id is %d\n", pid);
#endif

  /* close the coservers pipe descriptors */
  if (close (s2c[READ]) < 0)
    svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);
  if (close (c2s[WRITE]) < 0)
    svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);

  if ((sock = svz_pipe_create (c2s[READ], s2c[WRITE])) == NULL)
    {
      if (close (c2s[READ]) < 0)
	svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);
      if (close (s2c[WRITE]) < 0)
	svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);
      svz_coserver_delete (svz_array_size (svz_coservers) - 1);
      return NULL;
    }

  coserver->pid = pid;
  coserver->sock = sock;
  sock->disconnected_socket = svz_coserver_disconnect;
  sock->write_socket = svz_pipe_write_socket;
  sock->read_socket = svz_pipe_read_socket;
  svz_sock_enqueue (sock);

  svz_coservertypes[coserver->type].last_start = (long) time (NULL);
  sock->data = coserver;
  sock->check_request = svz_coserver_check_request;
  sock->handle_request = svz_coserver_handle_request;
  sock->flags |= (SOCK_FLAG_NOFLOOD | SOCK_FLAG_COSERVER);
  return sock;
}

/*
 * Call this routine whenever there is time, e.g. within the timeout of 
 * the @code{select()} statement. Indeed I built it in the 
 * @code{svz_periodic_tasks()} statement. Under Wind32 the routine checks 
 * if there was any response from an active coserver. Moreover it keeps
 * the coserver threads/processes alive. If one of the coservers dies due
 * to buffer overrun or might be overloaded this function starts a new one.
 */
void
svz_coserver_check (void)
{
  svz_coserver_t *coserver;
  svz_coservertype_t *ctype;
  svz_socket_t *sock;
  int n;
  
  /* check the number of coserver instances of each coserver type */
  for (n = 0; n < MAX_COSERVER_TYPES; n++)
    {
      ctype = &svz_coservertypes[n];
      if (svz_coserver_count (ctype->type) < ctype->instances &&
	  ((long) time (NULL)) - ctype->last_start >= 3)
	svz_coserver_start (ctype->type);
    }

  /* restart coserver instances if buffer overrun is in sight (send buffer
     fill >= 75 percent) */
  svz_array_foreach (svz_coservers, coserver, n)
    {
      ctype = &svz_coservertypes[coserver->type];
      sock = coserver->sock;
      if (sock->send_buffer_fill * 100 / sock->send_buffer_size >= 75 &&
	  ((long) time (NULL)) - ctype->last_start >= 3 &&
	  svz_coserver_count (ctype->type) <= ctype->instances)
	svz_coserver_start (coserver->type);
    }
}

/*
 * Create a single coserver with the given type @var{type}.
 */
void 
svz_coserver_create (int type)
{
  if (svz_coservertypes[type].init)
    svz_coservertypes[type].init ();
  svz_coserver_start (type);
}

/*
 * Global coserver initialization. Here you should start all the internal
 * coservers you want to use later.
 */
int
svz_coserver_init (void)
{
  int i, n;
  svz_coservertype_t *coserver;

  svz_coserver_callbacks = svz_hash_create (4, svz_free);
  svz_coserver_callback_id = 1;

  for (n = 0; n < MAX_COSERVER_TYPES; n++)
    {
      coserver = &svz_coservertypes[n];
      if (coserver->init)
	coserver->init ();
      for (i = 0; i < coserver->instances; i++)
	svz_coserver_start (coserver->type);
    }

  return 0;
}

/*
 * Global coserver finalization.
 */
int
svz_coserver_finalize (void)
{
  int n;
  svz_coservertype_t *coserver;

  for (n = 0; n < MAX_COSERVER_TYPES; n++)
    {
      coserver = &svz_coservertypes[n];
      svz_coserver_destroy (coserver->type);
    }

#if SVZ_ENABLE_DEBUG
  svz_log (LOG_DEBUG, "coserver: %d callback(s) left\n",
	   svz_hash_size (svz_coserver_callbacks));
#endif

  /* Destroy all callbacks left so far. */
  svz_hash_destroy (svz_coserver_callbacks);
  return 0;
}

/*
 * Invoke a @var{request} for one of the running internal coservers
 * with type @var{type}. @var{handle_result} and @var{arg} specify what 
 * should happen if the coserver delivers a result.
 */
void
svz_coserver_send_request (int type, char *request, 
			   svz_coserver_handle_result_t handle_result, 
			   svz_coserver_args_t)
{
  int n, busy;
  svz_coserver_t *coserver, *current;
  svz_coserver_callback_t *cb;
  
  /* 
   * Go through all coservers and find out which coserver 
   * type TYPE is the least busiest.
   */
  coserver = NULL;
  svz_array_foreach (svz_coservers, current, n)
    {
      if (current->type == type)
	{
	  if (coserver == NULL)
	    { 
	      coserver = current;
	      busy = coserver->busy;
	    }
	  else if (current->busy <= coserver->busy)
	    {
	      coserver = current;
	      busy = coserver->busy;
	    }
	}
    }

  /* found an appropriate coserver */
  if (coserver)
    {
      /*
       * Create new callback hash entry for this coserver request and
       * put it into the global coserver callback hash.
       */
      cb = svz_malloc (sizeof (svz_coserver_callback_t));
      cb->handle_result = handle_result;
      cb->arg[0] = arg0;
      cb->arg[1] = arg1;
      svz_hash_put (svz_coserver_callbacks, 
		    svz_itoa (svz_coserver_callback_id), cb);

      coserver->busy++;
      if (svz_sock_printf (coserver->sock, "%u:%s\n", 
			   svz_coserver_callback_id, request))
	{
	  svz_sock_schedule_for_shutdown (coserver->sock);
	}
      svz_coserver_callback_id++;
    }
}
