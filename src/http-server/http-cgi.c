/*
 * http-cgi.c - http cgi implementation
 *
 * Copyright (C) 2000, 2001, 2003, 2004, 2007 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: http-cgi.c,v 1.54 2007/03/29 18:12:31 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_HTTP_PROTO

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
#if HAVE_FLOSS_H
# include <floss.h>
#endif
#include <signal.h>

#if HAVE_STRINGS_H
# include <strings.h>
#endif

#ifdef __MINGW32__
# include <winsock2.h>
# include <io.h>
# include <shellapi.h>
#endif

#ifndef __MINGW32__
# include <netinet/in.h>
# if HAVE_WAIT_H
#  include <wait.h>
# endif
# if HAVE_SYS_WAIT_H
#  include <sys/wait.h>
# endif
#endif

#include "libserveez.h"
#include "http-proto.h"
#include "http-core.h"
#include "http-cgi.h"

/*
 * Extended disconnect_socket callback for CGIs. Handling CGI related
 * topics and afterwards we process the normal http disconnection
 * functionality.
 */
int
http_cgi_disconnect (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;

  /* flush CGI output if necessary */
  if (sock->flags & SOCK_FLAG_PIPE && sock->send_buffer_fill > 0)
    if (sock->write_socket)
      sock->write_socket (sock);

  /* close both of the CGI pipes if necessary */
  if (sock->pipe_desc[READ] != INVALID_HANDLE)
    {
      if (closehandle (sock->pipe_desc[READ]) == -1)
	svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);
      sock->pipe_desc[READ] = INVALID_HANDLE;
      sock->flags &= ~SOCK_FLAG_RECV_PIPE;
    }
  if (sock->pipe_desc[WRITE] != INVALID_HANDLE)
    {
      if (closehandle (sock->pipe_desc[WRITE]) == -1)
	svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);
      sock->pipe_desc[WRITE] = INVALID_HANDLE;
      sock->flags &= ~SOCK_FLAG_SEND_PIPE;
    }

#ifdef __MINGW32__
  /* 
   * Close the process handle if necessary, but only in the Windows-Port ! 
   */
  if (http->pid != INVALID_HANDLE)
    {
      if (!TerminateProcess (http->pid, 0))
	svz_log (LOG_ERROR, "TerminateProcess: %s\n", SYS_ERROR);
      if (closehandle (http->pid) == -1)
	svz_log (LOG_ERROR, "CloseHandle: %s\n", SYS_ERROR);
      http->pid = INVALID_HANDLE;
    }
#else /* not __MINGW32__ */
  /*
   * Try killing the cgi script.
   */
  if (http->pid != INVALID_HANDLE)
    {
      if (kill (http->pid, SIGKILL) == -1)
	svz_log (LOG_ERROR, "kill: %s\n", SYS_ERROR);
#if HAVE_WAITPID
      /* Test if the cgi is still running and cleanup. */
      else if (waitpid (http->pid, NULL, 0) == -1)
	svz_log (LOG_ERROR, "waitpid: %s\n", SYS_ERROR);
#endif /* not HAVE_WAITPID */
      http->pid = INVALID_HANDLE;
    }
#endif /* not __MINGW32__ */

  return http_disconnect (sock);
}

/*
 * This is the default idle function for http connections. It checks 
 * whether any died child was a cgi script.
 */
int
http_cgi_died (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;
#ifdef __MINGW32__
  DWORD result;
#endif

  if (sock->flags & SOCK_FLAG_PIPE)
    {
#ifndef __MINGW32__
      /* Check if a died child is this cgi. */
      if (svz_child_died && http->pid == svz_child_died)
	{
	  svz_log (LOG_NOTICE, "cgi script pid %d died\n", 
		   (int) svz_child_died);
	  svz_child_died = 0;
	}
#if HAVE_WAITPID
      /* Test if the cgi is still running. */
      if (waitpid (http->pid, NULL, WNOHANG) == http->pid)
	{
	  svz_log (LOG_NOTICE, "cgi script pid %d died\n", (int) http->pid);
	  http->pid = INVALID_HANDLE;
	}
#endif /* HAVE_WAITPID */

#else /* __MINGW32__ */

      /*
       * Check if there died a process handle in Win32, this has to be
       * done regularly here because there is no SIGCHLD in Win32 !
       */
      if (http->pid != INVALID_HANDLE)
	{
	  result = WaitForSingleObject (http->pid, LEAST_WAIT_OBJECT);
	  if (result == WAIT_FAILED)
	    {
	      svz_log (LOG_ERROR, "WaitForSingleObject: %s\n", SYS_ERROR);
	    }
	  else if (result != WAIT_TIMEOUT)
	    {
	      if (closehandle (http->pid) == -1)
		svz_log (LOG_ERROR, "CloseHandle: %s\n", SYS_ERROR);
	      svz_child_died = http->pid;
	      http->pid = INVALID_HANDLE;
	    }
	}
#endif /* __MINGW32__ */
    }
  
  sock->idle_counter = 1;
  return 0;
}

/*
 * The http cgi reader gets data from the stdout of a cgi
 * program and stores the data into the send buffer of
 * the socket structure. We set the HTTP_FLAG_DONE flag
 * to indicate there was no more data.
 */
int
http_cgi_read (svz_socket_t *sock)
{
  int do_read;
  int num_read;
  http_socket_t *http = sock->data;

  /* read as much space is left in the buffer */
  do_read = sock->send_buffer_size - sock->send_buffer_fill;
  if (do_read <= 0) 
    {
      return 0;
    }

#ifdef __MINGW32__
  /* check how many bytes could be read from the cgi pipe */
  if (!PeekNamedPipe (sock->pipe_desc[READ], NULL, 0, 
		      NULL, (DWORD *) &num_read, NULL))
    {
      svz_log (LOG_ERROR, "cgi: PeekNamedPipe: %s\n", SYS_ERROR);
      return -1;
    }

  /* adjust number of bytes to read */
  if (do_read > num_read)
    do_read = num_read;

  /* really read from pipe */
  if (!ReadFile (sock->pipe_desc[READ],
		 sock->send_buffer + sock->send_buffer_fill,
		 do_read, (DWORD *) &num_read, NULL))
    {
      svz_log (LOG_ERROR, "cgi: ReadFile: %s\n", SYS_ERROR);
      num_read = -1;
    }
#else /* not __MINGW32__ */
  if ((num_read = read (sock->pipe_desc[READ],
			sock->send_buffer + sock->send_buffer_fill,
			do_read)) == -1)
    {
      svz_log (LOG_ERROR, "cgi: read: %s\n", SYS_ERROR);
      if (svz_errno == EAGAIN)
	return 0;
      num_read = -1;
    }
#endif /* not __MINGW32__ */

  /* data has been read */
  else if (num_read > 0)
    {
      http->length += num_read;
      sock->send_buffer_fill += num_read;
      return 0;
    }

#ifdef __MINGW32__
  /*
   * because pipes cannot be select()ed it can happen that there is no
   * data within the receiving pipe, but the cgi has not yet terminated
   */
  if (num_read == 0 && http->pid != INVALID_HANDLE)
    {
      return 0;
    }
#endif /* __MINGW32__ */

  /* no data has been received */
  sock->userflags |= HTTP_FLAG_DONE;
  if (sock->send_buffer_fill == 0)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "cgi: data successfully received and resent\n");
#endif
      sock->userflags &= ~HTTP_FLAG_CGI;
      sock->flags &= ~SOCK_FLAG_RECV_PIPE;
      return -1;
    }
  
  return 0;
}

/*
 * HTTP_CGI_WRITE pipes all read data from the http socket connection 
 * into the cgi stdin. This is necessary for the so called post method.
 * It directly reads from the RECV_BUFFER of the socket structure.
 */
int
http_cgi_write (svz_socket_t *sock)
{
  int do_write;
  int num_written;
  http_socket_t *http = sock->data;

  /* 
   * Write as many bytes as possible, remember how many
   * were actually sent. Do not write more than the content
   * length of the post data.
   */
  do_write = sock->recv_buffer_fill;
  if (do_write > http->contentlength)
    do_write = http->contentlength;

#ifdef __MINGW32__
  if (!WriteFile (sock->pipe_desc[WRITE], sock->recv_buffer, 
		  do_write, (DWORD *) &num_written, NULL))
    {
      svz_log (LOG_ERROR, "cgi: WriteFile: %s\n", SYS_ERROR);
      num_written = -1;
    }
#else /* !__MINGW32__ */
  if ((num_written = write (sock->pipe_desc[WRITE], 
			    sock->recv_buffer, do_write)) == -1)
    {
      svz_log (LOG_ERROR, "cgi: write: %s\n", SYS_ERROR);
    }
#endif /* !__MINGW32__ */

  /* data has been successfully sent */
  if (num_written > 0)
    {
      sock->last_send = time (NULL);

      /*
       * Shuffle the data in the output buffer around, so that
       * new data can get stuffed into it.
       */
      if (sock->recv_buffer_fill > num_written)
	{
	  memmove (sock->recv_buffer, 
		   sock->recv_buffer + num_written,
		   sock->recv_buffer_fill - num_written);
	}
      sock->recv_buffer_fill -= num_written;
      http->contentlength -= num_written;
    }

  /* 
   * If we have written all data to the CGI stdin, we can now start
   * reading from the CGI's stdout and write again to the http
   * connection.
   */
  if (http->contentlength <= 0)
    {
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "cgi: post data sent to cgi\n");
#endif
      sock->userflags &= ~HTTP_FLAG_POST;
      sock->flags &= ~SOCK_FLAG_SEND_PIPE;
      sock->userflags |= HTTP_FLAG_CGI;
      sock->flags |= SOCK_FLAG_RECV_PIPE;
      sock->read_socket = http_cgi_read;
      sock->write_socket = http_default_write;
    }

  /*
   * Return a non-zero value if an error occurred.
   */
  return (num_written < 0) ? -1 : 0;
}

/*
 * Create the environment block for a CGI script. Depending on the
 * system the environment is a field of null terminated char pointers
 * (for Unices) followed by a null pointer or one char pointer where
 * the variables a separated by zeros and the block is terminated
 * by a further zero. It returns the amount of defined variables.
 */
static int
http_create_cgi_envp (svz_socket_t *sock,  /* socket for this request */
		      svz_envblock_t *env, /* env block */
		      char *script,        /* the cgi script's filename */
		      int type)            /* the cgi type */
{
  http_socket_t *http;
  http_config_t *cfg = sock->cfg;

  /* request type identifiers */
  static char request_type[2][5] = { "POST", "GET" };

  /* 
   * This data field is needed for the conversion of http request 
   * properties into environment variables.
   */
  static struct 
  { 
    char *property; /* property identifier */
    char *env;      /* variable identifier */
  }
  env_var[] =
  {
    { "Content-length",  "CONTENT_LENGTH"       },
    { "Content-type",    "CONTENT_TYPE"         },
    { "Accept",          "HTTP_ACCEPT"          },
    { "Referer",         "HTTP_REFERER"         },
    { "User-Agent",      "HTTP_USER_AGENT"      },
    { "Host",            "HTTP_HOST"            },
    { "Connection",      "HTTP_CONNECTION"      },
    { "Accept-Encoding", "HTTP_ACCEPT_ENCODING" },
    { "Accept-Language", "HTTP_ACCEPT_LANGUAGE" },
    { "Accept-Charset",  "HTTP_ACCEPT_CHARSET"  },
    { NULL, NULL }
  };

  unsigned n; 
  int c;

  /* setup default environment */
  svz_envblock_default (env);

  /* get http socket structure */
  http = sock->data;

  /* convert some http request properties into environment variables */
  if (http->property)
    for (c = 0; env_var[c].property; c++)
      for (n = 0; http->property[n]; n += 2)
	if (!svz_strcasecmp (http->property[n], env_var[c].property))
	  {
	    svz_envblock_add (env, "%s=%s", 
			      env_var[c].env, http->property[n + 1]);
	    break;
	  }

  /* 
   * set up some more environment variables which might be 
   * necessary for the cgi script
   */
  svz_envblock_add (env, "SERVER_NAME=%s", 
		    cfg->host ? cfg->host : svz_inet_ntoa (sock->local_addr));
  svz_envblock_add (env, "SERVER_PORT=%u", ntohs (sock->local_port));
  svz_envblock_add (env, "REMOTE_ADDR=%s", http->host ? http->host :
		    svz_inet_ntoa (sock->remote_addr));
  svz_envblock_add (env, "REMOTE_PORT=%u", ntohs (sock->remote_port));
  svz_envblock_add (env, "SCRIPT_NAME=%s%s", cfg->cgiurl, script);
  svz_envblock_add (env, "GATEWAY_INTERFACE=%s", CGI_VERSION);
  svz_envblock_add (env, "SERVER_PROTOCOL=%s", HTTP_VERSION);
  svz_envblock_add (env, "SERVER_SOFTWARE=%s/%s", 
		    svz_library, svz_version);
  svz_envblock_add (env, "REQUEST_METHOD=%s", request_type[type]);

  return env->size;
}

/*
 * Check the http option (the URL) for a cgi request. This routine
 * parses the text of the request and delivers the real file to be
 * invoked. This function makes sure that the cgi script file exists
 * and is executable. On success it delivers a pointer which must be
 * svz_free()ed after use.
 */
char *
http_check_cgi (svz_socket_t *sock, char *request)
{
#ifndef __MINGW32__
  struct stat buf;
#endif
  char *file;
  int fd;
  int size;
  char *p;
  char *saverequest;
  int len;
  http_config_t *cfg = sock->cfg;

  /* check if the request is a real cgi request */
  if (strstr (request, cfg->cgiurl) != request)
    {
      return HTTP_NO_CGI;
    }

  /* 
   * skip the CGI url and concate the script file itself, then
   * check for trailing '?' which is the starting character for
   * GET variables.
   */

  /* store the request in a local variable */
  len = strlen (request) + 1 - strlen (cfg->cgiurl);
  saverequest = svz_malloc (len);
  strcpy (saverequest, request + strlen (cfg->cgiurl));

  /* find the actual URL */
  p = saverequest;
  while (*p != '?' && *p != 0)
    p++;
  *p = 0;

  size = strlen (cfg->cgidir) + len;
  file = svz_malloc (size);
  sprintf (file, "%s%s", cfg->cgidir, saverequest);

  /* test if the file really exists and close it again */
  if ((fd = open (file, O_RDONLY)) == -1)
    {
      svz_log (LOG_ERROR, "cgi: open: %s (%s)\n", SYS_ERROR, file);
      svz_free (file);
      svz_free (saverequest);
      return NULL;
    }

#ifndef __MINGW32__
  /* test the file being an executable */
  if (fstat (fd, &buf) == -1)
    {
      svz_log (LOG_ERROR, "cgi: fstat: %s\n", SYS_ERROR);
      close (fd);
      svz_free (file);
      svz_free (saverequest);
      return NULL;
    }

  if (!(buf.st_mode & S_IFREG) ||
      !(buf.st_mode & S_IXUSR) || !(buf.st_mode & S_IRUSR))  
    {
      svz_log (LOG_ERROR, "cgi: no executable: %s\n", file);
      close (fd);
      svz_free (file);
      svz_free (saverequest);
      return NULL;
    }
#endif
  if (close (fd) == -1)
    svz_log (LOG_ERROR, "cgi: close: %s\n", SYS_ERROR);

  /* return a pointer referring to the actual plain cgi file */
  strcpy (file, saverequest);
  file = svz_realloc (file, strlen (file) + 1);
  svz_free (saverequest);
  return file;
}

/*
 * Prepare the invocation of a cgi script which means to change to 
 * the referred directory and the creation of a valid environment
 * block. Return a NULL pointer on errors or a pointer to the full
 * cgi file (including the path). This MUST be freed afterwards.
 */
char *
http_pre_exec (svz_socket_t *sock,   /* socket structure */
	       svz_envblock_t *envp, /* environment block to be filled */
	       char *file,           /* plain executable name */
	       char *request,        /* original http request */
	       int type)             /* POST or GET ? */
{
  char *cgidir;
  char *cgifile;
  char *p;
  http_config_t *cfg = sock->cfg;

  /* change into the CGI directory temporarily */
  if (chdir (cfg->cgidir) == -1)
    {
      svz_log (LOG_ERROR, "cgi: chdir: %s\n", SYS_ERROR);
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "cgi: cannot change dir: %s\n", cfg->cgidir);
#endif
      return NULL;
    }

  /* get the current directory  */
  cgidir = svz_getcwd ();
  
  /* put the directory and file together */
  cgifile = svz_malloc (strlen (cgidir) + strlen (file) + 1);
  sprintf (cgifile, "%s%s", cgidir, file);
  svz_free (cgidir);

  /* create the environment block for the CGI script */
  http_create_cgi_envp (sock, envp, file, type);

  /* put the QUERY_STRING into the env variables if necessary */
  if (type == GET_METHOD)
    {
      p = request;
      while (*p != '?' && *p != 0)
	p++;
      svz_envblock_add (envp, "QUERY_STRING=%s", *p ? p + 1 : "");
    }

  return cgifile;
}

/*
 * Write an initial HTTP response header to the socket SOCK
 * right after the the actual CGI script has been invoked.
 */
int
http_cgi_accepted (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;

  http->response = 202;
  return svz_sock_printf (sock, HTTP_OK
			  "Date: %s\r\n"
			  "Server: %s/%s\r\n"
			  "Connection: close\r\n",
			  http_asc_date (time (NULL)),
			  svz_library, svz_version);
}

/*
 * This routine generates some standard cgi associations.
 */
#define DEFAULT_CGIAPP "default"
void
http_gen_cgi_apps (http_config_t *cfg)
{
  char *p;

  /* create the cgi association hash table if necessary */
  if (cfg->cgiapps == NULL)
    cfg->cgiapps = svz_hash_create (4, svz_free);

  /* the associations need to be in the hash to be executed at all */
  if ((p = svz_hash_put (cfg->cgiapps, "exe", svz_strdup (DEFAULT_CGIAPP))) 
      != NULL)
    svz_free (p);
  if ((p = svz_hash_put (cfg->cgiapps, "com", svz_strdup (DEFAULT_CGIAPP)))
      != NULL)
    svz_free (p);
  if ((p = svz_hash_put (cfg->cgiapps, "bat", svz_strdup (DEFAULT_CGIAPP)))
      != NULL)
    svz_free (p);
}

/*
 * Invoke a cgi script. In Unices we fork() us and in Win32 we
 * CreateProcess().
 */
int
http_cgi_exec (svz_socket_t *sock, /* the socket structure */
	       svz_t_handle in,    /* here the cgi reads from or NULL if GET */
	       svz_t_handle out,   /* here the cgi writes to */
	       char *file,     /* cgi script file */
	       char *request,  /* original request (needed for GET) */
	       int type)       /* request type (POST or GET) */
{
  svz_t_handle pid; /* the pid from fork() or the process handle in Win32 */
  char *cgifile;    /* path including the name of the cgi script */
  http_socket_t *http;
  svz_envblock_t *envp;

#ifdef __MINGW32__
  http_config_t *cfg = sock->cfg;
  STARTUPINFO StartupInfo;         /* store here the inherited handles */
  PROCESS_INFORMATION ProcessInfo; /* where we get the process handle from */
  char *savedir;                   /* save the original directory */
  char *suffix, *p;
  char *cgiapp;
#else
  char *argv[2];
  struct stat buf;
  int retries;
  int oflags;
#endif

  /* Assign local CGI disconnection routine. */
  sock->disconnected_socket = http_cgi_disconnect;

#ifdef __MINGW32__
  /* 
   * Clean the StartupInfo, use the stdio handles, and store the
   * pipe handles there if necessary (depends on type).
   */
  memset (&StartupInfo, 0, sizeof (StartupInfo));
  StartupInfo.cb = sizeof (StartupInfo);
  StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  StartupInfo.hStdOutput = out;
  /* StartupInfo.hStdError = out; */
  if (type == POST_METHOD)
    StartupInfo.hStdInput = in;

  /* reserve buffer space for the environment block */
  envp = svz_envblock_create ();

  /* save the current directory */
  savedir = svz_getcwd ();

  if ((cgifile = http_pre_exec (sock, envp, file, request, type)) == NULL)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      chdir (savedir);
      svz_envblock_destroy (envp);
      svz_free (savedir);
      return -1;
    }

  /* find a cgi interpreter if possible */
  p = cgifile + strlen (cgifile) - 1;
  while (p != cgifile && *p != '.')
    p--;
  suffix = p + 1;

  if ((p = svz_hash_get (cfg->cgiapps, svz_tolower (suffix))) != NULL)
    {
      if (strcmp (p, DEFAULT_CGIAPP))
	{
	  cgiapp = svz_malloc (strlen (cgifile) + strlen (p) + 2);
	  sprintf (cgiapp, "%s %s", p, cgifile);
	  svz_free (cgifile);
	  cgifile = cgiapp;
	}
    }
  /* not a valid file extension */
  else
    {
      /* find an appropriate system association */
      cgiapp = svz_malloc (MAX_PATH);
      if (FindExecutable (cgifile, NULL, cgiapp) <= (HINSTANCE) 32)
	svz_log (LOG_ERROR, "FindExecutable: %s\n", SYS_ERROR);
#if SVZ_ENABLE_DEBUG
      /* if this is enabled you could learn about the system */
      else
	svz_log (LOG_DEBUG, "FindExecutable: %s\n", cgiapp);
#endif
      svz_free (cgiapp);

      /* print some error message */
      svz_sock_printf (sock, HTTP_ACCESS_DENIED "\r\n");
      http_error_response (sock, 403);
      sock->userflags |= HTTP_FLAG_DONE;
      chdir (savedir);
      svz_free (cgifile);
      svz_envblock_destroy (envp);
      svz_free (savedir);
      return -1;
    }

  /* send http header response */
  if (http_cgi_accepted (sock) == -1)
    {
      sock->userflags |= HTTP_FLAG_DONE;
      chdir (savedir);
      svz_free (cgifile);
      svz_envblock_destroy (envp);
      svz_free (savedir);
      return -1;
    }

  /* create the process here */
  if (!CreateProcess (NULL,                    /* ApplicationName */
		      cgifile,                 /* CommandLine */
		      NULL,                    /* ProcessAttributes */
		      NULL,                    /* ThreadAttributes */
		      TRUE,                    /* InheritHandles */
		      DETACHED_PROCESS,        /* CreationFlags */
		      svz_envblock_get (envp), /* Environment */
		      NULL,                    /* CurrentDirectory */
		      &StartupInfo, &ProcessInfo))
    {
      svz_log (LOG_ERROR, "cgi: CreateProcess: %s\n", SYS_ERROR);
#if SVZ_ENABLE_DEBUG
      svz_log (LOG_DEBUG, "cgi: cannot execute: %s\n", cgifile);
#endif
      svz_sock_printf (sock, "\r\n");
      sock->userflags |= HTTP_FLAG_DONE;
      chdir (savedir);
      svz_free (cgifile);
      svz_envblock_destroy (envp);
      svz_free (savedir);
      return -1;
    }
  
  /* reenter the actual directory and free reserved space */
  chdir (savedir);
  svz_free (cgifile);
  svz_envblock_destroy (envp);
  svz_free (savedir);
  pid = ProcessInfo.hProcess;

#ifdef SVZ_ENABLE_DEBUG
  svz_log (LOG_DEBUG, "http: cgi %s got pid 0x%08X\n", 
	   file + 1, ProcessInfo.dwProcessId);
#endif

#else /* not __MINGW32__ */

  retries = 3;
 retry:

  /* fork us here */
  if ((pid = fork ()) == 0)
    {
      /* ------ child process here ------ */

      /* create environment block */
      envp = svz_envblock_create ();
      if ((cgifile = http_pre_exec (sock, envp, file, request, type)) == NULL)
	{
	  exit (0);
	}

      /* make the output blocking */
      if ((oflags = fcntl (out, F_GETFL)) == -1)
	{
	  svz_log (LOG_ERROR, "cgi: fcntl: %s\n", SYS_ERROR);
	  exit (0);
	}
      if (fcntl (out, F_SETFL, oflags & ~O_NONBLOCK) == -1)
	{
	  svz_log (LOG_ERROR, "cgi: fcntl: %s\n", SYS_ERROR);
	  exit (0);
	}

      /* duplicate the receiving pipe descriptor to stdout */
      if (dup2 (out, 1) != 1)
	{
	  svz_log (LOG_ERROR, "cgi: dup2: %s\n", SYS_ERROR);
	  exit (0);
	}
#ifndef SVZ_ENABLE_DEBUG
      /* duplicate stderr to the cgi output */
      if (dup2 (out, 2) != 2)
	{
	  svz_log (LOG_ERROR, "cgi: dup2: %s\n", SYS_ERROR);
	  exit (0);
	}
#endif /* !SVZ_ENABLE_DEBUG */

      /* handle post method */
      if (type == POST_METHOD)
	{
	  /* make the input blocking */
	  if ((oflags = fcntl (in, F_GETFL)) == -1)
	    {
	      svz_log (LOG_ERROR, "cgi: fcntl: %s\n", SYS_ERROR);
	      exit (0);
	    }
	  if (fcntl (in, F_SETFL, oflags & ~O_NONBLOCK) == -1)
	    {
	      svz_log (LOG_ERROR, "cgi: fcntl: %s\n", SYS_ERROR);
	      exit (0);
	    }

	  /* duplicate the sending pipe descriptor to stdin */
	  if (dup2 (in, 0) != 0)
	    {
	      svz_log (LOG_ERROR, "cgi: dup2: %s\n", SYS_ERROR);
	      exit (0);
	    }

	  /* close the old file descriptors */
	  if (close (in) < 0)
	    svz_log (LOG_ERROR, "cgi: close: %s\n", SYS_ERROR);
	}
      /* close remaining stdin in get method */
      else
	{
	  close (0);
	}

      /* close the old file descriptors */
      if (close (out) < 0)
	svz_log (LOG_ERROR, "cgi: close: %s\n", SYS_ERROR);

      /* get the cgi scripts permissions */
      if (stat (cgifile, &buf) == -1)
	{
	  svz_log (LOG_ERROR, "cgi: stat: %s\n", SYS_ERROR);
	  exit (0);
	}

      /* set the appropriate user permissions */
      if (setgid (buf.st_gid) == -1)
	{
	  svz_log (LOG_ERROR, "cgi: setgid: %s\n", SYS_ERROR);
	  exit (0);
	}
      if (setuid (buf.st_uid) == -1)
	{
	  svz_log (LOG_ERROR, "cgi: setuid: %s\n", SYS_ERROR);
	  exit (0);
	}

      /* create the argv[] and envp[] pointers */
      argv[0] = cgifile;
      argv[1] = NULL;

      /* 
       * Execute the CGI script itself here. This will overwrite the 
       * current process.
       */
      if (execve (cgifile, argv, svz_envblock_get (envp)) == -1)
	{
	  svz_log (LOG_ERROR, "cgi: execve: %s\n", SYS_ERROR);
	  exit (0);
	}
    }
  else if (pid == -1)
    {
      if (errno == EAGAIN && --retries)
	{
	  /* sleep (1); */
	  goto retry;
	}

      /* ------ error forking new process ------ */
      svz_log (LOG_ERROR, "cgi: fork: %s\n", SYS_ERROR);
      svz_sock_printf (sock, HTTP_BAD_REQUEST "\r\n");
      http_error_response (sock, 400);
      sock->userflags |= HTTP_FLAG_DONE;
      return -1;
    }

  /* ------ still current (parent) process here ------ */

#ifdef SVZ_ENABLE_DEBUG
  svz_log (LOG_DEBUG, "http: cgi %s got pid %d\n", file + 1, pid);
#endif

  /* send http header response */
  if (http_cgi_accepted (sock) == -1)
    {
      sock->userflags |= HTTP_FLAG_DONE;
      return -1;
    }

#endif /* not __MINGW32__ */

  /* save the process id */
  http = sock->data;
  http->pid = pid;

  /* close the inherited http data handles */
  if (closehandle (out) == -1)
    {
      svz_log (LOG_ERROR, "cgi: close: %s\n", SYS_ERROR);
    }
  if (type == POST_METHOD)
    {
      /* close the reading end of the pipe for the post data */
      if (closehandle (in) == -1)
	{
	  svz_log (LOG_ERROR, "cgi: close: %s\n", SYS_ERROR);
	}
    }

  return 0;
}

/*
 * The http GET cgi request response.
 */
int
http_cgi_get_response (svz_socket_t *sock, char *request, int flags)
{
  svz_t_handle cgi2s[2];
  char *file;

  /* check if this is a cgi request at all */
  if ((file = http_check_cgi (sock, request)) == HTTP_NO_CGI)
    return -2;

  if (file == NULL)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      return -1;
    }

  /* create a pipe for the cgi script process */
  if (svz_pipe_create_pair (cgi2s) == -1)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return -1;
    }

  /* execute the cgi script in FILE */
  sock->userflags |= HTTP_FLAG_CGI;
  sock->flags |= SOCK_FLAG_RECV_PIPE;
  sock->read_socket = http_cgi_read;
  sock->pipe_desc[READ] = cgi2s[READ];
  svz_fd_cloexec ((int) cgi2s[READ]);

  if (http_cgi_exec (sock, INVALID_HANDLE, cgi2s[WRITE], 
		     file, request, GET_METHOD))
    {
      /* some error occurred here */
      sock->read_socket = svz_tcp_read_socket;
      svz_free (file);
      return -1;
    }
  svz_free (file);
  return 0;
}

/*
 * The http POST request response.
 */
int
http_post_response (svz_socket_t *sock, char *request, int flags)
{
  char *file;
  char *length;
  svz_t_handle s2cgi[2];
  svz_t_handle cgi2s[2];
  http_socket_t *http;

  /* get http socket structure */
  http = sock->data;

  /* is this a valid POST request ? */
  file = http_check_cgi (sock, request);
  if (file == NULL || file == HTTP_NO_CGI)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      return -1;
    }

  /* create a pair of pipes for the cgi script process */
  if (svz_pipe_create_pair (cgi2s) == -1)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return -1;
    }
  if (svz_pipe_create_pair (s2cgi) == -1)
    {
      svz_sock_printf (sock, HTTP_INTERNAL_ERROR "\r\n");
      http_error_response (sock, 500);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return -1;
    }

  /* get the content length from the header information */
  if ((length = http_find_property (http, "Content-length")) == NULL)
    {
      svz_sock_printf (sock, HTTP_BAD_REQUEST "\r\n");
      http_error_response (sock, 411);
      sock->userflags |= HTTP_FLAG_DONE;
      svz_free (file);
      return -1;
    }
  http->contentlength = svz_atoi (length);

  /* prepare everything for the cgi pipe handling */
  sock->pipe_desc[WRITE] = s2cgi[WRITE];
  sock->pipe_desc[READ] = cgi2s[READ];
  svz_fd_cloexec ((int) s2cgi[WRITE]);
  svz_fd_cloexec ((int) cgi2s[READ]);

  /* execute the cgi script in FILE */
  if (http_cgi_exec (sock, s2cgi[READ], cgi2s[WRITE], 
		     file, request, POST_METHOD))
    {
      /* some error occurred here */
      sock->read_socket = svz_tcp_read_socket;
      sock->write_socket = http_default_write;
      svz_free (file);
      return -1;
    }

  sock->write_socket = http_cgi_write;
  sock->flags |= SOCK_FLAG_SEND_PIPE;
  sock->userflags |= HTTP_FLAG_POST;

  svz_free (file);
  return 0;
}

#else /* ENABLE_HTTP_PROTO */

int http_cgi_dummy; /* Shut up compiler warnings. */

#endif /* not ENABLE_HTTP_PROTO */
