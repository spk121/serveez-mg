/*
 * http-cgi.c - http cgi implementation
 *
 * Copyright (C) 2000, 2001, 2003, 2004, 2007 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <netinet/in.h>

#include "libserveez.h"
#include "http-proto.h"
#include "http-core.h"
#include "http-cgi.h"

/*
 * Extended disconnect_socket callback for CGIs.  Handling CGI related
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
      if (close (sock->pipe_desc[READ]) == -1)
        svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);
      sock->pipe_desc[READ] = INVALID_HANDLE;
      sock->flags &= ~SOCK_FLAG_RECV_PIPE;
    }
  if (sock->pipe_desc[WRITE] != INVALID_HANDLE)
    {
      if (close (sock->pipe_desc[WRITE]) == -1)
        svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);
      sock->pipe_desc[WRITE] = INVALID_HANDLE;
      sock->flags &= ~SOCK_FLAG_SEND_PIPE;
    }

  /*
   * Try killing the cgi script.
   */
  if (http->pid != INVALID_HANDLE)
    {
      if (kill (http->pid, SIGKILL) == -1)
        svz_log (LOG_ERROR, "kill: %s\n", SYS_ERROR);
      /* Test if the cgi is still running and cleanup.  */
      else if (waitpid (http->pid, NULL, 0) == -1)
        svz_log (LOG_ERROR, "waitpid: %s\n", SYS_ERROR);
      http->pid = INVALID_HANDLE;
    }

  return http_disconnect (sock);
}

/*
 * This is the default idle function for http connections.  It checks
 * whether any died child was a cgi script.
 */
int
http_cgi_died (svz_socket_t *sock)
{
  http_socket_t *http = sock->data;

  if (sock->flags & SOCK_FLAG_PIPE)
    {
      /* Check if a died child is this cgi.  */
      if (svz_child_died && http->pid == svz_child_died)
        {
          svz_log (LOG_NOTICE, "cgi script pid %d died\n",
                   (int) svz_child_died);
          svz_child_died = 0;
        }
      /* Test if the cgi is still running.  */
      if (waitpid (http->pid, NULL, WNOHANG) == http->pid)
        {
          svz_log (LOG_NOTICE, "cgi script pid %d died\n", (int) http->pid);
          http->pid = INVALID_HANDLE;
        }
    }
  
  sock->idle_counter = 1;
  return 0;
}

/*
 * The http cgi reader gets data from the stdout of a cgi
 * program and stores the data into the send buffer of
 * the socket structure.  We set the HTTP_FLAG_DONE flag
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

  if ((num_read = read (sock->pipe_desc[READ],
			sock->send_buffer + sock->send_buffer_fill,
			do_read)) == -1)
    {
      svz_log (LOG_ERROR, "cgi: read: %s\n", SYS_ERROR);
      if (errno == EAGAIN)
        return 0;
      num_read = -1;
    }

  /* data has been read */
  else if (num_read > 0)
    {
      http->length += num_read;
      sock->send_buffer_fill += num_read;
      return 0;
    }

  /* no data has been received */
  sock->userflags |= HTTP_FLAG_DONE;
  if (sock->send_buffer_fill == 0)
    {
      svz_log (LOG_DEBUG, "cgi: data successfully received and resent\n");
      sock->userflags &= ~HTTP_FLAG_CGI;
      sock->flags &= ~SOCK_FLAG_RECV_PIPE;
      return -1;
    }

  return 0;
}

/*
 * HTTP_CGI_WRITE pipes all read data from the http socket connection
 * into the cgi stdin.  This is necessary for the so called post method.
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
   * were actually sent.  Do not write more than the content
   * length of the post data.
   */
  do_write = sock->recv_buffer_fill;
  if (do_write > http->contentlength)
    do_write = http->contentlength;

  if ((num_written = write (sock->pipe_desc[WRITE],
                            sock->recv_buffer, do_write)) == -1)
    {
      svz_log (LOG_ERROR, "cgi: write: %s\n", SYS_ERROR);
    }

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
      svz_log (LOG_DEBUG, "cgi: post data sent to cgi\n");
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
 * Create the environment block for a CGI script.  Depending on the
 * system the environment is a field of null terminated char pointers
 * (for Unices) followed by a null pointer or one char pointer where
 * the variables a separated by zeros and the block is terminated
 * by a further zero.  It returns the amount of defined variables.
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
        if (!strcasecmp (http->property[n], env_var[c].property))
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
 * Check the http option (the URL) for a cgi request.  This routine
 * parses the text of the request and delivers the real file to be
 * invoked.  This function makes sure that the cgi script file exists
 * and is executable.  On success it delivers a pointer which must be
 * svz_free()ed after use.
 */
char *
http_check_cgi (svz_socket_t *sock, char *request)
{
  struct stat buf;
  char *file;
  int fd;
  int size;
  char *p, *p2;
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
   * GET variables.  Anything between the trailing '?' and the
   * end of the CGI url goes in PATH_INFO.
   */

  /* store the request in a local variable */
  len = strlen (request) + 1 - strlen (cfg->cgiurl);
  saverequest = svz_malloc (len);
  strcpy (saverequest, request + strlen (cfg->cgiurl));

  /* find the actual CGI URL */
  p = saverequest;
  while (*p != '?' && *p != 0
	 && !(p > saverequest && *p == '/')) /* break on non-initial '/' */
    p++;
  *p = 0;

  size = strlen (cfg->cgidir) + len;
  file = svz_malloc (size);
  strcpy (file, cfg->cgidir);
  strncpy (file + strlen (cfg->cgidir), saverequest, p - saverequest);
  file[strlen (cfg->cgidir) + p - saverequest] = '\0';

  /* test if the file really exists and close it again */
  if ((fd = open (file, O_RDONLY)) == -1)
    {
      svz_log (LOG_ERROR, "cgi: open: %s (%s)\n", SYS_ERROR, file);
      svz_free (file);
      svz_free (saverequest);
      return NULL;
    }

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
  if (close (fd) == -1)
    svz_log (LOG_ERROR, "cgi: close: %s\n", SYS_ERROR);

  /* return a pointer referring to the actual plain cgi file */
  strncpy (file, saverequest, p - saverequest);
  file[p - saverequest] = '\0';
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
static char *
http_pre_exec (svz_socket_t *sock,   /* socket structure */
               svz_envblock_t *envp, /* environment block to be filled */
               char *file,           /* plain executable name */
               char *request,        /* original http request */
               int type)             /* POST or GET?  */
{
  char *cgidir;
  char *cgifile;
  char *p;
  http_config_t *cfg = sock->cfg;

  /* change into the CGI directory temporarily */
  if (chdir (cfg->cgidir) == -1)
    {
      svz_log (LOG_ERROR, "cgi: chdir: %s\n", SYS_ERROR);
      svz_log (LOG_DEBUG, "cgi: cannot change dir: %s\n", cfg->cgidir);
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

  if (type == GET_METHOD)
    {
      char *p_start;
      p_start = strstr (request, file);
      if (p_start == NULL)
	{
	  svz_log (LOG_ERROR, "cgi: file %s not found in query string %s",
		   file, request);
	  return NULL;
	}
      p_start += strlen (file);
      p = p_start;
      /* put the PATH_INFO into the env variables if necessary */
      if (*p == '/')
	{
	  char terminator;
	  while (*p != '?' && *p != 0)
	    p ++;
	  terminator = *p;
	  *p = 0;
	  svz_envblock_add (envp, "PATH_INFO=%s", p_start);
	  *p = terminator;
	}
      /* put the QUERY_STRING into the env variables if necessary */
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
static int
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
 * Invoke a cgi script.  In Unices we fork() us and in Win32 we
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

  char *argv[2];
  struct stat buf;
  int retries;
  int oflags;

  /* Assign local CGI disconnection routine.  */
  sock->disconnected_socket = http_cgi_disconnect;

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
      /* duplicate stderr to the cgi output */
      if (dup2 (out, 2) != 2)
        {
          svz_log (LOG_ERROR, "cgi: dup2: %s\n", SYS_ERROR);
          exit (0);
        }

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
       * Execute the CGI script itself here.  This will overwrite the
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

  svz_log (LOG_DEBUG, "http: cgi %s got pid %d\n", file + 1, pid);

  /* send http header response */
  if (http_cgi_accepted (sock) == -1)
    {
      sock->userflags |= HTTP_FLAG_DONE;
      return -1;
    }

  /* save the process id */
  http = sock->data;
  http->pid = pid;

  /* close the inherited http data handles */
  if (close (out) == -1)
    {
      svz_log (LOG_ERROR, "cgi: close: %s\n", SYS_ERROR);
    }
  if (type == POST_METHOD)
    {
      /* close the reading end of the pipe for the post data */
      if (close (in) == -1)
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
http_cgi_get_response (svz_socket_t *sock, char *request,
                       int flags __attribute__ ((unused)))
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
http_post_response (svz_socket_t *sock, char *request,
                    int flags __attribute__ ((unused)))
{
  char *file;
  char *length;
  svz_t_handle s2cgi[2];
  svz_t_handle cgi2s[2];
  http_socket_t *http;

  /* get http socket structure */
  http = sock->data;

  /* is this a valid POST request?  */
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
