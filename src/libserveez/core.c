/*
 * core.c - socket and file descriptor core implementations
 *
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: core.c,v 1.29 2003/06/14 14:57:59 ela Exp $
 *
 */

#include <stdio.h>              /* sprintf */
#include <string.h>             /* strcmp */
#include <errno.h>              /* errno */
#include <fcntl.h>              /* F_SETFL, fcntl */
#include <sys/stat.h>           /* fstat */
#include <sys/socket.h> /* SOL_SOCKET, struct sockaddr, connect getsockopt, socket, socketpair */
#include <netinet/in.h>         /* struct in_addr */
#include <arpa/inet.h>          /* inet_ntoa */
#include <netinet/tcp.h>        /* SOL_TCP, TCP_NODELAY, TCP_CORK */
#include <sys/sendfile.h>       /* sendfile */
#include <unistd.h>             /* close */

#include "core.h"
#include "util.h"

/*
 * Set the given file descriptor to nonblocking I/O. This heavily differs
 * in Win32 and Unix. The given file descriptor @var{fd} must be a socket
 * descriptor under Win32, otherwise the function fails. Return zero on
 * success, otherwise non-zero.
 */
int
svz_fd_nonblock (int fd)
{
  int flag;

  flag = fcntl (fd, F_GETFL);
  if (fcntl (fd, F_SETFL, flag | O_NONBLOCK) < 0)
    {
      svz_log (LOG_ERROR, "fcntl: %s\n", NET_ERROR);
      return -1;
    }

  return 0;
}

/*
 * Set the given file descriptor to blocking I/O. This routine is the
 * counter part to @code{svz_fd_nonblock()}.
 */
int
svz_fd_block (int fd)
{
  int flag;

  flag = fcntl (fd, F_GETFL);
  if (fcntl (fd, F_SETFL, flag & ~O_NONBLOCK) < 0)
    {
      svz_log (LOG_ERROR, "fcntl: %s\n", NET_ERROR);
      return -1;
    }

  return 0;
}

/*
 * Set the close-on-exec flag of the given file descriptor @var{fd} and
 * return zero on success. Otherwise return non-zero.
 */
int
svz_fd_cloexec (int fd)
{
  /* 
   * ... SNIP : from the cygwin mail archives 1999/05 ...
   * The problem is in socket() call on W95 - the socket returned 
   * is non-inheritable handle (unlike NT and Unixes, where
   * sockets are inheritable). To fix the problem DuplicateHandle 
   * call is used to create inheritable handle, and original 
   * handle is closed.
   * ... SNAP ...
   *
   * Thus here is NO NEED to set the FD_CLOEXEC flag and no
   * chance anyway.
   */

  int flag;

  flag = fcntl (fd, F_GETFD);
  if ((fcntl (fd, F_SETFD, flag | FD_CLOEXEC)) < 0)
    {
      svz_log (LOG_ERROR, "fcntl: %s\n", NET_ERROR);
      return -1;
    }


  return 0;
}

/*
 * This function creates an unnamed pair of connected sockets with the 
 * specified protocol @var{proto}. The descriptors used in referencing the 
 * new sockets are returned in desc[0] and desc[1]. The two sockets are 
 * indistinguishable. Also make both of them non-blocking and 
 * non-inheritable. Returns -1 on failure, otherwise zero.
 */
int
svz_socket_create_pair (int proto, svz_t_socket desc[2])
{
  int stype, ptype;

  /* Assign the appropriate socket type. */
  switch (proto)
    {
    case PROTO_TCP:
      stype = SOCK_STREAM;
      ptype = IPPROTO_IP;
      break;
    case PROTO_UDP:
      stype = SOCK_DGRAM;
      ptype = IPPROTO_UDP;
      break;
    case PROTO_ICMP:
      stype = SOCK_RAW;
      ptype = IPPROTO_ICMP;
      break;
    case PROTO_RAW:
      stype = SOCK_RAW;
      ptype = IPPROTO_RAW;
      break;
    default:
      stype = SOCK_STREAM;
      ptype = IPPROTO_IP;
      break;
    }

  /* Create the pair of sockets. */
  if (socketpair (AF_UNIX, stype, ptype, desc) < 0)
    {
      svz_log (LOG_ERROR, "socketpair: %s\n", NET_ERROR);
      return -1;
    }

  /* Make the sockets non-blocking and non-inheritable. */
  if (svz_fd_nonblock (desc[0]) != 0 || svz_fd_nonblock (desc[1]) != 0 ||
      svz_fd_cloexec (desc[0]) != 0 || svz_fd_cloexec (desc[1]) != 0)
    {
      close (desc[0]);
      close (desc[1]);
      return -1;
    }

  return 0;
}

/*
 * Create a new non-blocking socket which does not get inherited on 
 * @code{exec()}. The protocol is specified by @var{proto}. Return the
 * socket descriptor or -1 on errors.
 */
svz_t_socket
svz_socket_create (int proto)
{
  int stype;                 /* socket type (STREAM or DGRAM or RAW) */
  int ptype;                 /* protocol type (IP or UDP or ICMP) */
  svz_t_socket sockfd;

  /* Assign the appropriate socket type. */
  switch (proto)
    {
    case PROTO_TCP:
      stype = SOCK_STREAM;
      ptype = IPPROTO_IP;
      break;
    case PROTO_UDP:
      stype = SOCK_DGRAM;
      ptype = IPPROTO_UDP;
      break;
    case PROTO_ICMP:
      stype = SOCK_RAW;
      ptype = IPPROTO_ICMP;
      break;
      /* This protocol is for sending packets only. The kernel filters
	 any received packets by the socket protocol (here: IPPROTO_RAW
	 which is unspecified). */
    case PROTO_RAW:
      stype = SOCK_RAW;
      ptype = IPPROTO_RAW;
      break;
    default:
      stype = SOCK_STREAM;
      ptype = IPPROTO_IP;
      break;
    }

  /* Create a socket for communication with a server. */
  if ((sockfd = socket (AF_INET, stype, ptype)) == INVALID_SOCKET)
    {
      svz_log (LOG_ERROR, "socket: %s\n", NET_ERROR);
      return (svz_t_socket) -1;
    }

  /* Make the socket non-blocking. */
  if (svz_fd_nonblock (sockfd) != 0)
    {
      close (sockfd);
      return (svz_t_socket) -1;
    }
  
  /* Do not inherit this socket. */
  if (svz_fd_cloexec (sockfd) != 0)
    {
      close (sockfd);
      return (svz_t_socket) -1;
    }

  return sockfd;
}

/*
 * Saves the socket type (like @code{SOCK_STREAM}, @code{SOCK_DGRAM}, etc.) 
 * of the socket @var{fd} in the buffer pointed to by @var{type}. Returns
 * zero on success.
 */
int
svz_socket_type (svz_t_socket fd, int *type)
{
  int optval;
  socklen_t optlen = sizeof (optval);

  if (type)
    {
      if (getsockopt (fd, SOL_SOCKET, SO_TYPE, 
		      (void *) &optval, &optlen) < 0)
	{
	  svz_log (LOG_ERROR, "getsockopt: %s\n", NET_ERROR);
	  return -1;
	}
      *type = optval;
    }
  return 0;
}

/*
 * Connect the given socket descriptor @var{sockfd} to the host @var{host}
 * at the network port @var{port}. Return non-zero on errors.
 */
int
svz_socket_connect (svz_t_socket sockfd, 
		    unsigned long host, unsigned short port)
{
  struct sockaddr_in server;
  int error;

  /* Setup the server address. */
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = host;
  server.sin_port = port;
  
  /* Try to connect to the server, */
  if (connect (sockfd, (struct sockaddr *) &server, sizeof (server)) == -1)
    {
      error = errno;
      if (error != EINPROGRESS && error != EAGAIN)
        {
          svz_log (LOG_ERROR, "connect: %s\n", NET_ERROR);
          close (sockfd);
          return -1;
        }
      svz_log (LOG_DEBUG, "connect: %s\n", NET_ERROR);
    }
  return 0;
}

/*
 * Converts the given ip address @var{ip} to the dotted decimal 
 * representation. The string is a statically allocated buffer, please 
 * copy the result. The given ip address MUST be in network byte order.
 */
char *
svz_inet_ntoa (unsigned long ip)
{
  static char str[INET_ADDRSTRLEN + 1];
  inet_ntop(AF_INET, &ip, str, INET_ADDRSTRLEN);
  return str;
}

/*
 * Converts the Internet host address @var{str} from the standard 
 * numbers-and-dots notation into binary data and stores it in the 
 * structure that @var{addr} points to. @code{svz_inet_aton()} returns 
 * zero if the address is valid, nonzero if not.
 * This function handles an ip address of "*" special and sets 
 * @code{INADDR_ANY} for it.
 */
int
svz_inet_aton (char *str, struct sockaddr_in *addr)
{
  /* Handle "*" special: use INADDR_ANY for it */
  if (!strcmp (str, "*"))
    {
      /* FIXME: does that work ? */
      addr->sin_addr.s_addr = INADDR_ANY;
      return 0;
    }

  if (inet_pton (AF_INET, str, &addr->sin_addr) == 0)
    {
      return -1;
    }
  return 0;
}

/*
 * Enable or disable the @code{TCP_CORK} socket option of the given socket
 * descriptor @var{fd}. This is useful for performance reasons when using 
 * @code{sendfile()} with any prepending or trailing data not inside the 
 * file to transmit. The function return zero on success, otherwise non-zero.
 */
int
svz_tcp_cork (svz_t_socket fd, int set)
{
  int flags;

  /* get current socket options */
  if ((flags = fcntl (fd, F_GETFL)) < 0)
    {
      svz_log (LOG_ERROR, "fcntl: %s\n", NET_ERROR);
      return -1;
    }

  /* set or unset the cork option */
  flags = set ? flags | TCP_CORK : flags & ~TCP_CORK;

  /* apply new socket option */
  if (fcntl (fd, F_SETFL, flags) < 0)
    {
      svz_log (LOG_ERROR, "fcntl: %s\n", NET_ERROR);
      return -1;
    }

  return 0;
}

/*
 * Enable or disable the @code{TCP_NODELAY} setting for the given socket
 * descriptor @var{fd} depending on the flag @var{set}. In fact its turns 
 * the Nagle algorithm on or off. This means that packets are always sent 
 * as soon as possible and no unnecessary delays are introduced. The 
 * function saves the old setting if @var{old} is not @code{NULL}. Returns 
 * zero on success, otherwise non-zero.
 */
int
svz_tcp_nodelay (svz_t_socket fd, int set, int *old)
{
  int optval;
  socklen_t optlen = sizeof (optval);

  /* Get old setting if required. */
  if (old != NULL)
    {  
      if (getsockopt (fd, SOL_TCP, TCP_NODELAY, 
		      (void *) &optval, &optlen) < 0)
	{
	  svz_log (LOG_ERROR, "getsockopt: %s\n", NET_ERROR);
	  return -1;
	}
      *old = optval ? 1 : 0;
    }

  /* Set the setting. */
  optval = set ? 1 : 0;
  if (setsockopt (fd, SOL_TCP, TCP_NODELAY, 
		  (void *) &optval, sizeof (optval)) < 0)
    {
      svz_log (LOG_ERROR, "setsockopt: %s\n", NET_ERROR);
      return -1;
    }
  return 0;
}

/*
 * This function transmits data between one file descriptor and another 
 * where @var{in_fd} is the source and @var{out_fd} the destination. The
 * @var{offset} argument is a pointer to a variable holding the input file 
 * pointer position from which reading starts. When this routine returns,
 * the @var{offset} variable will be set to the offset of the byte following 
 * the last byte that was read. @var{count} is the number of bytes to copy 
 * between file descriptors. Returns the number of bytes actually 
 * read/written or -1 on errors.
 */
int
svz_sendfile (int out_fd, int in_fd, off_t *offset, unsigned int count)
{
  ssize_t ret;
  ret = sendfile (out_fd, in_fd, offset, count);
  return (int) ret;
}

/* List for file descriptors. */
static svz_array_t *svz_files = NULL;

/* Add a file descriptor to the list. */
static void
svz_file_add (int fd)
{
  if (svz_files == NULL)
    svz_files = svz_array_create (1, NULL);
  svz_array_add (svz_files, SVZ_NUM2PTR (fd));
}

/* Delete a file descriptor from the list. */
static void
svz_file_del (int fd)
{
  void *val;
  unsigned long n;

  svz_array_foreach (svz_files, val, n)
    {
      if (val == SVZ_NUM2PTR (fd))
	{
	  svz_array_del (svz_files, n);
	  break;
	}
    }
  if (svz_array_size (svz_files) == 0)
    {
      svz_array_destroy (svz_files);
      svz_files = NULL;
    }
}

/*
 * Close all file descriptors collected so far by the core API of serveez.
 * This should be called if @code{fork()} has been called without a 
 * following @code{exec()}.
 */
void
svz_file_closeall (void)
{
  void *fd;
  unsigned long n;

  svz_array_foreach (svz_files, fd, n)
    close ((int) SVZ_PTR2NUM (fd));
}
#
/*
 * Open the filename @var{file} and convert it into a file handle. The
 * given @var{flags} specify the access mode and the @var{mode} argument
 * the permissions if the @code{O_CREAT} flag is set.
 */
int
svz_open (const char *file, int flags, unsigned int mode)
{
  int fd;

  if ((fd = open (file, flags, (mode_t) mode)) < 0)
    {
      svz_log (LOG_ERROR, "open (%s): %s\n", file, SYS_ERROR);
      return -1;
    }
  if (svz_fd_cloexec (fd) < 0)
    {
      close (fd);
      return -1;
    }
  svz_file_add (fd);
  return fd;
}

/*
 * Close the given file handle @var{fd}. Return -1 on errors.
 */
int
svz_close (int fd)
{
  if (close (fd) < 0)
    {
      svz_log (LOG_ERROR, "close: %s\n", SYS_ERROR);
      return -1;
    }
  svz_file_del (fd);
  return 0;
}

/*
 * Conversion from FILETIME (100 nano-sec intervals from 1.1.1601) to
 * UTC time (seconds since 1.1.1970).
 */
#define DIFF_FT_LT                             \
  /* there have been 89 years with 366 days */ \
  ((((__int64) (1970 - 1601) * 365 + 89) * 24 * 3600) * 10000000L)

#define ft2lt(ft)                                                    \
  ((time_t) (((ft.dwLowDateTime | (__int64) ft.dwHighDateTime << 32) \
               - DIFF_FT_LT) / 10000000L))

/*
 * Return information about the specified file associated with the file
 * descriptor @var{fd} returned by @code{svz_open()}. Stores available
 * information in the stat buffer @var{buf}.
 */
int
svz_fstat (int fd, struct stat *buf)
{
  if (fstat (fd, buf) < 0)
    {
      svz_log (LOG_ERROR, "fstat: %s\n", SYS_ERROR);
      return -1;
    }
  return 0;
}

/*
 * Open the file whose name is the string pointed to by @var{file} and 
 * associates a stream with it.
 */
FILE *
svz_fopen (const char *file, const char *mode)
{
  FILE *f;

  if ((f = fopen (file, mode)) == NULL)
    {
      svz_log (LOG_ERROR, "fopen (%s): %s\n", file, SYS_ERROR);
      return NULL;
    }
  if (svz_fd_cloexec (fileno (f)) < 0)
    {
      fclose (f);
      return NULL;
    }
  svz_file_add (fileno (f));
  return f;
}

/*
 * Dissociates the named stream @var{f} from its underlying file.
 */
int
svz_fclose (FILE *f)
{
  svz_file_del (fileno (f));
  if (fclose (f) < 0)
    {
      svz_log (LOG_ERROR, "fclose: %s\n", SYS_ERROR);
      return -1;
    }
  return 0;
}

/*
 * Checks for the existence of the given file system node @var{file} and 
 * return zero on success.  Otherwise the function returns non-zero.
 */
int
svz_file_check (char *file)
{
  struct stat buf;
  return file ? stat (file, &buf) : -1;
}

/*
 * Constructs a fully qualified file name form @var{path} and @var{file}.
 * If @var{path} is omitted (@code{NULL}) the function returns @var{file}
 * only.  If @var{file} is @code{NULL} a null pointer is returned.
 * Please remember to @code{svz_free()} the returned pointer.
 */
char *
svz_file_path (char *path, char *file)
{
  char *full;

  if (file == NULL)
    return NULL;
  full = svz_malloc ((path ? strlen (path) + 1 : 0) + strlen (file) + 1);
  sprintf (full, "%s%s%s", path ? path : "", path ? "/" : "", file);
  return full;
}
