/*
 * test/codec-test.c - codec tests
 *
 * Copyright (C) 2001, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
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

#ifdef __MINGW32__
# include <io.h>
#endif

#include "libserveez.h"

/*
 * Stdin reader for the codec test.  Reads as much data as available and
 * sets the socket flags to `SOCK_FLAG_FLUSH' if ready.  Invokes the
 * `check_request' callback each time some data has been received.  Very
 * likely any other `read_socket' callback.
 */
int
codec_recv (svz_socket_t *sock)
{
  int num_read, do_read;

  if ((do_read = sock->recv_buffer_size - sock->recv_buffer_fill) <= 0)
    return 0;
  num_read = read ((int) sock->pipe_desc[READ],
                   sock->recv_buffer + sock->recv_buffer_fill, do_read);
#ifndef __MINGW32__
  if (num_read < 0 && errno == EAGAIN)
    return 0;
#endif
  if (num_read <= 0)
    {
      sock->flags |= SOCK_FLAG_FLUSH;
      close ((int) sock->pipe_desc[READ]);
      num_read = 0;
    }
  sock->recv_buffer_fill += num_read;
  return sock->check_request (sock);
}

/*
 * Stdout writer.  Writes as much data as possible to stdout and removes
 * written bytes from the send buffer.  Very likely any other `write_socket'
 * callback.
 */
int
codec_send (svz_socket_t *sock)
{
  int num_written, do_write;

  if ((do_write = sock->send_buffer_fill) <= 0)
    return 0;
  num_written = write ((int) sock->pipe_desc[WRITE],
                       sock->send_buffer, do_write);
#ifndef __MINGW32__
  if (num_written < 0 && errno == EAGAIN)
    return 0;
#endif
  if (num_written <= 0)
    return -1;
  if (num_written < do_write)
    memmove (sock->send_buffer, sock->send_buffer + num_written,
             do_write - num_written);
  sock->send_buffer_fill -= num_written;
  return 0;
}

/* Most simple `check_request' callback I could think of.  Simply copies
   the receive buffer into the send buffer.  */
int
codec_check (svz_socket_t *sock)
{
  if (svz_sock_write (sock, sock->recv_buffer, sock->recv_buffer_fill))
    return -1;
  sock->recv_buffer_fill = 0;
  return 0;
}

/*
 * Main entry point for codec tests.
 */
int
main (int argc, char **argv)
{
  int result = 1, id, version;
  svz_socket_t *sock;
  svz_codec_t *codec;
  char *desc;

  /* Requires `codec' argument.  */
  if (argc < 2)
    {
      fprintf (stderr, "usage: %s codec < infile > outfile\n", argv[0]);
      return result;
    }

  /* Setup serveez core library.  */
  svz_boot ();
#if SVZ_ENABLE_DEBUG
  svz_config.verbosity = 9;
  svz_log_setfile (stderr);
#endif

#ifdef __MINGW32__
  setmode (fileno (stdin), O_BINARY);
  setmode (fileno (stdout), O_BINARY);
#endif

  /* Create single pipe socket for stdin and stdout.  */
  if ((sock = svz_pipe_create ((svz_t_handle) fileno (stdin),
                               (svz_t_handle) fileno (stdout))) == NULL)
    return result;
  sock->read_socket = codec_recv;
  sock->write_socket = codec_send;
  sock->check_request = codec_check;
  if (svz_sock_enqueue (sock))
    return result;
  id = sock->id;
  version = sock->version;

  /* Setup codecs.  */
  desc = argv[1];
  if ((codec = svz_codec_get (desc, SVZ_CODEC_ENCODER)) == NULL)
    {
      svz_codec_list ();
      return result;
    }
  if (svz_codec_sock_receive_setup (sock, codec))
    return result;
  if ((codec = svz_codec_get (desc, SVZ_CODEC_DECODER)) == NULL)
    {
      svz_codec_list ();
      return result;
    }
  if (svz_codec_sock_send_setup (sock, codec))
    return result;

  /* Run server loop.  */
  svz_loop_pre ();
  do
    {
      svz_loop_one ();
    }
  while (svz_sock_find (id, version) && !svz_nuke_happened);
  svz_loop_post ();

  /* Finalize the core API.  */
  svz_halt ();

#if SVZ_ENABLE_DEBUG
  if (svz_allocated_bytes || svz_allocated_blocks)
    return result;
#endif /* SVZ_ENABLE_DEBUG */

  return 0;
}
