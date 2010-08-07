/*
 * codec.c - basic codec interface implementation
 *
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: codec.c,v 1.10 2002/06/02 16:22:27 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#define _GNU_SOURCE
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/array.h"
#include "libserveez/socket.h"
#include "libserveez/codec/codec.h"

/* Return text representation for codec type. */
#define SVZ_CODEC_TYPE_TEXT(codec)                          \
  (((codec)->type == SVZ_CODEC_DECODER) ? "decoder" :       \
   ((codec)->type == SVZ_CODEC_ENCODER) ? "encoder" : NULL)

/* Include codec headers if any. */
#if HAVE_ZLIB && HAVE_ZLIB_H
#include "libserveez/codec/gzlib.h"
#endif
#if HAVE_BZ2LIB
#include "libserveez/codec/bzip2.h"
#endif

/* Collection of available encoder and decoder provided by the libcodec
   archive or by external (shared) libraries. */
static svz_array_t *svz_codecs = NULL;

/* Find an appropriate codec for the given @var{description} and @var{type}
   which can be either @code{SVZ_CODEC_ENCODER} or @code{SVZ_CODEC_DECODER}.
   The function returns @code{NULL} if there is no such codec registered. */
svz_codec_t *
svz_codec_get (char *description, int type)
{
  int i;
  svz_codec_t *codec;

  if (description == NULL)
    return NULL;

  svz_array_foreach (svz_codecs, codec, i)
    {
      if (strcmp (description, codec->description) == 0 &&
	  type == codec->type)
	return codec;
    }
  return NULL;
}

/* Prints the text representation of the list of known codecs registered 
   within the core library. This includes all encoder and decoder once ran
   through @code{svz_codec_register()}. */
void
svz_codec_list (void)
{
  int n;
  svz_codec_t *codec;

  fprintf (stderr, "--- list of available codecs ---");

  /* Print encoder list. */
  fprintf (stderr, "\n\tencoder:");
  svz_array_foreach (svz_codecs, codec, n)
    if (codec->type == SVZ_CODEC_ENCODER)
      fprintf (stderr, " %s", codec->description);

  /* Print decoder list. */
  fprintf (stderr, "\n\tdecoder:");
  svz_array_foreach (svz_codecs, codec, n)
    if (codec->type == SVZ_CODEC_DECODER)
      fprintf (stderr, " %s", codec->description);

  fprintf (stderr, "\n");
}

/* This routine is called by @code{svz_boot()} and registers the builtin
   codecs. */
int
svz_codec_init (void)
{
#if HAVE_ZLIB && HAVE_ZLIB_H
  svz_codec_register (&zlib_encoder);
  svz_codec_register (&zlib_decoder);
#endif /* HAVE_ZLIB && HAVE_ZLIB_H */
#if HAVE_BZ2LIB
  svz_codec_register (&bzip2_encoder);
  svz_codec_register (&bzip2_decoder);
#endif /* HAVE_BZ2LIB */
  return 0;
}

/* This routine is called by @code{svz_halt()} and destroys the list of
   known codecs. */
int
svz_codec_finalize (void)
{
  if (svz_codecs)
    {
      svz_array_destroy (svz_codecs);
      svz_codecs = NULL;
    }
  return 0;
}

/* Checks the given codec for validity. Return zero on success. */
static int
svz_codec_check (svz_codec_t *codec)
{
  if (codec == NULL || codec->description == NULL ||
      (codec->type != SVZ_CODEC_DECODER && codec->type != SVZ_CODEC_ENCODER))
    return -1;
  return 0;
}

/* Register the given codec @var{codec}. Does not register invalid or 
   duplicate codecs. Returns zero on success, non-zero otherwise. */
int
svz_codec_register (svz_codec_t *codec)
{
  svz_codec_t *c;
  int i;

  /* Check validity of the codec. */
  if (svz_codec_check (codec))
    {
      svz_log (LOG_ERROR, "cannot register invalid codec\n");
      return -1;
    }

  /* Check for duplicate codecs. */
  svz_array_foreach (svz_codecs, c, i)
    {
      if (strcmp (c->description, codec->description) == 0 &&
	  c->type == codec->type)
	{
	  svz_log (LOG_ERROR, "cannot register duplicate codec `%s'\n",
		   codec->description);
	  return -1;
	}
    }

  /* Add this codec to the list of known codecs. */
  if (svz_codecs == NULL)
    svz_codecs = svz_array_create (2, NULL);
  svz_array_add (svz_codecs, codec);
  svz_log (LOG_NOTICE, "registered `%s' %s\n", codec->description,
	   SVZ_CODEC_TYPE_TEXT (codec));
  return 0;
}

/* Removes the given codec @var{codec} from the list of known codecs. Returns
   zero if the codec could be successfully removed, non-zero otherwise. */
int
svz_codec_unregister (svz_codec_t *codec)
{
  svz_codec_t *c;
  int i;

  /* Check validity of the codec. */
  if (svz_codec_check (codec))
    {
      svz_log (LOG_ERROR, "cannot unregister invalid codec\n");
      return -1;
    }

  /* Find codec within the list of known codecs. */
  svz_array_foreach (svz_codecs, c, i)
    {
      if (strcmp (c->description, codec->description) == 0 &&
	  c->type == codec->type)
	{
	  svz_array_del (svz_codecs, i);
	  svz_log (LOG_NOTICE, "unregistered `%s' %s\n", codec->description,
		   SVZ_CODEC_TYPE_TEXT (codec));
	  return 0;
	}
    }

  svz_log (LOG_ERROR, "cannot unregister codec `%s'\n",
	   codec->description);
  return -1;
}

/* Print a text representation of a codec's current ratio in percent
   if possible. */
void
svz_codec_ratio (svz_codec_t *codec, svz_codec_data_t *data)
{
  unsigned long in = 0, out = 0;

  if (codec->ratio == NULL)
    return;
  if (codec->ratio (data, &in, &out) == SVZ_CODEC_OK)
    {
      if (in != 0)
	svz_log (LOG_NOTICE, "%s: %s ratio is %lu.%02lu%%\n",
		 codec->description, SVZ_CODEC_TYPE_TEXT (codec),
		 out * 100UL / in, (out * 10000UL / in) % 100UL);
      else
	svz_log (LOG_NOTICE, "%s: %s ratio is infinite\n",
		 codec->description, SVZ_CODEC_TYPE_TEXT (codec));
    }
}

/* The following four (4) macros are receive buffer switcher used in order
   to apply the output buffer of the codec to the receive buffer of a socket
   structure and to revert these changes. */

#define svz_codec_set_recv_buffer(sock, data) \
  do {                                        \
    sock->recv_buffer = data->out_buffer;     \
    sock->recv_buffer_size = data->out_size;  \
    sock->recv_buffer_fill = data->out_fill;  \
  } while (0)

#define svz_codec_unset_recv_buffer(sock, data) \
  do {                                          \
    data->out_buffer = sock->recv_buffer;       \
    data->out_size = sock->recv_buffer_size;    \
    data->out_fill = sock->recv_buffer_fill;    \
  } while (0)

#define svz_codec_save_recv_buffer(sock, data) \
  do {                                         \
    data->in_buffer = sock->recv_buffer;       \
    data->in_fill = sock->recv_buffer_fill;    \
    data->in_size = sock->recv_buffer_size;    \
  } while (0)

#define svz_codec_restore_recv_buffer(sock, data) \
  do {                                            \
    sock->recv_buffer = data->in_buffer;          \
    sock->recv_buffer_size = data->in_size;       \
    sock->recv_buffer_fill = data->in_fill;       \
  } while (0)

/* Reverts the changes made in @code{svz_codec_sock_receive_setup()}. */
static void
svz_codec_sock_recv_revert (svz_socket_t *sock)
{
  svz_codec_data_t *data = (svz_codec_data_t *) sock->recv_codec;

  svz_codec_restore_recv_buffer (sock, data);
  sock->check_request = data->check_request;
  sock->disconnected_socket = data->disconnected_socket;
  svz_free (data->out_buffer);
  svz_free (sock->recv_codec);
  sock->recv_codec = NULL;
}

/* Setup the given socket structure @var{sock} to decode or encode its
   receive data via the codec @var{codec}. Therefore you must have setup
   the @code{check_request} method previously. The function returns zero
   on success, non-zero otherwise. */
int
svz_codec_sock_receive_setup (svz_socket_t *sock, svz_codec_t *codec)
{
  svz_codec_data_t *data;

  if (sock->recv_codec != NULL)
    return 0;

  /* Setup internal codec data. */
  data = svz_calloc (sizeof (svz_codec_data_t));
  data->codec = codec;
  data->flag = SVZ_CODEC_INIT;
  data->state = SVZ_CODEC_NONE;
  data->config = data->data = NULL;
  sock->recv_codec = data;

  /* Save the given sockets receive buffer, @code{check_request} callback
     and @code{disconnected_socket} callback if necessary. */
  svz_codec_save_recv_buffer (sock, data);
  data->check_request = sock->check_request;
  sock->check_request = svz_codec_sock_receive;
  if (sock->disconnected_socket != svz_codec_sock_disconnect)
    {
      data->disconnected_socket = sock->disconnected_socket;
      sock->disconnected_socket = svz_codec_sock_disconnect;
    }

  /* Apply new receive buffer which is the output buffer of the codec. */
  data->out_fill = 0;
  data->out_size = sock->recv_buffer_size;
  data->out_buffer = svz_malloc (data->out_size);

  /* Initialize the codec. */
  if (codec->init (data) == SVZ_CODEC_ERROR)
    {
      svz_log (LOG_ERROR, "%s: init: %s\n", 
	       codec->description, codec->error (data));
      svz_codec_sock_recv_revert (sock);
      return -1;
    }
  data->state |= SVZ_CODEC_READY;
  svz_log (LOG_NOTICE, "%s: %s initialized\n", codec->description,
	   SVZ_CODEC_TYPE_TEXT (codec));
  return 0;
}

/* This routine is the new @code{check_request} callback for reading codecs.
   It is applied in the above @code{svz_codec_sock_receive_setup()} function. 
   Usually it gets called whenever there is data in the receive buffer. It 
   lets the current receive buffer be the input of the codec. The output 
   buffer of the codec gets the new receive buffer buffer of the socket 
   structure @var{sock}. The old @code{check_request} callback of @var{sock}
   gets called afterwards. When leaving this functions the receive buffer 
   gets restored again with the bytes snipped consumed by the codec itself. */
int
svz_codec_sock_receive (svz_socket_t *sock)
{
  svz_codec_data_t *data = (svz_codec_data_t *) sock->recv_codec;
  svz_codec_t *codec = data->codec;
  int ret;

  /* Check internal state of codec first. */
  if (!(data->state & SVZ_CODEC_READY))
    {
      return 0;
    }

  /* Run the encoder / decoder of the applied codec. */
  data->flag = SVZ_CODEC_CODE;
  if (sock->flags & SOCK_FLAG_FLUSH)
    data->flag = SVZ_CODEC_FINISH;
  svz_codec_save_recv_buffer (sock, data);
  while ((ret = codec->code (data)) == SVZ_CODEC_MORE_OUT)
    {
      /* Resize output buffer if necessary. */
      data->flag |= SVZ_CODEC_FLUSH;
      data->out_size *= 2;
      data->out_buffer = svz_realloc (data->out_buffer, data->out_size);
    }

  /* Evaluate the return value of the codec. */
  switch (ret)
    {
    case SVZ_CODEC_ERROR:    /* Error occurred. */
      svz_log (LOG_ERROR, "%s: code: %s\n", 
	       codec->description, codec->error (data));
      return -1;

    case SVZ_CODEC_FINISHED: /* Codec finished. */
      svz_codec_ratio (codec, data);
      if (codec->finalize (data) != SVZ_CODEC_OK)
	{
	  svz_log (LOG_ERROR, "%s: finalize: %s\n", 
		   codec->description, codec->error (data));
	}
      else
	{
	  data->state &= ~SVZ_CODEC_READY;
	  svz_log (LOG_NOTICE, "%s: %s finalized\n", 
		   codec->description, SVZ_CODEC_TYPE_TEXT (codec));
	}
      break;

    case SVZ_CODEC_OK:       /* No error. */
      break;

    case SVZ_CODEC_MORE_IN:  /* Needs more input data to continue. */
      break;

    default:                 /* Unhandled. */
      svz_log (LOG_ERROR, "%s: code: invalid return value: %d\n",
	       codec->description, ret);
      break;
    }

  /* Call the saved @code{check_request} callback with the new receive 
     buffer which is the output buffer of the codec. */
  svz_codec_set_recv_buffer (sock, data);
  if ((ret = data->check_request (sock)) != 0)
    {
      svz_codec_unset_recv_buffer (sock, data);
      return ret;
    }

  /* Save back changes made to the current receive buffer. */
  svz_codec_unset_recv_buffer (sock, data);
  /* Restore old receive buffer. */
  svz_codec_restore_recv_buffer (sock, data);
  return 0;
}

/* These are send buffer switcher used to apply the output buffer of a
   sending codec to the send buffer of a socket structure and vice-versa. */

#define svz_codec_set_send_buffer(sock, data) \
  do {                                        \
    sock->send_buffer = data->out_buffer;     \
    sock->send_buffer_size = data->out_size;  \
    sock->send_buffer_fill = data->out_fill;  \
  } while (0)

#define svz_codec_unset_send_buffer(sock, data) \
  do {                                          \
    data->out_buffer = sock->send_buffer;       \
    data->out_size = sock->send_buffer_size;    \
    data->out_fill = sock->send_buffer_fill;    \
  } while (0)

#define svz_codec_save_send_buffer(sock, data) \
  do {                                         \
    data->in_buffer = sock->send_buffer;       \
    data->in_fill = sock->send_buffer_fill;    \
    data->in_size = sock->send_buffer_size;    \
  } while (0)

#define svz_codec_restore_send_buffer(sock, data) \
  do {                                            \
    sock->send_buffer = data->in_buffer;          \
    sock->send_buffer_size = data->in_size;       \
    sock->send_buffer_fill = data->in_fill;       \
  } while (0)

/* Reverts the changes made in @code{svz_codec_sock_send_setup()}. */
static void
svz_codec_sock_send_revert (svz_socket_t *sock)
{
  svz_codec_data_t *data = (svz_codec_data_t *) sock->send_codec;

  svz_codec_restore_send_buffer (sock, data);
  sock->check_request = data->check_request;
  sock->disconnected_socket = data->disconnected_socket;
  svz_free (data->out_buffer);
  svz_free (sock->send_codec);
  sock->send_codec = NULL;
}

/* Setup the socket structure @var{sock} for encoding or decoding its send
   buffer via the given codec @var{codec}. Therefore you previously need to
   assign the @code{write_socket} member of @var{sock} properly. The function
   returns zero on success, non-zero otherwise. */
int
svz_codec_sock_send_setup (svz_socket_t *sock, svz_codec_t *codec)
{
  svz_codec_data_t *data;

  /* Return here if there is already a codec registered. */
  if (sock->send_codec != NULL)
    return 0;

  /* Setup internal codec data. */
  data = svz_calloc (sizeof (svz_codec_data_t));
  data->codec = codec;
  data->flag = SVZ_CODEC_INIT;
  data->state = SVZ_CODEC_NONE;
  data->config = data->data = NULL;
  sock->send_codec = data;

  /* Save the given sockets send buffer, the @code{write_socket} callback
     and the @code{disconnected_socket} callback if necessary. */
  svz_codec_save_send_buffer (sock, data);
  data->write_socket = sock->write_socket;
  sock->write_socket = svz_codec_sock_send;
  if (sock->disconnected_socket != svz_codec_sock_disconnect)
    {
      data->disconnected_socket = sock->disconnected_socket;
      sock->disconnected_socket = svz_codec_sock_disconnect;
    }

  /* Apply new send buffer which is the output buffer of the codec. */
  data->out_fill = 0;
  data->out_size = sock->send_buffer_size;
  data->out_buffer = svz_malloc (data->out_size);

  /* Initialize the codec. */
  if (codec->init (data) == SVZ_CODEC_ERROR)
    {
      svz_log (LOG_ERROR, "%s: init: %s\n", 
	       codec->description, codec->error (data));
      svz_codec_sock_send_revert (sock);
      return -1;
    }
  data->state |= SVZ_CODEC_READY;
  svz_log (LOG_NOTICE, "%s: %s initialized\n", codec->description,
	   SVZ_CODEC_TYPE_TEXT (codec));
  return 0;
}

/* This is a codec socket structures @var{sock} new @code{write_socket}
   callback which is called whenever there is data within the send buffer
   available and @var{sock} is scheduled for writing. It uses the current
   send buffer as input buffer for the codec. The output buffer of the codec
   is used to invoke the @code{write_socket} callback saved within
   @code{svz_codec_sock_send_setup()}. After this the send buffer is 
   restored again without the bytes consumed by the codec. */
int
svz_codec_sock_send (svz_socket_t *sock)
{
  svz_codec_data_t *data = (svz_codec_data_t *) sock->send_codec;
  svz_codec_t *codec = data->codec;
  int ret;

  /* Check internal state of codec first. */
  if (!(data->state & SVZ_CODEC_READY))
    {
      return 0;
    }

  /* Run the encoder / decoder of the applied codec. */
  data->flag = SVZ_CODEC_CODE;
  if (sock->flags & SOCK_FLAG_FLUSH)
    data->flag = SVZ_CODEC_FINISH;
  svz_codec_save_send_buffer (sock, data);
  while ((ret = codec->code (data)) == SVZ_CODEC_MORE_OUT)
    {
      /* Resize output buffer if necessary. */
      data->flag |= SVZ_CODEC_FLUSH;
      data->out_size *= 2;
      data->out_buffer = svz_realloc (data->out_buffer, data->out_size);
    }

  /* Evaluate the return value of the codec. */
  switch (ret)
    {
    case SVZ_CODEC_ERROR:    /* Error occurred. */
      svz_log (LOG_ERROR, "%s: code: %s\n", 
	       codec->description, codec->error (data));
      return -1;

    case SVZ_CODEC_FINISHED: /* Codec finished. */
      svz_codec_ratio (codec, data);
      if (codec->finalize (data) != SVZ_CODEC_OK)
	{
	  svz_log (LOG_ERROR, "%s: finalize: %s\n", 
		   codec->description, codec->error (data));
	}
      else
	{
	  data->state &= ~SVZ_CODEC_READY;
	  svz_log (LOG_NOTICE, "%s: %s finalized\n", 
		   codec->description, SVZ_CODEC_TYPE_TEXT (codec));
	}
      break;

    case SVZ_CODEC_OK:       /* No error. */
      break;

    case SVZ_CODEC_MORE_IN:  /* Needs more input data to continue. */
      break;

    default:                 /* Unhandled. */
      svz_log (LOG_ERROR, "%s: code: invalid return value: %d\n",
	       codec->description, ret);
      break;
    }

  /* Call the saved @code{write_socket} callback with the new send 
     buffer which is the output buffer of the codec. */
  svz_codec_set_send_buffer (sock, data);
  if ((ret = data->write_socket (sock)) != 0)
    {
      svz_codec_unset_send_buffer (sock, data);
      return ret;
    }

  /* Save back changes made to the current send buffer. */
  svz_codec_unset_send_buffer (sock, data);
  /* Restore old send buffer. */
  svz_codec_restore_send_buffer (sock, data);
  return 0;
}

/* This callback is used as the @code{disconnected_socket} callback of the
   socket structure @var{sock}. It tries to release the resources of both
   the receiving and sending codec of @var{sock}. The routine is called by
   default if the codec socket structure @var{sock} gets disconnected for 
   some external reason. */
int
svz_codec_sock_disconnect (svz_socket_t *sock)
{
  svz_codec_data_t *data;
  int (* disconnected) (svz_socket_t *);

  disconnected = NULL;

  /* Check and release receiving codec. */
  if ((data = (svz_codec_data_t *) sock->recv_codec) != NULL)
    {
      disconnected = data->disconnected_socket;
      if (data->state & SVZ_CODEC_READY)
	data->codec->finalize (data);
      svz_codec_sock_recv_revert (sock);
    }
  /* Check and release sending codec. */
  if ((data = (svz_codec_data_t *) sock->send_codec) != NULL)
    {
      disconnected = data->disconnected_socket;
      if (data->state & SVZ_CODEC_READY)
	data->codec->finalize (data);
      svz_codec_sock_send_revert (sock);
    }

  /* Run old @code{disconnected_socket} callback. */
  if (disconnected != NULL)
    return disconnected (sock);
  return 0;
}

/* This routine can be used to detect a codec on the receive buffer of the
   given socket structure @var{sock}. It returns a valid codec or @code{NULL}
   if no codec could be detected. */
svz_codec_t *
svz_codec_sock_detect (svz_socket_t *sock)
{
  svz_codec_t *codec;
  int i;

  svz_array_foreach (svz_codecs, codec, i)
    {
      if (codec->detection_size > 0 && 
	  sock->recv_buffer_fill >= codec->detection_size)
	{
	  if (memcmp (sock->recv_buffer,
		      codec->detection, codec->detection_size) == 0)
	    {
	      svz_log (LOG_NOTICE, "%s: %s detected\n", codec->description,
		       SVZ_CODEC_TYPE_TEXT (codec));
	      return codec;
	    }
	}
    }
  return NULL;
}
