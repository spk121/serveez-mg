/*
 * codec.h - public codec interface definitions
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
 * $Id: codec.h,v 1.6 2001/12/13 18:00:01 ela Exp $
 *
 */

#ifndef __CODEC_H__
#define __CODEC_H__ 1

#include "defines.h"
#include "socket.h"

/* Modes of operation. */
#define SVZ_CODEC_INIT   0x0001
#define SVZ_CODEC_FLUSH  0x0002
#define SVZ_CODEC_RESET  0x0004
#define SVZ_CODEC_FINISH 0x0008
#define SVZ_CODEC_CODE   0x0010

/* Return values of the codec callbacks. */
#define SVZ_CODEC_OK       0x0001
#define SVZ_CODEC_FINISHED 0x0002
#define SVZ_CODEC_ERROR    0x0004
#define SVZ_CODEC_MORE_OUT 0x0008
#define SVZ_CODEC_MORE_IN  0x0010

/* Internal state of a codec. */
#define SVZ_CODEC_NONE  0x0000
#define SVZ_CODEC_READY 0x0001

/* Codec types. */
#define SVZ_CODEC_ENCODER 0x0001
#define SVZ_CODEC_DECODER 0x0002

typedef struct svz_codec svz_codec_t;
typedef struct svz_codec_data svz_codec_data_t;

/*
 * General codec data structure for both encoding and encoding calls.
 */
struct svz_codec_data
{
  /* Current codec class. */
  svz_codec_t *codec;

  /* Operation flags. */
  int flag;

  /* Current state flags. */
  int state;

  /* Input buffer description. */
  char *in_buffer;
  int in_fill;
  int in_size;

  /* Output buffer description. */
  char *out_buffer;
  int out_fill;
  int out_size;

  /* Configuration field (passed to each codec callback). Could be
     used as compression level, algorithm, etc. indicator. */
  void *config;

  /* Arbitrary data field. Can be used by codec for internal data. */
  void *data;

  /* Saved @code{check_request} callback. Used by receiving codecs. */
  int (* check_request) (svz_socket_t *sock);

  /* Saved @code{write_socket} callback. Used by sending codecs. */
  int (* write_socket) (svz_socket_t *sock);

  /* Saved @code{disconnected_socket} callback. Used by both. */
  int (* disconnected_socket) (svz_socket_t *sock);
};

/*
 * Description of a codec class.
 */
struct svz_codec
{
  /* Name of the codec. Should be short descriptive name. */
  char *description;

  /* Codec type. */
  int type;

  /* Initializer. */
  int (* init) (svz_codec_data_t *);

  /* Finalizer. */
  int (* finalize) (svz_codec_data_t *);

  /* Encoding / decoding routine. */
  int (* code) (svz_codec_data_t *);

  /* Last error description. */
  char * (* error) (svz_codec_data_t *);

  /* Overall ratio request. */
  int (* ratio) (svz_codec_data_t *, unsigned long *, unsigned long *);

  /* Magic detection sequence. */
  char *detection;

  /* Length of the above detection sequence. */
  int detection_size;
};

__BEGIN_DECLS

/* Exported functions. */
SERVEEZ_API void svz_codec_list __PARAMS ((void));
SERVEEZ_API svz_codec_t * svz_codec_get __PARAMS ((char *, int));
SERVEEZ_API int svz_codec_init __PARAMS ((void));
SERVEEZ_API int svz_codec_finalize __PARAMS ((void));
SERVEEZ_API int svz_codec_register __PARAMS ((svz_codec_t *));
SERVEEZ_API int svz_codec_unregister __PARAMS ((svz_codec_t *));
SERVEEZ_API int svz_codec_sock_receive_setup __PARAMS ((svz_socket_t *, 
							svz_codec_t *));
SERVEEZ_API int svz_codec_sock_receive __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_codec_sock_send_setup __PARAMS ((svz_socket_t *, 
						     svz_codec_t *));
SERVEEZ_API int svz_codec_sock_send __PARAMS ((svz_socket_t *));
SERVEEZ_API int svz_codec_sock_disconnect __PARAMS ((svz_socket_t *));
SERVEEZ_API void svz_codec_ratio __PARAMS ((svz_codec_t *, 
					    svz_codec_data_t *));
SERVEEZ_API svz_codec_t * svz_codec_sock_detect __PARAMS ((svz_socket_t *));

__END_DECLS

#endif /* not __CODEC_H__ */
