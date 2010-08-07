/*
 * bzip2.h - interface to the 'bzip2' block-sorting compression library
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
 * $Id: bzip2.h,v 1.2 2001/12/13 18:00:01 ela Exp $
 *
 */

#ifndef __BZIP2_H__
#define __BZIP2_H__ 1

#include "libserveez/defines.h"

/* Configuration structure for the bzip2 codec. */
typedef struct
{
  int blockSize100k; /* block size in 100 KByte */
  int verbosity;     /* verbosity */
  int workFactor;    /* workFactor */
  int small;         /* use an alternative decompression algorithm */
}
bzip2_config_t;

__BEGIN_DECLS

extern svz_codec_t bzip2_encoder;
extern svz_codec_t bzip2_decoder;
char * bzip2_error __PARAMS ((svz_codec_data_t *));
int bzip2_encoder_init __PARAMS ((svz_codec_data_t *));
int bzip2_encoder_finalize __PARAMS ((svz_codec_data_t *));
int bzip2_encode __PARAMS ((svz_codec_data_t *));
int bzip2_decoder_init __PARAMS ((svz_codec_data_t *));
int bzip2_decoder_finalize __PARAMS ((svz_codec_data_t *));
int bzip2_decode __PARAMS ((svz_codec_data_t *));
int bzip2_ratio __PARAMS ((svz_codec_data_t *, unsigned long *, 
			   unsigned long *));

__END_DECLS

#endif /* not __BZIP2_H__ */
