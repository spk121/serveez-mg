/*
 * gzlib.h - interface to the 'zlib' compression library
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
 * $Id: gzlib.h,v 1.3 2001/12/13 18:00:01 ela Exp $
 *
 */

#ifndef __GZLIB_H__
#define __GZLIB_H__ 1

#include "defines.h"

__BEGIN_DECLS

extern svz_codec_t zlib_encoder;
extern svz_codec_t zlib_decoder;
char * zlib_error __PARAMS ((svz_codec_data_t *));
int zlib_encoder_init __PARAMS ((svz_codec_data_t *));
int zlib_encoder_finalize __PARAMS ((svz_codec_data_t *));
int zlib_encode __PARAMS ((svz_codec_data_t *));
int zlib_decoder_init __PARAMS ((svz_codec_data_t *));
int zlib_decoder_finalize __PARAMS ((svz_codec_data_t *));
int zlib_decode __PARAMS ((svz_codec_data_t *));
int zlib_ratio __PARAMS ((svz_codec_data_t *, unsigned long *, 
			  unsigned long *));

__END_DECLS

#endif /* not __GZLIB_H__ */
