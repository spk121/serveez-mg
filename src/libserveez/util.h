/*
 * util.h - utility function interface
 *
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
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

#ifndef __UTIL_H__
#define __UTIL_H__ 1

#include "defines.h"

#include <stdio.h>              /* FILE * */
#include <string.h>             /* strerror */
#include <errno.h>              /* errno */

/* 
 * level of server's verbosity:
 * 0 - only fatal error messages
 * 1 - error messages
 * 2 - warnings
 * 3 - informational messages
 * 4 - debugging output
 * levels always imply numerically lesser levels
 */
#define LOG_FATAL     0
#define LOG_ERROR     1
#define LOG_WARNING   2
#define LOG_NOTICE    3
#define LOG_DEBUG     4

__BEGIN_DECLS

SERVEEZ_API void svz_log (int, const char *, ...);
SERVEEZ_API void svz_log_setfile (FILE *);

SERVEEZ_API int svz_hexdump (FILE *, char *, int, char *, int, int);
SERVEEZ_API char *svz_itoa (unsigned int);
SERVEEZ_API unsigned int svz_atoi (char *);
SERVEEZ_API char *svz_getcwd (void);
SERVEEZ_API int svz_openfiles (int);
SERVEEZ_API char *svz_time (long);
SERVEEZ_API char *svz_uptime (long);
SERVEEZ_API char *svz_tolower (char *);
SERVEEZ_API char *svz_sys_version (void);

/*
 * Convert the byte array pointed to by @var{p} to a signed 32 bit integer.
 * This is needed on aligned architectures where a plain type cast ends up
 * in a fatal bus error.
 */
#define SVZ_INT32(p) \
  ((unsigned char) *p | ((unsigned char) *(p + 1) << 8) | \
  ((unsigned char) *(p + 2) << 16) | ((signed char) *(p + 3) << 24))

/*
 * Convert the byte array pointed to by @var{p} to a signed 64 bit integer.
 */
#define SVZ_INT64(p) \
  ((unsigned char) *p | ((unsigned char) *(p + 1) << 8) | \
  ((unsigned char) *(p + 2) << 16) | ((unsigned char) *(p + 3) << 24) \
  ((unsigned char) *(p + 2) << 32) | ((unsigned char) *(p + 3) << 40) \
  ((unsigned char) *(p + 2) << 48) | ((signed char) *(p + 3) << 54))

/*
 * Convert the byte array pointed to by @var{p} to a signed 16 bit integer.
 */
#define SVZ_INT16(p) \
  ((unsigned char) *p | ((signed char) *(p + 1) << 8))

/*
 * Convert the byte array pointed to by @var{p} to an unsigned 32 bit integer.
 */
#define SVZ_UINT32(p) \
  ((unsigned char) *p | ((unsigned char) *(p + 1) << 8) | \
  ((unsigned char) *(p + 2) << 16) | ((unsigned char) *(p + 3) << 24))

/*
 * Convert the byte array pointed to by @var{p} to an unsigned 64 bit integer.
 */
#define SVZ_UINT64(p) \
  ((unsigned char) *p | ((unsigned char) *(p + 1) << 8) | \
  ((unsigned char) *(p + 2) << 16) | ((unsigned char) *(p + 3) << 24) \
  ((unsigned char) *(p + 2) << 32) | ((unsigned char) *(p + 3) << 40) \
  ((unsigned char) *(p + 2) << 48) | ((unsigned char) *(p + 3) << 54))

/*
 * Convert the byte array pointed to by @var{p} to an unsigned 16 bit integer.
 */
#define SVZ_UINT16(p) \
  ((unsigned char) *p | ((unsigned char) *(p + 1) << 8))

/* Converts the integer value @var{n} into a pointer platform independently.
   Both of the @code{SVZ_NUM2PTR()} and @code{SVZ_PTR2NUM()} macros rely on
   the @code{(unsigned long)} having the same size as @code{(void *)}.  */
#define SVZ_NUM2PTR(n) \
  ((void *) ((unsigned long) (n)))

/* Convert the pointer @var{p} into a integer value platform independently.  */
#define SVZ_PTR2NUM(p) \
  ((unsigned long) ((void *) (p)))

#define INVALID_SOCKET    ((svz_t_socket) -1)
#define INVALID_HANDLE    ((svz_t_handle) -1)

__END_DECLS

/* Definition of very system dependent routines.  */
#define SYS_ERROR strerror (errno)
#define NET_ERROR strerror (errno)

#endif /* not __UTIL_H__ */
