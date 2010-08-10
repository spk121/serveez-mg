/*
 * snprintf.h - (v)snprintf function interface
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * $Id: snprintf.h,v 1.8 2003/06/15 17:30:00 ela Exp $
 *
 */

#ifndef __SNPRINTF_H__
#define __SNPRINTF_H__ 1

#include "libserveez/defines.h"

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

/* to be sure not to redefine `va_start' in <stdarg.h> */
#if defined (SVZ_HAVE_VARARGS_H) && !defined (va_start)
# include <varargs.h>
#endif

#ifdef __MINGW32__
/*
 * Both of these functions are actually implemented but not within the
 * B20.1 release of Cygwin, but in the latest. So we define them here
 * ourselves.
 */
# if !defined (SVZ_HAVE_SNPRINTF)
int _snprintf (char *, unsigned int, const char *, ...);
# endif
# if !defined (SVZ_HAVE_VSNPRINTF)
int _vsnprintf (char *, unsigned int, const char *, va_list);
# endif
# define svz_vsnprintf _vsnprintf
# define svz_snprintf _snprintf

#else /* __MINGW32__ */

#ifndef SVZ_HAVE_VSNPRINTF
# define svz_vsnprintf(str, n, format, ap) vsprintf (str, format, ap)
#else
# define svz_vsnprintf(str, n, format, ap) vsnprintf (str, n, format, ap)
#endif

#endif

__BEGIN_DECLS

#ifndef SVZ_HAVE_SNPRINTF
SERVEEZ_API int svz_snprintf __PARAMS ((char *, unsigned int,
					const char *, ...));
#elif !defined (svz_snprintf)
# define svz_snprintf snprintf
#endif

__END_DECLS

#endif /* not __SNPRINTF_H__ */
