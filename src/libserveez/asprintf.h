/*
 * asprintf.h - (v)asprintf function interface
 *
 * Copyright (C) 2002 Andreas Rottmann <a.rottmann@gmx.at>
 * Copyright (C) 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: asprintf.h,v 1.3 2003/06/14 14:57:59 ela Exp $
 *
 */

#ifndef __ASPRINTF_H__
#define __ASPRINTF_H__ 1

#include "libserveez/defines.h"

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

/* to be sure not to redefine `va_start' in <stdarg.h> */
#if defined (SVZ_HAVE_VARARGS_H) && !defined (va_start)
# include <varargs.h>
#endif

__BEGIN_DECLS

SERVEEZ_API int svz_asprintf __PARAMS ((char **, const char *, ...));
SERVEEZ_API int svz_vasprintf __PARAMS ((char **, const char *,
					 va_list));

__END_DECLS

#endif /* not __ASPRINTF_H__ */
