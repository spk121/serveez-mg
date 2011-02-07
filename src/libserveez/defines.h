/*
 * defines.h - useful global definitions for portability
 *
 * Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __DEFINES_H__
#define __DEFINES_H__ 1

/* Depending on the kind of build include either <config.h> (for internal
   build) or <svzconfig.h> for the external usage of the core library.  */

/* __BEGIN_DECLS should be used at the beginning of your declarations,
   so that C++ compilers don't mangle their names.  Use __END_DECLS at
   the end of C declarations.  */

#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS
# define __END_DECLS
#endif

/* SERVEEZ_API is a macro prepended to all function and data definitions
   which should be exported or imported in the resulting dynamic link
   library in the Win32 port.  */

#if defined (__SERVEEZ_IMPORT__)
# define SERVEEZ_API __declspec (dllimport) extern
#elif defined (__SERVEEZ_EXPORT__) || defined (DLL_EXPORT)
# define SERVEEZ_API __declspec (dllexport) extern
#else
# define SERVEEZ_API extern
#endif

/* Define if debug code should be suppressed.  */
#ifndef SVZ_ENABLE_DEBUG
#define NDEBUG 1
#endif


#endif /* !__DEFINES_H__ */
