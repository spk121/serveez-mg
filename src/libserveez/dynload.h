/*
 * dynload.h - dynamic server loading interface
 *
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __DYNLOAD_H__
#define __DYNLOAD_H__ 1

#include "defines.h"

/* Structure holding a shared libraries info. */
typedef struct
{
  void *handle; /* its handle */
  char *file;   /* the shared libraries filename */
  int ref;      /* reference counter */
}
dyn_library_t;

__BEGIN_DECLS

SERVEEZ_API void svz_dynload_init (void);
SERVEEZ_API void svz_dynload_finalize (void);
SERVEEZ_API svz_servertype_t *svz_servertype_load (char *);
SERVEEZ_API int svz_servertype_unload (char *);
SERVEEZ_API void svz_dynload_path_set (svz_array_t *);
SERVEEZ_API svz_array_t *svz_dynload_path_get (void);

__END_DECLS

#endif /* not __DYNLOAD_H__ */
