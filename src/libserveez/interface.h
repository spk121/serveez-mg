/*
 * interfaces.h - network interface definitions
 *
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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

#ifndef __INTERFACE_H__
#define __INTERFACE_H__ 1

#include "defines.h"

/*
 * Structure for collecting IP interfaces.
 */
typedef struct svz_interface
{
  unsigned long index;  /* interface index */
  char *description;    /* interface description */
  unsigned long ipaddr; /* its IP address */
  int detected;         /* interface flag */
}
svz_interface_t;

__BEGIN_DECLS

SERVEEZ_API svz_vector_t *svz_interfaces;

/* Export these functions. */
SERVEEZ_API void svz_interface_list (void);
SERVEEZ_API void svz_interface_collect (void);
SERVEEZ_API int svz_interface_free (void);
SERVEEZ_API int svz_interface_add (int, char *, unsigned long, int);
SERVEEZ_API svz_interface_t *svz_interface_search (char *);
SERVEEZ_API svz_interface_t *svz_interface_get (unsigned long);
SERVEEZ_API void svz_interface_check (void);

__END_DECLS

/*
 * Iteration macro for the list of known network interfaces.  If any
 * each interface gets assigned to @var{ifc}.  The variable @var{i} is the
 * iteration variable.
 */
#define svz_interface_foreach(ifc, i) \
  svz_vector_foreach (svz_interfaces, (ifc), (i))

#endif /* not __INTERFACE_H__ */
