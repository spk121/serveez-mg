/*
 * ident.h - ident coserver header file
 *
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __IDENT_H__
#define __IDENT_H__ 1

#include "defines.h"

__BEGIN_DECLS

/*
 * Opens a connection to ident server and receives / parses the response.
 */
char *ident_handle_request (char *);

__END_DECLS

#endif /* not __IDENT_H__ */
