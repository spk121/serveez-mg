/*
 * guile-api.h - compatibility and miscellaneous guile functionality
 *
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: guile-api.h,v 1.15 2003/04/06 20:01:24 ela Exp $
 *
 */

#ifndef __GUILE_API_H__
#define __GUILE_API_H__ 1

#include <libguile.h>

SCM guile_lookup (const char *name);
void guile_api_init (void);
void guile_api_finalize (void);

#endif /* not __GUILE_API_H__ */
