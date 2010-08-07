/*
 * guile-bin.h - binary data exchange layer for guile servers
 *
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: guile-bin.h,v 1.5 2002/05/24 12:51:13 ela Exp $
 *
 */

#ifndef __GUILE_BIN_H__
#define __GUILE_BIN_H__ 1

void guile_bin_init (void);
int guile_bin_check (SCM);
SCM guile_garbage_to_bin (void *, int);
SCM guile_data_to_bin (void *, int);
void *guile_bin_to_data (SCM, int *);

#endif /* not __GUILE_BIN_H__ */
