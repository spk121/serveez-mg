/*
 * server-loop.h - server loop definition
 *
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: server-loop.h,v 1.5 2001/12/13 18:00:00 ela Exp $
 *
 */

#ifndef __SERVER_LOOP_H__
#define __SERVER_LOOP_H__ 1

#include "defines.h"

__BEGIN_DECLS

SERVEEZ_API int svz_check_sockets (void);

__END_DECLS

#endif /* not __SERVER_LOOP_H__ */
