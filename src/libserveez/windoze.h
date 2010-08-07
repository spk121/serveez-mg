/*
 * windoze.h - windows port interface
 *
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: windoze.h,v 1.5 2001/12/13 18:00:01 ela Exp $
 *
 */

#ifndef __WINDOZE_H__
#define __WINDOZE_H__ 1

#include "libserveez/defines.h"

#ifdef __MINGW32__

/* definitions for Win95..WinME */
#define MaxSocketKey       HKEY_LOCAL_MACHINE
#define MaxSocketSubKey    "System\\CurrentControlSet\\Services\\VxD\\MSTCP"
#define MaxSocketSubSubKey "MaxConnections"

/* window definitions */
#define WM_SERVEEZ_NOTIFYICON (WM_APP + 100)
#define SERVEEZ_ICON_ID       (1001)
#define SERVEEZ_CLASS         "serveez"

__BEGIN_DECLS

/* exported functions */
SERVEEZ_API int svz_windoze_start_daemon __PARAMS ((char *));
SERVEEZ_API int svz_windoze_stop_daemon __PARAMS ((void));
SERVEEZ_API WCHAR *svz_windoze_asc2uni __PARAMS ((CHAR *asc));
SERVEEZ_API CHAR *svz_windoze_uni2asc __PARAMS ((WCHAR *unicode));

/* registry functions */
SERVEEZ_API unsigned svz_windoze_get_reg_unsigned __PARAMS ((HKEY, char *, 
							     char *, 
							     unsigned));
SERVEEZ_API void svz_windoze_set_reg_unsigned __PARAMS ((HKEY, char *, 
							 char *, 
							 unsigned));
SERVEEZ_API char *svz_windoze_get_reg_string __PARAMS ((HKEY, char *, 
							char *, char *));
SERVEEZ_API void svz_windoze_set_reg_string __PARAMS ((HKEY, char *, 
						       char *, char *));

__END_DECLS

#endif /* not __MINGW32__ */

#endif /* not __WINDOZE_H__ */
