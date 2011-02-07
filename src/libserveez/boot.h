/*
 * boot.h - configuration and boot declarations
 *
 * Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __BOOT_H__
#define __BOOT_H__ 1

#include "defines.h"
#include "socket.h"

/*
 * General serveez configuration structure.
 */
typedef struct
{
  /* program's password */
  char *password;
  /* defines how many clients are allowed to connect */
  svz_t_socket max_sockets;
  /* when was the program started */
  long start;
  /* log level verbosity */
  int verbosity;
}  
svz_config_t;

__BEGIN_DECLS

/* Core library configuration.  */
SERVEEZ_API svz_config_t svz_config;

/* Exported functions.  */
SERVEEZ_API void svz_init_config (void);
SERVEEZ_API void svz_boot (void);
SERVEEZ_API void svz_halt (void);

/* Some static strings.  */
SERVEEZ_API char *svz_library;
SERVEEZ_API char *svz_version;
SERVEEZ_API char *svz_build;

/* Exported from `boot.c' because it is a central point.  */
SERVEEZ_API int svz_have_debug;
SERVEEZ_API int svz_have_Win32;
SERVEEZ_API int svz_have_floodprotect;

__END_DECLS

#endif /* not __BOOT_H__ */
