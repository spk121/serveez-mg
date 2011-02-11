/*
 * boot.c - configuration and boot functions
 *
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

#include <pthread.h> 		/* pthread_mutex_t, pthread_mutex_init,
				 pthread_mutex_destroy */
#include <stdio.h>
#include <sys/types.h>
#include <time.h>


#include "alloc.h"
#include "util.h"
#include "vector.h"
#include "interface.h"
#include "socket.h"
#include "icmp-socket.h"
#include "pipe-socket.h"
#include "server.h"
#include "dynload.h"
#include "boot.h"
#include "mutex.h"
#include "server-core.h"
#include "codec/codec.h"

/*
 * The configuration structure of the core library.
 */
svz_config_t svz_config = { NULL, 0, 0, 0 };

/* The symbolic name of the core library. */
char *svz_library = PACKAGE;
/* The version of the core library. */
char *svz_version = VERSION;

/* Runtime flag if this is Win32 or not.  */
#if defined (__MINGW32__) || defined (__CYGWIN__)
int svz_have_Win32 = 1;
#else
int svz_have_Win32 = 0;
#endif

/* Runtime flag if this is the debug version or not.  */
int svz_have_debug = 1;

/* Runtime checkable flags for configuration language and code if flood
   protection has been enabled or not.  */
int svz_have_floodprotect = 1;

/* Extern declaration of the logging mutex.  */
extern pthread_mutex_t svz_log_mutex;

/*
 * Initialization of the configuration.
 */
void
svz_init_config (void)
{
  svz_config.start = time (NULL);
  svz_config.verbosity = LOG_DEBUG;
  svz_config.max_sockets = 100;
  svz_config.password = NULL;
}

/*
 * Initialization of the core library.
 */
void
svz_boot (void)
{
  pthread_mutex_init (&svz_log_mutex, NULL);
  svz_strsignal_init ();
  svz_sock_table_create ();
  svz_signal_up ();
  svz_init_config ();
  svz_interface_collect ();
  svz_dynload_init ();
  svz_codec_init ();
  svz_config_type_init ();
}

/*
 * Finalization of the core library.
 */
void
svz_halt (void)
{
  svz_free_and_zero (svz_config.password);
  svz_portcfg_finalize ();
  svz_config_type_finalize ();
  svz_codec_finalize ();
  svz_dynload_finalize ();
  svz_interface_free ();
  svz_signal_dn ();
  svz_sock_table_destroy ();
  svz_strsignal_destroy ();
  pthread_mutex_destroy (&svz_log_mutex);
}
