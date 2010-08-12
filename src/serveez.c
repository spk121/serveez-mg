/*
 * serveez.c - main module
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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
 * $Id: serveez.c,v 1.52 2003/06/14 14:57:59 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <libguile.h>
#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef __MINGW32__
# include <winsock2.h>
#endif

#include "libserveez.h"
#include "serveez.h"
#include "cfgfile.h"
#include "option.h"
#include "guile-api.h"
#include "guile.h"
#include "guile-server.h"

/* Command line option structure. */
option_t *options = NULL;

/* Our private launch pad. */
void
guile_launch_pad (void *closure, int argc, char **argv)
{
  void (* entry) (int, char **) = (void (*) (int, char **)) closure;
  entry (argc, argv);
  exit (0);
}

/*
 * This is the entry point for the guile interface.
 */
static void
guile_entry (int argc, char **argv)
{
  /* Detect operating system. */
  svz_log (LOG_NOTICE, "%s\n", svz_sys_version ());

  /* Start loading the configuration file. */
  if (guile_load_config (options->cfgfile) == -1)
    {
      svz_log (LOG_ERROR, "error loading config file\n");
      exit (3);
    }

  /*
   * Make command line arguments overriding the configuration
   * file settings.
   */
  if (options->verbosity != -1)
    svz_config.verbosity = options->verbosity;

  if (options->sockets != -1)
    svz_config.max_sockets = options->sockets;

  if (options->pass)
    {
      svz_free (svz_config.password);
      svz_config.password = svz_strdup (options->pass);
    }

#if SVZ_ENABLE_DEBUG
  svz_log (LOG_NOTICE, "serveez starting, debugging enabled\n");
#endif /* SVZ_ENABLE_DEBUG */

  svz_openfiles (svz_config.max_sockets);
  svz_log (LOG_NOTICE, "using %d socket descriptors\n",
	   svz_config.max_sockets);

  /* Startup the internal coservers here. */
  if (svz_coserver_init () == -1)
    {
      exit (4);
    }

  /* Initialize server instances. */
  if (svz_server_init_all () == -1)
    {
      exit (6);
    }

  svz_loop ();

  /* Run the finalizers. */
  svz_server_finalize_all ();
  svz_servertype_finalize ();

  /* Disconnect the previously invoked internal coservers. */
  svz_log (LOG_NOTICE, "destroying internal coservers\n");
  svz_coserver_finalize ();

#if ENABLE_GUILE_SERVER
  guile_server_finalize ();
#endif /* ENABLE_GUILE_SERVER */

  svz_halt ();

#if DEBUG_MEMORY_LEAKS
  CHECK_LEAKS ();
#endif /* DEBUG_MEMORY_LEAKS */

#ifdef __MINGW32__
  if (options->daemon)
    {
      svz_windoze_stop_daemon ();
    }
#endif

  svz_log (LOG_NOTICE, "serveez terminating\n");

  /* FIXME: Serveez leaks because of a open logfile handle. */
  if (options->loghandle != stderr)
    svz_fclose (options->loghandle);
}

/*
 * Main entry point.
 */
int
main (int argc, char *argv[])
{
  /* Initialize the the core library. */
  svz_boot ();
  svz_executable (argv[0]);
  svz_envblock_setup ();

  /* Handle command line arguments. */
  options = handle_options (argc, argv);

  /* Send all logging messages to the log handle. */
  if (options->logfile && options->logfile[0])
    options->loghandle = svz_fopen (options->logfile, "w");
  if (!options->loghandle)
    options->loghandle = stderr;
  svz_log_setfile (options->loghandle);

  /* Setup verbosity once. */
  if (options->verbosity != -1)
    svz_config.verbosity = options->verbosity;

  /* Start as daemon, not as foreground application. */
  if (options->daemon)
    {
#ifndef __MINGW32__
      int pid;

      if ((pid = fork ()) == -1)
	{
	  svz_log (LOG_ERROR, "fork: %s\n", SYS_ERROR);
	  exit (1);
	}
      else if (pid != 0)
	{
	  exit (0);
	}
      /* Close the log file if necessary. */
      if (options->loghandle == stderr)
	svz_log_setfile (NULL);
      /* Close stdin, stdout and stderr. */
      if (isatty (fileno (stdin)))
	close (fileno (stdin));
      if (isatty (fileno (stdout)))
	close (fileno (stdout));
      if (isatty (fileno (stderr)))
	close (fileno (stderr));
#else /* __MINGW32__ */
      if (svz_windoze_start_daemon (argv[0]) == -1)
	exit (1);
      if (options->loghandle == stderr)
	svz_log_setfile (NULL);
      closehandle (GetStdHandle (STD_INPUT_HANDLE));
      closehandle (GetStdHandle (STD_OUTPUT_HANDLE));
      closehandle (GetStdHandle (STD_ERROR_HANDLE));
#endif /* __MINGW32__ */
    }

  /* Initialize the static server types. */
  init_server_definitions ();
#if 0
  svz_servertype_print ();
#endif

  /* Enter the main guile function. */
  scm_boot_guile (argc, argv, guile_launch_pad, (void *) guile_entry);
  /* Never reached. */
  return 0;
}
