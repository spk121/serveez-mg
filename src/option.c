/*
 * option.c - getopt function implementation
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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
 * $Id: option.c,v 1.12 2003/06/15 17:30:00 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif 

#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>

#include "libserveez.h"
#include "option.h"

#ifndef HAVE_GETOPT
/*
 * Lousy implementation of @code{getopt()}.
 * Only good for parsing simple short option command lines on
 * stupid systems like Win32. No error checking !
 */
char *optarg = NULL;
int optind = 1;
int opterr = 1;
int optopt = 0;

int 
getopt (int argc, char * const argv[], const char *optstring) 
{
  static int current_arg = 1, current_opt = 0, current_idx = 1;
  int n;
  char *prog = argv[0] + strlen (argv[0]);

  /* parse the programs name */
  while (prog > argv[0] && (*prog != '/' || *prog != '\\'))
    prog--;
  if (*prog == '/' || *prog == '\\')
    prog++;

  while (current_arg < argc) 
    {
      if (argv[current_arg][0] == '-') 
        {
          if (current_opt == 0)
            current_opt = 1;
          while (argv[current_arg][current_opt] != '\0')
	    {
	      n = 0;
	      /* go through all option characters */
	      while (optstring[n]) 
		{
		  if (optstring[n] == argv[current_arg][current_opt])
		    {
		      current_opt++;
		      if (optstring[n + 1] == ':') 
			{
			  optarg = argv[current_arg + current_idx];
			  current_idx++;
			  if (opterr && optarg == NULL)
			    fprintf (stderr, 
				     "%s: option requires an argument -- %c\n",
				     prog, optstring[n]);
			}
		      else
			optarg = NULL;
		      if (argv[current_arg][current_opt] == '\0')
			{ 
			  current_arg += current_idx;
			  current_opt = 0;
			  current_idx = 1;
			} 
		      optind = current_arg + current_idx - 1;
		      return optstring[n];
		    }
		  n++;
		}
	      optopt = argv[current_arg][current_opt];
	      if (opterr)
		fprintf (stderr, "%s: invalid option -- %c\n", prog, optopt);
	      return '?';
            }
	  current_opt++;
	}
      current_arg++;
      current_idx = 1;
    }

  current_arg = 1;
  current_opt = 0;
  current_idx = 1;
  return EOF;
}
#endif /* not HAVE_GETOPT */

/*
 * Print program version.
 */
static void 
version (void)
{
  fprintf (stdout, "%s %s\n", svz_library, svz_version);
}

/*
 * Display program command line options.
 */
static void 
usage (void)
{
  fprintf (stdout, "Usage: serveez [OPTION]...\n\n"
#if HAVE_GETOPT_LONG
 "  -h, --help               display this help and exit\n"
 "  -V, --version            display version information and exit\n"
 "  -i, --iflist             list local network interfaces and exit\n"
 "  -f, --cfg-file=FILENAME  file to use as configuration file (serveez.cfg)\n"
 "  -v, --verbose=LEVEL      set level of verbosity\n"
 "  -l, --log-file=FILENAME  use FILENAME for logging (default is stderr)\n"
 "  -P, --password=STRING    set the password for control connections\n"
 "  -m, --max-sockets=COUNT  set the max. number of socket descriptors\n"
 "  -d, --daemon             start as daemon in background\n"
 "  -c, --stdin              use standard input as configuration file\n"
#else /* not HAVE_GETOPT_LONG */
 "  -h           display this help and exit\n"
 "  -V           display version information and exit\n"
 "  -i           list local network interfaces and exit\n"
 "  -f FILENAME  file to use as configuration file (serveez.cfg)\n"
 "  -v LEVEL     set level of verbosity\n"
 "  -l FILENAME  use FILENAME for logging (default is stderr)\n"
 "  -P STRING    set the password for control connections\n"
 "  -m COUNT     set the max. number of socket descriptors\n"
 "  -d           start as daemon in background\n"
 "  -c           use standard input as configuration file\n"
#endif /* not HAVE_GETOPT_LONG */
 "\nReport bugs to <bug-serveez@gnu.org>.\n");
}

#if HAVE_GETOPT_LONG
/*
 * Argument array for `getopt_long()' system call.
 */
static struct option serveez_options[] = {
  {"help", no_argument, NULL, 'h'},
  {"version", no_argument, NULL, 'V'},
  {"iflist", no_argument, NULL, 'i'},
  {"daemon", no_argument, NULL, 'd'},
  {"stdin", no_argument, NULL, 'c'},
  {"verbose", required_argument, NULL, 'v'},
  {"cfg-file", required_argument, NULL, 'f'},
  {"log-file", required_argument, NULL, 'l'},
  {"password", required_argument, NULL, 'P'},
  {"max-sockets", required_argument, NULL, 'm'},
  {NULL, 0, NULL, 0}
};
#endif /* HAVE_GETOPT_LONG */

#define SERVEEZ_OPTIONS "l:hViv:f:P:m:dc"

/*
 * Parse the command line options. If these have been correct the function
 * either terminates the program with exit code 0 or returns an option
 * structure containing information about the command line arguments or it
 * leaves the program with exit code 1 if the command line has been wrong.
 */
option_t *
handle_options (int argc, char **argv)
{
  static option_t options;
  static char *cfgfile = "serveez.cfg";
  int arg;
#if HAVE_GETOPT_LONG
  int index;
#endif

  /* initialize command line options */
  options.logfile = NULL;
  options.cfgfile = cfgfile;
  options.verbosity = -1;
  options.sockets = -1;
  options.pass = NULL;
  options.daemon = 0;
  options.loghandle = NULL;

  /* go through the command line itself */
#if HAVE_GETOPT_LONG
  while ((arg = getopt_long (argc, argv, SERVEEZ_OPTIONS, serveez_options,
			     &index)) != EOF)
#else
  while ((arg = getopt (argc, argv, SERVEEZ_OPTIONS)) != EOF)
#endif
    {
      switch (arg)
	{
	case 'h':
	  usage ();
	  exit (0);
	  break;

	case 'V':
	  version ();
	  exit (0);
	  break;

	case 'i':
	  svz_interface_list ();
	  exit (0);
	  break;

	case 'c':
	  if (options.cfgfile != cfgfile)
	    {
	      usage ();
	      exit (1);
	    }
	  options.cfgfile = NULL;
	  break;

	case 'f':
	  if (!optarg || options.cfgfile == NULL)
	    {
	      usage ();
	      exit (1);
	    }
	  options.cfgfile = optarg;
	  break;

	case 'v':
	  if (optarg)
	    {
	      options.verbosity = atoi (optarg);
	      if (options.verbosity < LOG_FATAL)
		options.verbosity = LOG_FATAL;
	      else if (options.verbosity > LOG_DEBUG)
		options.verbosity = LOG_DEBUG;
	    }
	  else
	    options.verbosity = LOG_DEBUG;
	  break;

	case 'l':
	  if (!optarg)
	    {
	      usage ();
	      exit (1);
	    }
	  options.logfile = optarg;
	  break;

	case 'P':
	  if (!optarg || strlen (optarg) < 2)
	    {
	      usage ();
	      exit (1);
	    }
#if SVZ_ENABLE_CRYPT
	  options.pass = svz_pstrdup (crypt (optarg, optarg));
#else
	  options.pass = svz_pstrdup (optarg);
#endif
	  break;

	case 'm':
	  if (!optarg)
	    {
	      usage ();
	      exit (1);
	    }
	  options.sockets = atoi (optarg);
	  break;

	case 'd':
	  options.daemon = 1;
	  break;

	default:
	  usage ();
	  exit (1);
	}
    }

  return &options;
}
