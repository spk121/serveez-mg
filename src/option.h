/*
 * option.h - getopt function interface
 *
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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
 * $Id: option.h,v 1.4 2001/04/09 13:46:05 ela Exp $
 *
 */

#ifndef __OPTION_H__
#define __OPTION_H__

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if HAVE_GETOPT_H
# include <getopt.h>
#elif HAVE_UNISTD_H
# define __EXTENSIONS__
/* FreeBSD and probably all commercial Un*ces define getopt() 
   in this specific file */
# include <unistd.h>
#endif

/*
 * The following structure contains all command line options which might
 * override the settings from the configuration file.
 */
typedef struct
{
  char *logfile;   /* logging file */
  FILE *loghandle; /* logging file handle */
  char *cfgfile;   /* configuration file */
  int verbosity;   /* verbosity level */
  int sockets;     /* maximum amount of open files (sockets) */
  char *pass;      /* password */
  int daemon;      /* start as daemon or not */
}
option_t;

/*
 * Defining here the struct and #define's for getopt_long() if it
 * is in libiberty.a but could not be found in getopt.h
 */
#if defined (HAVE_GETOPT_LONG) && !defined (DECLARED_GETOPT_LONG)

extern char *optarg;

struct option
{
  const char *name;
  int has_arg;
  int *flag;
  int val;
};

#define no_argument       0
#define required_argument 1
#define optional_argument 2

extern int getopt_long (int argc, 
			char * const argv[], 
			const char *optstring,
			const struct option *longopts, 
			int *longindex);

#endif /* DECLARED_GETOPT_LONG */

#ifndef HAVE_GETOPT

int getopt (int argc, char * const argv[], const char *optstring);
extern char *optarg;
extern int optind;
extern int opterr;
extern int optopt;

#endif /* not HAVE_GETOPT */

option_t *handle_options (int argc, char **argv);

#endif /* __OPTION_H__ */
