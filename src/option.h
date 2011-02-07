/*
 * option.h - getopt function interface
 *
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
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

#ifndef __OPTION_H__
#define __OPTION_H__

#include <config.h>

#include <stdio.h>
#include <getopt.h>

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

option_t *handle_options (int argc, char **argv);

#endif /* __OPTION_H__ */
