/*
 * util.c - utility function implementation
 *
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
 * Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
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
 * $Id: util.c,v 1.26 2003/06/18 03:32:49 ela Exp $
 *
 */

#include <stdio.h>       /* sprintf, fflush, vfprintf, fprintf, ferror, feof */
#include <string.h>             /* strerror */
#include <ctype.h>              /* isupper, tolower */
#include <time.h>               /* ctime, time, localtime, struct tm */
#include <errno.h>              /* errno */
#include <sys/utsname.h>        /* uname */
#include <stdarg.h>             /* va_start */
#include <sys/types.h>          /* time_t */
#include <unistd.h>             /* sysconf, getcwd */
#include <sys/resource.h>       /* RLIMIT_NOFILE, struct rlimit */

#include "alloc.h"
#include "boot.h"
#include "mutex.h"
#include "util.h"

/* 
 * Level of the logging interfaces verbosity:
 * 0 - only fatal error messages
 * 1 - error messages
 * 2 - warnings
 * 3 - informational messages
 * 4 - debugging output
 * Levels always imply numerically lesser levels.
 */

static char log_level[][16] = {
  "fatal",
  "error",
  "warning",
  "notice",
  "debug"
};

/*
 * This is the file all log messages are written to. Change it with a
 * call to @code{svz_log_setfile()}. By default, all log messages are written
 * to @code{stderr}.
 */
static FILE *svz_logfile = NULL;

/* Global definition of the logging mutex. */
svz_mutex_define (svz_log_mutex)

/*
 * Print a message to the log system. @var{level} specifies the prefix.
 */
void
svz_log (int level, const char *format, ...)
{
  va_list args;
  time_t tm;
  struct tm *t;

  if (level > svz_config.verbosity || svz_logfile == NULL ||
      feof (svz_logfile) || ferror (svz_logfile))
    return;

  svz_mutex_lock (&svz_log_mutex);
  tm = time (NULL);
  t = localtime (&tm);
  fprintf (svz_logfile, "[%4d/%02d/%02d %02d:%02d:%02d] %s: ",
	   t->tm_year + 1900, t->tm_mon + 1, t->tm_mday,
	   t->tm_hour, t->tm_min, t->tm_sec, log_level[level]);
  va_start (args, format);
  vfprintf (svz_logfile, format, args);
  va_end (args);
  fflush (svz_logfile);
  svz_mutex_unlock (&svz_log_mutex);
}

/*
 * Set the file stream @var{file} to the log file all messages are printed
 * to. Could also be @code{stdout} or @code{stderr}.
 */
void
svz_log_setfile (FILE * file)
{
  svz_logfile = file;
}

#define MAX_DUMP_LINE 16   /* bytes per line */

/*
 * Dump a @var{buffer} with the length @var{len} to the file stream @var{out}.
 * You can specify a description in @var{action}. The hexadecimal text 
 * representation of the given buffer will be either cut at @var{len} or 
 * @var{max}. @var{from} is a numerical identifier of the buffers creator.
 */
int
svz_hexdump (FILE *out,    /* output FILE stream */
	     char *action, /* hex dump description */
	     int from,	   /* who created the dumped data */
	     char *buffer, /* the buffer to dump */
	     int len,	   /* length of that buffer */
	     int max)	   /* maximum amount of bytes to dump (0 = all) */
{
  int row, col, x, max_col;

  if (!max)
    max = len;
  if (max > len)
    max = len;
  max_col = max / MAX_DUMP_LINE;
  if ((max % MAX_DUMP_LINE) != 0)
    max_col++;

  fprintf (out, "%s [ FROM:0x%08X SIZE:%d ]\n", action, (unsigned) from, len);

  for (x = row = 0; row < max_col && x < max; row++)
    {
      /* print hexdump */
      fprintf (out, "%04X   ", x);
      for (col = 0; col < MAX_DUMP_LINE; col++, x++)
	{
	  if (x < max)
	    fprintf (out, "%02X ", (unsigned char) buffer[x]);
	  else
	    fprintf (out, "   ");
	}
      /* print character representation */
      x -= MAX_DUMP_LINE;
      fprintf (out, "  ");
      for (col = 0; col < MAX_DUMP_LINE && x < max; col++, x++)
	{
	  fprintf (out, "%c", buffer[x] >= ' ' ? buffer[x] : '.');
	}
      fprintf (out, "\n");
    }

  fflush (out);
  return 0;
}

/*
 * Transform the given binary data @var{t} (UTC time) to an ASCII time text
 * representation without any trailing characters.
 */
char *
svz_time (long t)
{
  static char *asc;
  char *p;

  p = asc = ctime ((time_t *) &t);
  while (*p)
    p++;
  while (*p < ' ')
    *(p--) = '\0';

  return asc;
}

/*
 * Create some kind of uptime string. It tells how long the core library
 * has been running. 
 */
char *
svz_uptime (long diff)
{
  static char text[64];
  long sec, min, hour, day, old;

  old = diff;
  sec = diff % 60;
  diff /= 60;
  min = diff % 60;
  diff /= 60;
  hour = diff % 24;
  diff /= 24;
  day = diff;

  if (old < 60)
    {
      sprintf (text, "%ld sec", sec);
    }
  else if (old < 60 * 60)
    {
      sprintf (text, "%ld min", min);
    }
  else if (old < 60 * 60 * 24)
    {
      sprintf (text, "%ld hours, %ld min", hour, min);
    }
  else
    {
      sprintf (text, "%ld days, %ld:%02ld", day, hour, min);
    }

  return text;
}

/*
 * Convert the given string @var{str} to lower case text representation.
 */
char *
svz_tolower (char *str)
{
  char *p = str;

  while (*p)
    {
      *p = (char) (isupper ((svz_uint8_t) * p) ? 
		   tolower ((svz_uint8_t) * p) : *p);
      p++;
    }
  return str;
}

/*
 * This routine is for detecting the operating system version of Win32 
 * and all Unices at runtime. You should call it at least once at startup.
 * It saves its result in the variable @code{svz_os_version} and prints an
 * appropriate message.
 */
char *
svz_sys_version (void)
{
  static char os[256] = ""; /* contains the os string */

  struct utsname buf;

  /* detect only once */
  if (os[0])
    return os;

  uname (&buf);
  sprintf (os, "%s %s on %s", buf.sysname, buf.release, buf.machine);
  return os;
}

/*
 * Converts an unsigned integer to its decimal string representation 
 * returning a pointer to an internal buffer, so copy the result.
 */
char *
svz_itoa (unsigned int i)
{
  static char buffer[32];
  char *p = buffer + sizeof (buffer) - 1;

  *p = '\0';
  do
    {
      p--;
      *p = (char) ((i % 10) + '0');
    }
  while ((i /= 10) != 0);
  return p;
}

/*
 * Converts a given string @var{str} in decimal format to an unsigned integer.
 * Stops conversion on any invalid characters.
 */
unsigned int
svz_atoi (char *str)
{
  unsigned int i = 0;

  while (*str >= '0' && *str <= '9')
    {
      i *= 10;
      i += *str - '0';
      str++;
    }
  return i;
}

/*
 * Returns the current working directory. The returned pointer needs to be
 * @code{svz_free()}'ed after usage.
 */
char *
svz_getcwd (void)
{
  char *buf, *dir;
  int len = 64;

  buf = dir = NULL;
  do
    {
      buf = svz_realloc (buf, len);
      dir = getcwd (buf, len);
      len *= 2;
    }
  while (dir == NULL);

  return dir;
}

/*
 * This routine checks for the current and maximum limit of open files
 * of the current process. The function heavily depends on the underlying
 * platform. It tries to set the limit to the given @var{max_sockets} 
 * amount.
 */
int
svz_openfiles (int max_sockets)
{
  struct rlimit rlim;

  long openfiles;

  if ((openfiles = sysconf(_SC_OPEN_MAX)) == -1)
    {
      svz_log (LOG_ERROR, "getdtablesize: %s\n", SYS_ERROR);
      return -1;
    }
  svz_log (LOG_NOTICE, "file descriptor table size: %d\n", openfiles);

  if (getrlimit (RLIMIT_NOFILE, &rlim) == -1)
    {
      svz_log (LOG_ERROR, "getrlimit: %s\n", SYS_ERROR);
      return -1;
    }
  svz_log (LOG_NOTICE, "current open file limit: %d/%d\n",
	   rlim.rlim_cur, rlim.rlim_max);

  if ((int) rlim.rlim_max < (int) max_sockets || 
      (int) rlim.rlim_cur < (int) max_sockets)
    {
      rlim.rlim_max = max_sockets;
      rlim.rlim_cur = max_sockets;

      if (setrlimit (RLIMIT_NOFILE, &rlim) == -1)
	{
	  svz_log (LOG_ERROR, "setrlimit: %s\n", SYS_ERROR);
	  return -1;
	}
      getrlimit (RLIMIT_NOFILE, &rlim);
      svz_log (LOG_NOTICE, "open file limit set to: %d/%d\n",
	       rlim.rlim_cur, rlim.rlim_max);
    }

  return 0;
}
