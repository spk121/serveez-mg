/*
 * mkpassword.c - simple password generation program
 *
 * Copyright (C) 2000, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: mkpassword.c,v 1.6 2003/06/15 17:30:00 ela Exp $
 *
 */

#include <config.h>
#include <stdio.h>              /* fprintf */
#include <stdlib.h>             /* rand, srand */
#include <string.h>             /* strchr  */
#include <time.h>               /* time */
#include <crypt.h>              /* getpass? crypt? */
#include "getpass.h"

/*
 * Main entry point.
 */
int 
main (int argc, char **argv)
{
  static char saltChars[] = 
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYT"
    "0123456789./";

  char salt[3];
  char *plaintext;

  if (argc < 2) 
    {
      srand (time (NULL));
      salt[0] = saltChars[rand () % strlen (saltChars)];
      salt[1] = saltChars[rand () % strlen (saltChars)];
      salt[2] = 0;
    }
  else 
    {
      salt[0] = argv[1][0];
      salt[1] = argv[1][1];
      salt[2] = '\0';
      if ((strchr (saltChars, salt[0]) == NULL) || 
	  (strchr (saltChars, salt[1]) == NULL))
	{
	  fprintf (stderr, "illegal salt %s\n", salt);
	  exit (1);
	}
    }

#if SVZ_HAVE_CRYPT
  plaintext = getpass ("Password: ");
  fprintf (stdout, "%s\n", crypt (plaintext, salt));
#else
  fprintf (stderr, "please --enable-crypt\n");
#endif
  return 0;
}
