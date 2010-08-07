/*
 * irc-crypt.c - IRC crypt routines
 *
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: irc-crypt.c,v 1.8 2001/08/01 10:16:22 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_IRC_PROTO

#define _GNU_SOURCE
#include <string.h>

#ifdef __MINGW32__
# include <winsock2.h>
#endif

#include "libserveez.h"
#include "irc-proto.h"
#include "irc-crypt.h"

/*
 * Generate a key for the de- and encryption routine.
 */
svz_uint8_t
irc_gen_key (char *pass)
{
  svz_uint8_t *p;
  int n;
  svz_uint8_t key;

  key = 0;
  n = 0;
  p = (svz_uint8_t *) pass;
  while (*p)
    {
      key += (svz_uint8_t) ((*p + n) ^ IRC_CRYPT_BYTE);
      n++;
      p++;
    }
  return key;
}

/*
 * Encrypt a string by a given key.
 */
void
irc_encrypt_text (char *text, svz_uint8_t key)
{
  char crypt[MAX_MSG_LEN];
  char *t, *c;
  svz_uint8_t code;

  memset (crypt, 0, MAX_MSG_LEN);
  t = text;
  c = crypt;

  while (*t)
    {
      code = (svz_uint8_t) (*t ^ key);
      if (code < (svz_uint8_t) 0x20 || code == IRC_CRYPT_PREFIX)
	{
	  *c++ = IRC_CRYPT_PREFIX;
	  *c++ = (char) (code + IRC_CRYPT_PREFIX);
	}
      else
	{
	  *c++ = code;
	}
      t++;
    }
  strcpy (text, crypt);
}

/*
 * Decrypt a string by a given key.
 */
char *
irc_decrypt_text (char *crypt, svz_uint8_t key)
{
  static char text[MAX_MSG_LEN];
  char *t, *c;

  memset (text, 0, MAX_MSG_LEN);
  t = text;
  c = crypt;

  while (*c)
    {
      if (*c == IRC_CRYPT_PREFIX)
	{
	  c++;
	  *t++ = (char) ((*c - IRC_CRYPT_PREFIX) ^ key);
	}
      else
	{
	  *t++ = (char) (*c ^ key);
	}
      c++;
    }
  return text;
}

#else /* ENABLE_IRC_PROTO */

int irc_crypt_dummy; /* Shut up compiler. */

#endif /* ENABLE_IRC_PROTO */
