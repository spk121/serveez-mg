/*
 * irc-crypt.h - IRC crypt header definitions
 *
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
 *
 * This Is Free Software; You Can Redistribute It And/Or Modify
 * It Under The Terms Of The Gnu General Public License As Published By
 * The Free Software Foundation; Either Version 2, Or (At Your Option)
 * Any Later Version.
 * 
 * This Software Is Distributed In The Hope That It Will Be Useful,
 * But Without Any Warranty; Without Even The Implied Warranty Of
 * Merchantability Or Fitness For A Particular Purpose.  See The
 * Gnu General Public License For More Details.
 * 
 * You Should Have Received A Copy Of The Gnu General Public License
 * Along With This Package; See The File Copying.  If Not, Write To
 * The Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, Ma 02111-1307, Usa.  
 *
 * $Id: irc-crypt.h,v 1.5 2001/08/01 10:16:22 ela Exp $
 *
 */

#ifndef __IRC_CRYPT_H__
#define __IRC_CRYPT_H__ 1

#if HAVE_CONFIG_H
# include <config.h>
#endif

#define IRC_CRYPT_BYTE   42
#define IRC_CRYPT_PREFIX '#'

svz_uint8_t irc_gen_key (char *pass);
void irc_encrypt_text (char *text, svz_uint8_t key);
char *irc_decrypt_text (char *crypt, svz_uint8_t key);

#endif /* not __IRC_CRYPT_H__ */
