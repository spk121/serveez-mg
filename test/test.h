/*
 * test/test.h - test suite utility function definitions
 *
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: test.h,v 1.2 2000/10/22 19:11:03 ela Exp $
 *
 */

#ifndef __TEST_H__
#define __TEST_H__

#if HAVE_CONFIG_H
# include <config.h>
#endif

void test_init (void);
void test_print (char *text);
void test_ok (void);
void test_failed (void);
char * test_string (void);
unsigned long test_value (unsigned long nr);

#endif /* __TEST_H__ */
