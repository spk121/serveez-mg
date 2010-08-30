## Process this file with automake to produce Makefile.in
# 
# test/Makefile.am
#
# Automake input file for the test directory.
#
# Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
# Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this package; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.  
#

TESTS = vector-test array-test hash-test spvec-test test-http-get

EXTRA_DIST = prog-test.cfg test-suite.scm binary-test.scm server-test.scm \
	guile-server-test.scm

noinst_PROGRAMS = hash-test spvec-test array-test vector-test codec-test \
	prog-test

hash_test_SOURCES = hash-test.c test.c test.h
hash_test_LDADD = -L$(top_builddir)/src/libserveez -lserveez
spvec_test_SOURCES = spvec-test.c test.c test.h
spvec_test_LDADD = -L$(top_builddir)/src/libserveez -lserveez
array_test_SOURCES = array-test.c test.c test.h
array_test_LDADD = -L$(top_builddir)/src/libserveez -lserveez
vector_test_SOURCES = vector-test.c test.c test.h
vector_test_LDADD = -L$(top_builddir)/src/libserveez -lserveez
codec_test_SOURCES = codec-test.c
codec_test_LDADD = -L$(top_builddir)/src/libserveez -lserveez
prog_test_SOURCES = prog-test.c

AM_CPPFLAGS = $(SERVEEZ_CFLAGS) -I$(top_srcdir)/src \
	-I$(top_srcdir)/src/libserveez
