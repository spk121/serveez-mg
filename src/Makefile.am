## Process this file with automake to produce Makefile.in
# 
# src/Makefile.am
#
# Automake input file.
#
# Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
# Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
# Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
# Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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


pkgdata_DATA = serveez.scm serveez.cfg

#
# Add your server subdirectory here
#
SUBDIRS = foo-server guile-server $(SERVERDIRS)

#
# Add your library here
#

bin_PROGRAMS = serveez

serveez_SOURCES = \
	serveez.c serveez.h \
	guile.c guile.h \
	cfgfile.c cfgfile.h \
	option.c option.h \
	guile-api.c guile-api.h \
	guile-server.c guile-server.h

LDADD = ../libserveez/libserveez.a $(GUILE_LIBS) -lz 

AM_CPPFLAGS = $(GUILE_CFLAGS)

# mkpassword_SOURCES = mkpassword.c getpass.c getpass.h
# mkpassword_LDADD = -lcrypt

