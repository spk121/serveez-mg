## Process this file with automake to produce Makefile.in
#
# src/fakeident-server/Makefile.am
#
# Automake input file for a fake ident server (identd).
#
# Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
# Copyright (C) 2001 Raimund Jacob <raimi@lkcc.org>
# Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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

noinst_LIBRARIES = libfakeident.a

libfakeident_a_SOURCES = \
        ident-proto.h ident-proto.c

AM_CPPFLAGS = $(SERVEEZ_CFLAGS) -I$(top_srcdir)/src
CLEANFILES = *~
MAINTAINERCLEANFILES = Makefile.in
