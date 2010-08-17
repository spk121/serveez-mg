## Process this file with automake to produce Makefile.in
#
# src/libserveez/codec/Makefile.am
#
# Automake input file for the encoder/decoder interface.
#
# Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
# Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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

noinst_LTLIBRARIES = libcodec.la

AM_CPPFLAGS = -D__BUILD_SVZ_LIBRARY__ -I$(top_srcdir)/src

libcodec_la_LDFLAGS = @NOUNDEFINED@
libcodec_la_SOURCES = codec.c gzlib.c
libcodec_la_LIBADD = $(CODECLIBS)

libcodecdir = $(includedir)/libserveez/codec

libcodec_HEADERS = codec.h

noinst_HEADERS = gzlib.h

CLEANFILES = *~
MAINTAINERCLEANFILES = Makefile.in
