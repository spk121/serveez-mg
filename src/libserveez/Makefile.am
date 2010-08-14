## Process this file with automake to produce Makefile.in
# 
# src/libserveez/Makefile.am
#
# Automake input file for the serveez core library.
#
# Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
# Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

EXTRA_DIST = libserveez.dsp libserveez.ap ChangeLog svzpath.h.in \
	svzpath.h.serveez
SUBDIRS = coserver codec

# the serveez core library rules

lib_LTLIBRARIES = libserveez.la

libserveez_la_LDFLAGS = -export-dynamic @NOUNDEFINED@ -release @VERSION@
libserveez_la_DEPENDENCIES = coserver/libcoserver.la codec/libcodec.la
libserveez_la_LIBADD = coserver/libcoserver.la codec/libcodec.la \
  $(LIBSERVEEZLIBS) $(LIBSOCKET)

libserveez_la_SOURCES = alloc.c util.c snprintf.c hash.c sparsevec.c      \
  tcp-socket.c pipe-socket.c udp-socket.c icmp-socket.c raw-socket.c      \
  server-core.c server-loop.c windoze.c boot.c server.c server-socket.c   \
  interface.c dynload.c vector.c core.c socket.c array.c portcfg.c        \
  binding.c passthrough.c cfg.c asprintf.c mutex.c

# these file are available by #include <libserveez/foo.h>

libserveezdir = $(includedir)/libserveez

libserveez_HEADERS = alloc.h util.h snprintf.h hash.h sparsevec.h socket.h \
  tcp-socket.h pipe-socket.h udp-socket.h icmp-socket.h raw-socket.h       \
  server-core.h server-loop.h windoze.h defines.h boot.h server.h          \
  server-socket.h interface.h dynload.h vector.h core.h array.h portcfg.h  \
  binding.h passthrough.h cfg.h asprintf.h mutex.h

# generate path info (included in <libserveez/dynload.c>

SED = sed

$(srcdir)/dynload.c: svzpath.h
svzpath.h: $(srcdir)/svzpath.h.in $(srcdir)/Makefile.in
	@echo "Generating svzpath.h..."
	@$(SED) < $(srcdir)/svzpath.h.in \
	  -e 's,@BINDIR@,@bindir@,g' \
	  -e 's,@PKGDATADIR@,@datadir@/serveez,g' \
	  -e 's,@LIBDIR@,@libdir@,g' \
	  > svzpath.h

noinst_HEADERS = ipdata.h iphlpapi.h

AM_CPPFLAGS = -D__BUILD_SVZ_LIBRARY__ -I$(top_srcdir)/src

CLEANFILES = *~ *.orig *.rej svzpath.h

MAINTAINERCLEANFILES = Makefile.in libserveez.dsp
