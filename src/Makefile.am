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

SUBDIRS = libserveez

pkgsysconfdir = ${sysconfdir}/${PACKAGE}
dist_pkgsysconf_DATA = serveez.cfg

pkgguilesitedir = ${GUILE_SITEDIR}/${PACKAGE}
dist_pkgguilesite_DATA = lib.scm

bin_PROGRAMS = serveez mkpassword

serveez_SOURCES = \
	serveez.c serveez.h \
	guile.c guile.h \
	cfgfile.c cfgfile.h \
	option.c option.h \
	guile-api.c guile-api.h \
	guile-server.c guile-server.h \
	ctrl-server/control-proto.c \
	ctrl-server/control-proto.h \
	http-server/http-cache.c \
	http-server/http-cache.h \
	http-server/http-cgi.c \
	http-server/http-cgi.h \
	http-server/http-core.c \
	http-server/http-core.h \
	http-server/http-dirlist.c \
	http-server/http-dirlist.h \
	http-server/http-proto.c \
	http-server/http-proto.h \
	prog-server/prog-server.c \
	prog-server/prog-server.h

LDADD = libserveez/libserveez.la \
	$(GUILE_LIBS)

EXTRA_DIST guile-server/hello.scm

AM_CPPFLAGS = $(GUILE_CFLAGS) -I$(srcdir)/libserveez

mkpassword_SOURCES = mkpassword.c getpasswd.c getpasswd.h
mkpassword_LDADD = -lcrypt

# If Guile's sitedir isn't under this package's $(prefix), distcheck
# will fail.  This is pretty common.  So, for distcheck, Guile's
# sitedir is always under $(prefix).
distcheck-hook:
  GUILE_SITEDIR=${datarootdir}/guile/site