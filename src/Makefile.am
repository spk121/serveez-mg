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

pkgdata_DATA = serveez.scm serveez.cfg

#
# Add your server subdirectory here
#
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
	guile-server.c guile-server.h \
	awcs-server/awcs-proto.c \
	awcs-server/awcs-proto.h \
	ctrl-server/control-proto.c \
	ctrl-server/control-proto.h \
	fakeident-server/ident-proto.c \
	fakeident-server/ident-proto.h \
	foo-server/foo-proto.c \
	foo-server/foo-proto.h \
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
	irc-core/irc-core.c \
	irc-core/irc-core.h \
	irc-server/irc-config.c \
	irc-server/irc-config.h \
	irc-server/irc-crypt.c \
	irc-server/irc-crypt.h \
	irc-server/irc-event-1.c \
	irc-server/irc-event-2.c \
	irc-server/irc-event-3.c \
	irc-server/irc-event-4.c \
	irc-server/irc-event-5.c \
	irc-server/irc-event-6.c \
	irc-server/irc-event-7.c \
	irc-server/irc-event.h \
	irc-server/irc-proto.c \
	irc-server/irc-proto.h \
	irc-server/irc-server.c \
	irc-server/irc-server.h \
	nut-server/gnutella.c \
	nut-server/gnutella.h \
	nut-server/nut-core.c \
	nut-server/nut-core.h \
	nut-server/nut-hostlist.c \
	nut-server/nut-hostlist.h \
	nut-server/nut-request.c \
	nut-server/nut-request.h \
	nut-server/nut-route.c \
	nut-server/nut-route.h \
	nut-server/nut-transfer.c \
	nut-server/nut-transfer.h \
	prog-server/prog-server.c \
	prog-server/prog-server.h \
	sntp-server/sntp-proto.c \
	sntp-server/sntp-proto.h \
	tunnel-server/tunnel.c \
	tunnel-server/tunnel.h

LDADD = libserveez/libserveez.la \
	$(GUILE_LIBS)

AM_CPPFLAGS = $(GUILE_CFLAGS) -I$(srcdir)/libserveez

# mkpassword_SOURCES = mkpassword.c getpass.c getpass.h
# mkpassword_LDADD = -lcrypt

