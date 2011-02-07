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
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this package.  If not, see <http://www.gnu.org/licenses/>.
#

lib_LTLIBRARIES = libserveez.la

libserveez_la_SOURCES = alloc.c alloc.h array.c array.h binding.c	\
  binding.h boot.c boot.h cfg.c cfg.h codec/codec.c codec/codec.h	\
  codec/gzlib.c codec/gzlib.h core.c core.h coserver/coserver.c	\
  coserver/coserver.h coserver/dns.c coserver/dns.h coserver/ident.c	\
  coserver/ident.h coserver/reverse-dns.c coserver/reverse-dns.h	\
  defines.h dynload.c dynload.h hash.c hash.h icmp-socket.c		\
  icmp-socket.h interface.c interface.h libserveez.h mutex.c mutex.h	\
  passthrough.c passthrough.h pipe-socket.c pipe-socket.h portcfg.c	\
  portcfg.h raw-socket.c raw-socket.h server-core.c server-core.h	\
  server-loop.c server-loop.h server-socket.c server-socket.h server.c	\
  server.h socket.c socket.h sparsevec.c sparsevec.h tcp-socket.c	\
  tcp-socket.h udp-socket.c udp-socket.h util.c util.h vector.c	\
  vector.h

libserveez_la_LIBADD = -ldl -lz

AM_CPPFLAGS = -DDATADIR='$(datadir)'