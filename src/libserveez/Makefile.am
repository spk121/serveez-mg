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

lib_LTLIBRARIES = libserveez.la

libserveez_la_SOURCES = alloc.c util.c hash.c sparsevec.c tcp-socket.c	\
  pipe-socket.c udp-socket.c icmp-socket.c raw-socket.c server-core.c	\
  server-loop.c boot.c server.c server-socket.c interface.c dynload.c	\
  vector.c core.c socket.c array.c portcfg.c binding.c passthrough.c	\
  cfg.c mutex.c coserver/coserver.c coserver/dns.c coserver/ident.c	\
  coserver/reverse-dns.c coserver/coserver.h coserver/dns.h		\
  coserver/ident.h coserver/reverse-dns.h codec/codec.c codec/gzlib.c	\
  codec/codec.h codec/gzlib.h libserveez.h

libserveez_la_LIBADD = -ldl -lz

AM_CPPFLAGS = -DDATADIR='$(datadir)'