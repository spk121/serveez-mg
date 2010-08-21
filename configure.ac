dnl -*-autoconf-*-
dnl
dnl configure.in
dnl
dnl Autoconf input file.
dnl
dnl Copyright (C) 2000, 2001, 2002, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
dnl Copyright (C) 2000, 2001 Raimund Jacob <raimi@lkcc.org>
dnl Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
dnl Copyright (C) 2010 Michael Gran <spk121@yahoo.com>
dnl
dnl This is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl 
dnl This software is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl 
dnl You should have received a copy of the GNU General Public License
dnl along with this package; see the file COPYING.  If not, write to
dnl the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
dnl Boston, MA 02111-1307, USA.  
dnl

dnl ----------------------------------------------------------------------

dnl
dnl Initialize autoconf and automake
dnl
AC_INIT([serveez],[0.1.7],[bug-serveez@gnu.org])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_SRCDIR([src/serveez.c])
dnl
dnl Determine if maintainer portions of the Makefiles should be included.
dnl
dnl ----------------------------------------------------------------------

dnl
dnl Check for a proper C compiler.
dnl
dnl AC_PROG_CC
AC_C_BIGENDIAN
LT_INIT

dnl ----------------------------------------------------------------------

dnl
dnl Check whether debug output should be included.
dnl

AC_MSG_CHECKING([whether to enable debug output])
AC_ARG_ENABLE([debug],
  [  --disable-debug         Include debug output @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_debug=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_debug=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_debug=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_debug = yes ; then
  AC_DEFINE(SVZ_ENABLE_DEBUG, 1, [Define if debug output should be supported.])
else
  AC_DEFINE(NDEBUG, 1, [Define if debug code should be suppressed.])
fi
unset define_enable_debug

dnl ----------------------------------------------------------------------

dnl
dnl Check whether we try to list local interfaces.
dnl

AC_MSG_CHECKING([whether to enable list of local interfaces])
AC_ARG_ENABLE([iflist],
  [  --disable-iflist        List local interfaces @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_iflist=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_iflist=no
    AC_MSG_RESULT([no])
    fi,
  define_enable_iflist=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_iflist = yes ; then
    AC_DEFINE(ENABLE_IFLIST,
      1, [Define if we are able to list local network interfaces.])
fi
unset define_enable_iflist

dnl ----------------------------------------------------------------------

dnl
dnl Check whether poll loop should be supported.
dnl

AC_MSG_CHECKING([whether to enable poll loop])
AC_ARG_ENABLE([poll],
  [  --disable-poll          Include poll() server loop @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_poll=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_poll=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_poll=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_poll = yes ; then
  AC_DEFINE(ENABLE_POLL,
    1, [Define if poll() should be supported if possible.])
fi
unset define_enable_poll

dnl
dnl Check whether some thread safety functionality should be included.
dnl

AC_ARG_WITH([thread-safety],
  [  --with-thread-safety    Enable some thread-safety functionality @<:@default=no@:>@],
  [case "$withval" in
   no)  enable_threads="no" ;;
   yes) enable_threads="yes" ;;
   *)   enable_threads="yes" ;;
  esac],
  enable_threads="no")

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the SNTP server should be included.
dnl
AC_MSG_CHECKING([whether to enable the SNTP server])
AC_ARG_ENABLE([sntp-proto],
  [  --disable-sntp-proto    Include the SNTP server @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_sntp=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_sntp=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_sntp=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_sntp = yes ; then
  AC_DEFINE(ENABLE_SNTP_PROTO,
    1, [Define if the SNTP server should be included.])
  SERVERLIBS="$SERVERLIBS sntp-server/libsntp.a"
  SERVERDIRS="$SERVERDIRS sntp-server"
fi
unset define_enable_sntp

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the Gnutella server should be included.
dnl

AC_MSG_CHECKING([whether to enable the Gnutella spider])
AC_ARG_ENABLE([gnutella],
  [  --disable-gnutella      Include the Gnutella spider @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_nut=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_nut=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_nut=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_nut = yes ; then
  AC_DEFINE(ENABLE_GNUTELLA,
    1, [Define if the Gnutella spider should be included.])
  SERVERLIBS="$SERVERLIBS nut-server/libnut.a"
  SERVERDIRS="$SERVERDIRS nut-server"
fi
unset define_enable_nut

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the Tunnel server should be included.
dnl

AC_MSG_CHECKING([whether to enable the port forwarder])
AC_ARG_ENABLE([tunnel],
  [  --disable-tunnel        Include port forwarder @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_tunnel=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_tunnel=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_tunnel=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_tunnel = yes ; then
  AC_DEFINE(ENABLE_TUNNEL,
    1, [Define if the port forwarder should be included.])
  SERVERLIBS="$SERVERLIBS tunnel-server/libtunnel.a"
  SERVERDIRS="$SERVERDIRS tunnel-server"
fi
unset define_enable_tunnel

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the control protocol should be included.
dnl

AC_MSG_CHECKING([whether to enable support for the control protocol])
AC_ARG_ENABLE([control-proto],
  [  --disable-control-proto Include the control protocol @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_control_proto=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_control_proto=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_control_proto=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_control_proto = yes ; then
  AC_DEFINE(ENABLE_CONTROL_PROTO,
    1, [Define if the control protocol should be supported.])
  SERVERLIBS="$SERVERLIBS ctrl-server/libctrl.a"
  SERVERDIRS="$SERVERDIRS ctrl-server"
fi
unset define_enable_control_proto

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the IRC protocol should be included.
dnl

AC_MSG_CHECKING([whether to enable support for the IRC protocol])
AC_ARG_ENABLE([irc-proto],
  [  --disable-irc-proto     Include the IRC protocol @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_irc_proto=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_irc_proto=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_irc_proto=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_irc_proto = yes ; then
  AC_DEFINE(ENABLE_IRC_PROTO,
    1, [Define if the IRC protocol should be supported.])
  SERVERLIBS="$SERVERLIBS irc-server/libircserver.a irc-core/libirccore.a"
  SERVERDIRS="$SERVERDIRS irc-server irc-core"
fi
unset define_enable_irc_proto

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the IRC protocol "TimeStamp" extension should be included.
dnl

AC_MSG_CHECKING([whether to enable TimeStamp for the IRC protocol])
AC_ARG_ENABLE([irc-ts],
  [  --disable-irc-ts        Include TimeStamp in IRC protocol @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_irc_ts=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_irc_ts=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_irc_ts=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_irc_ts = yes ; then
  AC_DEFINE(ENABLE_TIMESTAMP,
    1, [Define if the IRC protocol TimeStamp extension should be supported.])
fi
unset define_enable_irc_ts

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the password encryption should be included.
dnl

AC_MSG_CHECKING([whether to enable password encryption])
AC_ARG_ENABLE([crypt],
  [  --enable-crypt          Include password encryption @<:@default=no@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_crypt=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_crypt=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_crypt=no
  AC_MSG_RESULT([no]))

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the aWCS protocol should be included.
dnl

AC_MSG_CHECKING([whether to enable support for the aWCS protocol])
AC_ARG_ENABLE([awcs-proto],
  [  --disable-awcs-proto    Include the aWCS protocol @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_awcs_proto=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_awcs_proto=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_awcs_proto=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_awcs_proto = yes ; then
  AC_DEFINE(ENABLE_AWCS_PROTO,
    1, [Define if the aWCS protocol should be supported.])
  SERVERLIBS="$SERVERLIBS awcs-server/libawcs.a"
  SERVERDIRS="$SERVERDIRS awcs-server"
fi
unset define_enable_awcs_proto

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the HTTP protocol should be included.
dnl

AC_MSG_CHECKING([whether to enable support for the HTTP protocol])
AC_ARG_ENABLE([http-proto],
  [  --disable-http-proto    Include the HTTP protocol @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_http_proto=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_http_proto=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_http_proto=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_http_proto = yes ; then
  AC_DEFINE(ENABLE_HTTP_PROTO,
    1, [Define if the HTTP protocol should be supported.])
  SERVERLIBS="$SERVERLIBS http-server/libhttp.a"
  SERVERDIRS="$SERVERDIRS http-server"
fi
unset define_enable_http_proto

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the Fake Ident Server should be included.
dnl

AC_MSG_CHECKING([whether to enable the fake ident server])
AC_ARG_ENABLE([fakeident],
  [  --disable-fakeident     Include the fake ident server @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_fakeident=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_fakeident=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_fakeident=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_fakeident = yes ; then
  AC_DEFINE(ENABLE_FAKEIDENT,
    1, [Define if the Fake Ident Server should be included.])
  SERVERLIBS="$SERVERLIBS fakeident-server/libfakeident.a"
  SERVERDIRS="$SERVERDIRS fakeident-server"
fi
unset define_enable_fakeident

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the passthrough server should be included.
dnl

AC_MSG_CHECKING([whether to enable the passthrough server])
AC_ARG_ENABLE([passthrough],
  [  --disable-passthrough   Include the passthrough server @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_prog=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_prog=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_prog=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_prog = yes ; then
  AC_DEFINE(ENABLE_PROG_SERVER,
    1, [Define if the program passthrough server should be included.])
  SERVERLIBS="$SERVERLIBS prog-server/libprog.a"
  SERVERDIRS="$SERVERDIRS prog-server"
fi
unset define_enable_prog

dnl ----------------------------------------------------------------------

dnl
dnl Check whether the flood protection should be included.
dnl

AC_MSG_CHECKING([whether to enable support for flood protection])
AC_ARG_ENABLE([flood],
  [  --disable-flood         Include flood protection @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_flood=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_flood=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_flood=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_flood = yes; then
  AC_DEFINE(SVZ_ENABLE_FLOOD_PROTECTION,
    1, [Define if flood protection should be supported.])
fi
unset define_enable_flood

dnl ----------------------------------------------------------------------

dnl
dnl Checking for libtool support.
dnl 

dnl LT_INIT([win32-dll])

dnl ----------------------------------------------------------------------

dnl
dnl Check Guile installation.
dnl

PKG_CHECK_MODULES(GUILE, guile-2.0)
AC_SUBST(GUILE_CFLAGS)
AC_SUBST(GUILE_LIBS)

dnl ----------------------------------------------------------------------

dnl
dnl Check if guile servers should be included.
dnl

AC_MSG_CHECKING([whether to include guile server support])
AC_ARG_ENABLE([guile-server],
  [  --disable-guile-server  Include guile servers @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_guile=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_guile=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_guile=yes
  AC_MSG_RESULT([yes]))

if test $define_enable_guile = yes ; then
  AC_DEFINE(ENABLE_GUILE_SERVER, 
    1, [Define if guile servers should be supported.])
fi
unset define_enable_guile

dnl ----------------------------------------------------------------------

dnl
dnl Check whether to include support for named pipes under Linux.
dnl
AC_CHECK_HEADERS([rpc/pmap_clnt.h])
AC_CHECK_FUNCS([sendfile pmap_set])

dnl ----------------------------------------------------------------------

dnl
dnl Check whether to include support for heap counters.
dnl
AC_MSG_CHECKING([whether to enable heap counters])
AC_ARG_ENABLE([heap-count],
  [  --disable-heap-count    Include support for heap counters @<:@default=yes@:>@],
  if test x$enableval = "xyes" ; then
    define_enable_heap=yes
    AC_MSG_RESULT([yes])
  else
    define_enable_heap=no
    AC_MSG_RESULT([no])
  fi,
  define_enable_heap=yes
  AC_MSG_RESULT([yes]))

if test x$define_enable_heap = "xyes" ; then
  AC_DEFINE(ENABLE_HEAP_COUNT, 
    1, [Define if heap counters should be enabled.])
fi
unset define_enable_heap

dnl
dnl Output files.
dnl
AC_CONFIG_SUBDIRS([src/libserveez])

AC_CONFIG_FILES([
    Makefile
    src/Makefile 
    src/serveez-config
    src/awcs-server/Makefile
    src/ctrl-server/Makefile
    src/http-server/Makefile
    src/irc-core/Makefile
    src/irc-server/Makefile
    src/foo-server/Makefile
    src/sntp-server/Makefile
    src/nut-server/Makefile
    src/tunnel-server/Makefile
    src/fakeident-server/Makefile
    src/prog-server/Makefile
    src/guile-server/Makefile
    data/Makefile
    test/Makefile
    redhat.sh
    debian.sh
    src/version.h])
AC_OUTPUT

dnl
dnl Print results.
dnl
AC_MSG_RESULT([])
AC_MSG_RESULT([ $PACKAGE version $VERSION configured successfully.])
AC_MSG_RESULT([])

