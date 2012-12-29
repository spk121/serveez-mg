dnl -*-autoconf-*-
dnl
dnl configure.in
dnl
dnl Autoconf input file.
dnl
dnl Copyright (C) 2000, 2001, 2002, 2003, 2004 Stefan Jahn <stefan@lkcc.org>
dnl Copyright (C) 2000, 2001 Raimund Jacob <raimi@lkcc.org>
dnl Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
dnl Copyright (C) 2010, 2012 Michael Gran <spk121@yahoo.com>
dnl
dnl This is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl
dnl This software is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this package.  If not, see <http://www.gnu.org/licenses/>.
dnl

dnl ----------------------------------------------------------------------

dnl
dnl Initialize autoconf and automake
dnl
AC_INIT([Serveez-MG],[0.3],[spk121@yahoo.com])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_SRCDIR([src/serveez.c])
AC_CONFIG_MACRO_DIR([m4])

dnl ----------------------------------------------------------------------

dnl
dnl Check for C compiler and libtool.
dnl
AC_PROG_CC
AC_C_BIGENDIAN
LT_INIT[static disable-shared]

dnl ----------------------------------------------------------------------

dnl
dnl Check Guile installation.
dnl

PKG_CHECK_MODULES([GUILE], [guile-2.0])
AC_SUBST([GUILE_CFLAGS])
AC_SUBST([GUILE_LIBS])
AC_ARG_VAR([GUILE_SITEDIR],
           [Directory where Guile Scheme files specific to your site should be installed])
if test -z "$GUILE_SITEDIR"
then
  GUILE_SITEDIR=`guile -c "(display (%site-dir))"`
fi
AC_SUBST([GUILE_SITEDIR])

dnl ----------------------------------------------------------------------

dnl
dnl Check for functions and headers that are in LSB 4.1 but not 
dnl in current Cygwin
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
    doc/Makefile
    src/Makefile 
    data/Makefile
    test/Makefile
    m4/Makefile])
AC_OUTPUT

dnl
dnl Print results.
dnl
AC_MSG_RESULT([])
AC_MSG_RESULT([ $PACKAGE version $VERSION configured successfully.])
AC_MSG_RESULT([])

