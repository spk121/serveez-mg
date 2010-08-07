dnl
dnl Autoconf macros for configuring the Serveez package.
dnl
dnl AC_GUILE -- Locate a Guile installation.
dnl This macro sets both the variables GUILE_CFLAGS and GUILE_LDFLAGS to be
dnl passed to the compiler and linker. In a first try it uses the 
dnl `guile-config' script in order to obtain these settings. Then it proceeds
dnl the `--with-guile=DIR' option of the ./configure script.
dnl
dnl AC_GUILE_SOURCE -- Check a source tree of Guile.
dnl Check if the user has given the `--with-guile-source=DIR' argument to
dnl ./configure script. If so, it overrides the above AC_GUILE macro and
dnl provides the following Makefile variables:
dnl   GUILE_SOURCE - the source directory
dnl   GUILE_CFLAGS - flags passed to the compiler
dnl   GUILE_LDFLAGS - flags passed to the linker
dnl   GUILE_DEPENDENCY - a dependency for the guile library
dnl   GUILE_RULE - name of the rule to make the guile library
dnl   GUILE_MAKE_LTDL - make command line for the above rule
dnl   GUILE_MAKE_LIB - yet another make line for that rule
dnl Please have a look at the `src/Makefile.am' file for more details how
dnl these variables are actually used to build a static guile library linked
dnl to the main binary.
dnl
dnl AC_GUILE_CONFIGURE -- Configure a Guile source tree.
dnl Configure the guile source tree if necessary. The directory has been given
dnl in the AC_GUILE_SOURCE directory. Use the local cache file for speeding up
dnl this process. Most optional modules of guile will be disabled. Tell the
dnl ./configure script of guile to build a static library only.
dnl
dnl AC_GUILE_CHECK -- Checks for Guile results and exits if necessary.
dnl
dnl AC_LIBTOOL_SOLARIS -- Helps libtool to build on Solaris.
dnl

AC_DEFUN([AC_GUILE], [
  AC_ARG_WITH(guile,
    [  --with-guile=DIR        guile installation in DIR @<:@/usr/local@:>@],
    [case "$withval" in
     no)  GUILEDIR="no" ;;
     yes) GUILEDIR="/usr/local" ;;
     *)   GUILEDIR="$withval" ;;
    esac],
    GUILEDIR="/usr/local")

  AC_MSG_CHECKING([for guile installation])
  if test "x$GUILEDIR" != "xno" ; then
    GUILEDIR="`eval cd "$GUILEDIR" 2>/dev/null && pwd`"
    case $build_os in
    mingw*)
	GUILEDIR="`eval cygpath -w -i "$GUILEDIR"`"
	GUILEDIR="`echo "$GUILEDIR" | sed -e 's%\\\\%/%g'`"
	;;
    esac
    if test -f "$GUILEDIR/lib/libguile.so" -o \
      -n "`find "$GUILEDIR/lib" -name "libguile.so.*" 2>/dev/null`" -o \
      -f "$GUILEDIR/lib/libguile.dylib" -o \
      -f "$GUILEDIR/bin/libguile.dll" -o \
      -f "$GUILEDIR/bin/cygguile.dll" ; then
      GUILE_CFLAGS="-I$GUILEDIR/include"
      if test x"$CYGWIN" = xyes -o x"$MINGW32" = xyes ; then
	GUILE_CFLAGS="-D__GUILE_IMPORT__ $GUILE_CFLAGS"
      fi
      GUILE_LDFLAGS="-L$GUILEDIR/lib -lguile"
      GUILE_BUILD="yes"
      AC_MSG_RESULT([yes])
    fi
  fi

  if test "x$GUILE_BUILD" != "xyes" ; then
    guile=""
    if test "x`eval guile-config --version 2>&1 | grep version`" != "x" ; then
      guile="`eval guile-config --version 2>&1 | grep version`"
      [guile="`echo $guile | sed -e 's/[^0-9\.]//g'`"]
    fi
    if test "x$guile" != "x" ; then
      case "$guile" in
      [1.3 | 1.3.[2-9] | 1.[4-9]* | [2-9].*)]
	AC_MSG_RESULT($guile >= 1.3)
	GUILE_BUILD="yes"
	;;
      [*)]
	AC_MSG_RESULT($guile < 1.3)
	AC_MSG_WARN([
  GNU Guile version 1.3 or above is needed, and you do not seem to have
  it handy on your system.])
	;;
      esac
      GUILE_CFLAGS="`eval guile-config compile`"
      GUILE_LDFLAGS="`eval guile-config link`"
    else
      AC_MSG_RESULT([missing])
      GUILE_CFLAGS=""
      GUILE_LDFLAGS=""
    fi
    unset guile
  fi
  unset GUILEDIR
  AC_SUBST(GUILE_CFLAGS)
  AC_SUBST(GUILE_LDFLAGS)
])

AC_DEFUN([AC_GUILE_SOURCE], [
  AC_ARG_WITH(guile-source,
    [  --with-guile-source     guile source tree in DIR @<:@/usr/src@:>@],
    [case "$withval" in
     no)  GUILESRC="no" ;;
     yes) GUILESRC="/usr/src" ;;
     *)   GUILESRC="$withval" ;;
    esac],
    GUILESRC="/usr/src")

  AC_MSG_CHECKING([for guile source tree])
  if test "x$GUILESRC" != "xno"; then
    GUILESRC="`eval cd "$GUILESRC" 2>/dev/null && pwd`"
    case $build_os in
    mingw*) 
	GUILESRC="`eval cygpath -w -i "$GUILESRC"`"
	GUILESRC="`echo "$GUILESRC" | sed -e 's%\\\\%/%g'`"
	;;
    esac
    if test -f "$GUILESRC/configure" ; then
      GUILE_SOURCE="$GUILESRC"
      GUILE_CFLAGS="-I$GUILESRC -I$GUILESRC/libguile -DGUILE_SOURCE"
      GUILE_LDFLAGS="$GUILESRC/libguile/libguile.la"
      GUILE_DEPENDENCY="$GUILESRC/libguile/libguile.la"
      GUILE_RULE="$GUILESRC/libguile/libguile.la"
      GUILE_MAKE_LTDL='(cd $(GUILE_SOURCE)/libltdl && $(MAKE) libltdlc.la)'
      if test ! -f "$GUILESRC/libltdl/configure" ; then 
	GUILE_MAKE_LTDL="# $GUILE_MAKE_LTDL"
      fi
      GUILE_MAKE_LIB='(cd $(GUILE_SOURCE)/libguile && $(MAKE) libguile.la)'
      AC_SUBST(GUILE_SOURCE)
      AC_SUBST(GUILE_CFLAGS)
      AC_SUBST(GUILE_LDFLAGS)
      AC_SUBST(GUILE_DEPENDENCY)
      AC_SUBST(GUILE_RULE)
      AC_SUBST(GUILE_MAKE_LTDL)
      AC_SUBST(GUILE_MAKE_LIB)
      AC_MSG_RESULT([yes])
      GUILE_BUILD="yes"
    else
      AC_MSG_RESULT([missing])
      GUILE_SOURCE="no"
    fi
  else
    AC_MSG_RESULT([disabled])
    GUILE_SOURCE="no"
  fi
  unset GUILESRC
])

AC_DEFUN([AC_GUILE_CONFIGURE], [
  if test "x$GUILE_SOURCE" != "xno" ; then
    if test "`echo "$cache_file" | cut -b 1`" != "/" ; then
      cache_file="`pwd`/$cache_file"
    fi
    case $build_os in
    mingw*)
	cache_file="`eval cygpath -w -i "$cache_file"`"
	cache_file="`echo "$cache_file" | sed -e 's%\\\\%/%g'`"
	;;
    esac
    if test ! -f "$GUILE_SOURCE/libguile/scmconfig.h" ; then
      AC_MSG_RESULT([configuring Guile...])
      ([cd $GUILE_SOURCE && $SHELL configure \
        --enable-static --disable-shared \
        --disable-debug-freelist --disable-debug-malloc --disable-guile-debug \
        --disable-arrays --disable-posix --enable-networking --disable-regex \
        --without-threads --enable-ltdl-convenience --prefix=$prefix \
        --host=$host --build=$build --target=$target --cache-file=$cache_file])
    else
      AC_MSG_RESULT([The file \`$GUILE_SOURCE/libguile/scmconfig.h' exists.])
      AC_MSG_RESULT([Guile already configured... skipped.])
    fi
  fi
])

AC_DEFUN([AC_GUILE_CHECK], [
  if test "x$GUILE_BUILD" != "xyes" ; then
    AC_MSG_ERROR([
  The $PACKAGE $VERSION package requires either an installed Guile
  version or an unpacked source tarball at hand.  You can specify the
  install location by passing \`--with-guile=<directory>' or the source
  location by passing \`--with-guile-source=<directory>'.  Guile 
  version 1.4 is preferred.])
  fi
])

AC_DEFUN([AC_LIBTOOL_SOLARIS], [
  if test "x$GCC" = "xyes" -a "x$enable_shared" = "xyes" ; then
    case $host_os in
    solaris*)
      LIBERTY="`gcc --print-file-name=libiberty.a`"
      LIBERTY="-L`dirname $LIBERTY 2>/dev/null`"
      SERVEEZ_LDFLAGS="$SERVEEZ_LDFLAGS $LIBERTY"
      GCCLIB="`gcc --print-libgcc-file-name`"
      GCCDIR="-L`dirname $GCCLIB 2>/dev/null`"
      GCCFILE="`basename $GCCLIB 2>/dev/null`"
      GCCFILE="-l`echo "$GCCFILE" | sed -e 's/lib\(.*\)\.a/\1/'`"
      SERVEEZ_LDFLAGS="$SERVEEZ_LDFLAGS $GCCDIR"
      SERVEEZ_LIBS="$SERVEEZ_LIBS $GCCFILE"
      AC_MSG_WARN([
  The configure script added
  '$LIBERTY $GCCDIR $GCCFILE'
  to your linker line.  This may not be what you want.  Please report
  to <bug-serveez@gnu.org> if we failed to build shared libraries 
  for '$host_os'.])
      unset LIBERTY
      unset GCCLIB
      unset GCCDIR
      unset GCCFILE
      ;;
    esac
  fi
])
