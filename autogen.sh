#! /bin/sh
#
# autogen.sh
#
# Run this script to re-generate all maintainer-generated files.
#
# Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
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

here=`pwd`
cd `dirname $0`

# let automake find this automatic created file
if ! test -f doc/serveez-api.texi; then
cat <<EOF > doc/serveez-api.texi
@setfilename serveez-api.info
@include version1.texi
EOF
info_touched="yes"
fi

echo -n "Creating aclocal.m4... "
aclocal
echo "done."
echo -n "Creating config.h.in... "
autoheader
echo "done."
echo -n "Creating ltmain.sh... "
libtoolize -f -c --automake
echo "done."
echo -n "Creating Makefile.in(s)... "
automake -a -f -c
echo "done."
echo -n "Creating configure... "
autoconf
echo "done."
echo -n "Creating Win32 projects... "
perl autodsp
echo "done."

# patching libtool 1.5 code for MinGW32 build
echo -n "Patching configure... "
mv -f configure configure.x
cat configure.x | sed 's/x86 DLL/x86 DLL|\^x86 archive static/' > configure
chmod +x configure
rm -f configure.x
echo "done."

# reschedule this file for building
if test x"$info_touched" = xyes ; then rm -f doc/serveez-api.texi; fi

#
# run configure, maybe with parameters recorded in config.status
#
if [ -r config.status ]; then
  # Autoconf 2.13
  CMD=`awk '/^#.*\/?configure .*/ { $1 = ""; print; exit }' < config.status`
  if test "x$CMD" = "x" ; then
    # Autoconf 2.5x
    CMD=`grep "with options" < config.status | \
         sed 's/[^"]*["]\([^"]*\)["]/\1/' | sed 's/\\\//g' | sed "s/'//g"`
    CMD="./configure $CMD"
  fi
else
  CMD="./configure --enable-maintainer-mode --enable-warn"
fi
echo "Running $CMD $@ ..."
$CMD "$@"
