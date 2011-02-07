## Process this file with automake to produce Makefile.in
#
# data/Makefile.am
#
# Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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

EXTRA_DIST = mime.types irc-MOTD.txt serveez.cfg \
	serveez1.ico serveez2.ico init.serveez.in PLATFORMS
SED = sed

install-init: init.serveez
	@instdir="/sbin/init.d" ; \
	test -d $$instdir || instdir="/etc/init.d" ; \
	$(INSTALL_SCRIPT) init.serveez $$instdir ; \
	$(INSTALL_DATA) serveez.cfg /etc/serveez.conf ; \
	echo "Init script \`init.serveez' has been installed in \`$$instdir'." ; \
	echo "  Please note that you need to set symbolic links in one of" ; \
	echo "  the \`$$instdir/rc{runlevel}.d' directories to start and stop" ; \
	echo "  the installed service automatically.  You can" ;\
	echo "    $$ ln -s $$instdir/init.serveez $$instdir/rc{runlevel}.d/S99serveez" ; \
	echo "    $$ ln -s $$instdir/init.serveez $$instdir/rc{runlevel}.d/K01serveez" ; \
	echo "  in order to get a very late start (and early kill)."

init.serveez: init.serveez.in
	@instdir="/sbin/init.d" ; \
	test -d $$instdir || instdir="/etc/init.d" ; \
	echo "Generating init.serveez..." ; \
	$(SED) < $(srcdir)/init.serveez.in \
	  -e 's,@BINDIR@,@bindir@,g' \
	  -e 's,@INSTDIR@,'$$instdir',g' \
	  > $(srcdir)/init.serveez
	@chmod +x $(srcdir)/init.serveez

CLEANFILES = *~ *.orig *.rej init.serveez
MAINTAINERCLEANFILES = Makefile.in
