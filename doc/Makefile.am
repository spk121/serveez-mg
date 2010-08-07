## Process this file with automake to produce Makefile.in
# 
# doc/Makefile.am
#
# Automake input file for the documentation directory.
#
# Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
# Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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

AUTOMAKE_OPTIONS = no-texinfo.tex
info_TEXINFOS = serveez.texi serveez-api.texi
serveez_TEXINFOS = guile-api.texi
man_MANS = serveez.1 serveez-config.1
EXTRA_DIST = $(man_MANS) index.html serveez.en.html guile-api.texi.in \
	logo.jpg logosmall.jpg mandel.jpg serveez-api.texi.in \
	serveez-doc-snarf.awk
CLEANFILES = *.info *.tgz *~ *.info-[0-9] stamp-vti* serveez-api.texi \
	$(SED_FILE) guile-api.texi $(GUILE_SED_FILE) $(HTML_DIR)/*.html \
	$(GUILE_TXT_FILE)
MAINTAINERCLEANFILES = Makefile.in
DISTCLEANFILES = stamp-* version*.texi
DOC_DIRS = $(top_srcdir)/src/libserveez \
  $(top_srcdir)/src/libserveez/coserver $(top_srcdir)/src/libserveez/codec
DOC_FILES = icmp-socket.c raw-socket.c udp-socket.c pipe-socket.c	     \
  tcp-socket.c alloc.c sparsevec.c hash.c util.c vector.c socket.c	     \
  server-core.c server-loop.c server-socket.c boot.c interface.c interface.h \
  dynload.c windoze.c snprintf.c server.c core.c array.c hash.h binding.c    \
  portcfg.c array.h coserver.c coserver.h vector.h portcfg.h server-core.h   \
  socket.h alloc.h server.h util.h codec.c passthrough.c passthrough.h cfg.c \
  cfg.h asprintf.c mutex.c mutex.h
SED_FILE = serveez-api.sed
GUILE_DOC_DIRS = $(top_srcdir)/src
GUILE_DOC_FILES = guile.c guile-server.c guile-bin.c guile-api.c
GUILE_SED_FILE = guile-api.sed
GUILE_TXT_FILE = serveez-procedures.txt
SNARFER = $(AWK) -f $(srcdir)/serveez-doc-snarf.awk
HTML_DIR = manual
SITE_DIR = $(top_srcdir)/../serveez-site
SED = sed
MKDIR = mkdir -p
CP = cp
RM = rm
SITE_FILES = serveez.en.html index.html *.jpg
MANUAL_FILES = \
  $(top_srcdir)/README $(top_srcdir)/README-Win32 $(top_srcdir)/TODO \
  $(top_srcdir)/BUGS $(top_srcdir)/NEWS $(top_srcdir)/README-cross
LOCAL_TARGETS = $(GUILE_TXT_FILE) \
  $(HTML_DIR)/serveez.html $(HTML_DIR)/serveez-api.html
pkgdata_DATA = $(GUILE_TXT_FILE)

# redefines DIST_COMMON?

DIST_COMMON = $(serveez_TEXINFOS) $(srcdir)/Makefile.am \
        $(srcdir)/Makefile.in $(srcdir)/version.texi $(srcdir)/version1.texi \
	ChangeLog mdate-sh

 # local dependencies

builddir = $(top_builddir)/doc

distclean-local:
	$(RM) -rf *\~ *.orig *.rej $(HTML_DIR)/

all-local: $(LOCAL_TARGETS)

dist-hook:
	$(RM) -f $(distdir)/*.info $(distdir)/*.info-[0-9] \
		$(distdir)/stamp-vti* $(distdir)/serveez-api.texi \
		$(distdir)/guile-api.texi $(distdir)/stamp-*

serveez-api.texi: $(srcdir)/serveez-api.texi.in
	$(RM) -f $(SED_FILE)
	for dir in $(DOC_DIRS); do \
	  for file in $(DOC_FILES); do \
	    if test -r $$dir/$$file; then \
	      $(SNARFER) $$dir/$$file >> $(SED_FILE); fi \
	  done \
	done
	$(SED) -f $(SED_FILE) < $(srcdir)/serveez-api.texi.in > $@

guile-api.texi: $(srcdir)/guile-api.texi.in
	$(RM) -f $(GUILE_SED_FILE)
	for dir in $(GUILE_DOC_DIRS); do \
	  for file in $(GUILE_DOC_FILES); do \
	    if test -r $$dir/$$file; then \
	      $(SNARFER) $$dir/$$file >> $(GUILE_SED_FILE); fi \
	  done \
	done
	$(SED) -f $(GUILE_SED_FILE) < $(srcdir)/guile-api.texi.in > $@

$(GUILE_TXT_FILE):
	$(RM) -f $@
	for dir in $(GUILE_DOC_DIRS); do \
	  for file in $(GUILE_DOC_FILES); do \
	    if test -r $$dir/$$file; then \
	      $(SNARFER) $$dir/$$file -t >> $@; fi \
	  done \
	done

site: all-local info
	test ! -d $(SITE_DIR) || $(RM) -rf $(SITE_DIR)
	@$(MKDIR) $(SITE_DIR)
	$(CP) -p serveez.en.html logo.jpg $(SITE_DIR)
	$(CP) -rp $(HTML_DIR) $(SITE_DIR)
	$(CP) -p index.html logosmall.jpg mandel.jpg $(SITE_DIR)/$(HTML_DIR)
	$(CP) -p $(MANUAL_FILES) $(SITE_DIR)/$(HTML_DIR)

$(HTML_DIR)/serveez.html: serveez.texi guile-api.texi
	$(TEXI2HTML) -subdir $(HTML_DIR) -I $(srcdir) -I $(builddir) -split_chapter $(srcdir)/serveez.texi

$(HTML_DIR)/serveez-api.html: serveez-api.texi
	$(TEXI2HTML) -subdir $(HTML_DIR) -I $(srcdir) -I $(builddir) -split_chapter $(builddir)/serveez-api.texi

.texi.info:
	$(MAKEINFO) -o $(builddir)/$@ -I $(srcdir) -I $(builddir) --no-split $<

.texi.dvi:
	$(TEXI2DVI) -I $(srcdir) -I $(builddir) $<

SUFFIXES = .html .info .dvi
