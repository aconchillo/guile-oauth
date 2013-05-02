# Makefile.am
#
# Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-oauth.
#
# guile-oauth is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3, or
# (at your option) any later version.
#
# guile-oauth is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with guile-oauth; see the file COPYING.  If not,
# write to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA  02110-1301  USA

moddir=$(prefix)/share/guile/site/oauth/oauth1

SOURCES = client.scm request.scm utils.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

CLEANFILES = $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"
