#
# Makefile.am
#
# Copyright (C) 2013-2018 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-oauth.
#
# guile-oauth is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# guile-oauth is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with guile-oauth; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA
#

moddir = $(datadir)/guile/site/@GUILE_EFFECTIVE_VERSION@/oauth/industria
objdir = $(libdir)/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache/oauth/industria

SOURCES = base64.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

CLEANFILES = $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"
