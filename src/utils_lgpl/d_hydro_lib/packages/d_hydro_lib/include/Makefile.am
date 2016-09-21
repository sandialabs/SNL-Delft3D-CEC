#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
#-------------------------------------------------------------------------------
#Includes
dist_noinst_HEADERS = \
	d_hydro_lib_version.h

# For source distributions: the tar.gz will contain the correct version numbers in the generated *version.h
# So, just before packaging the .tar.gz dist, disable version number Regeneration parts of this Makefile.am.
dist-hook:
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.am
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.in

BUILT_SOURCES = ./always \
	d_hydro_lib_version.h

REGENERATE_VN=yes

# (the space before ifeq is intentional to fool Automake, and leave this conditional to Make)
 ifeq ($(REGENERATE_VN),yes)
# We're in a working copy build dir, so always update the *version.h
d_hydro_lib_version.h: d_hydro_lib_version.h.svn ./always
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh d_hydro_lib_version.h $(srcdir) version_number.ini $(top_builddir)
./always:
	rm -rf d_hydro_lib_version.h
 else
# Do not force regeneration of *version.h, because it is already shipped in a source distribution archive.
d_hydro_lib_version.h:
./always:
 endif

.PHONY: ./always

#-------------------------------------------------------------------------------
#Libraries
