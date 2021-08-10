#-------------------------------------------------------------------------------
#   Makefile gridgeom
#
#   07 jun 2017
#
#   Copyright (C)  Stichting Deltares, 2011-2017.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libgridgeom.la

# For source distributions: the tar.gz will contain the correct version numbers in the generated *version.F90
# So, just before packaging the .tar.gz dist, disable version number REgeneration parts of this Makefile.am.
dist-hook:
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.am
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.in

# Version number generation (only when in SVN source dir, not for tar.gz dist)

REGENERATE_VN=yes

# (the space before ifeq is intentional to fool Automake, and leave this conditional to Make)
 ifeq ($(REGENERATE_VN),yes)
# We're in a working copy build dir, so always update the *version.F90
gridgeom_version.F90: gridgeom_version.F90.svn .FORCE
	$(top_builddir)/scripts_lgpl/linux/update_version.sh \
	$(builddir)/$@ $(srcdir) $(srcdir)/../include/version_number.ini $(abs_top_builddir)
 else
# Do not force regeneration of *version.F90, because it is already shipped in a source distribution archive.
gridgeom_version.F90:
 endif

.FORCE:

.PHONY: .FORCE

#-------------------------------------------------------------------------------
#Includes

FMODULES += \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/third_party_open/kdtree2/src-f90 \
    -I$(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src \
    -I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
    -I$(builddir)/../include

#-------------------------------------------------------------------------------
#Sources
libgridgeom_la_SOURCES = \
         generalmodules.f90 \
         polygon.f90 \
         network_data.f90 \
         odugrid.F90 \
         gridoperations.F90 \
         gridgeom.F90 \
         gridgeom_api.F90


#-------------------------------------------------------------------------------
#Libraries

