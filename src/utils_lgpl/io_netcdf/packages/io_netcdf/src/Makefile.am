#-------------------------------------------------------------------------------
#   Makefile io_netcdf
#
#   14 jun 2016
#
#   Copyright (C)  Stichting Deltares, 2011-2016.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libIONetcdf.la

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
io_netcdf_version.F90: io_netcdf_version.F90.svn .FORCE
	$(top_builddir)/scripts_lgpl/linux/update_version.sh \
	$(builddir)/$@ $(srcdir) $(srcdir)/../include/version_number.ini $(abs_top_builddir)
 else
# Do not force regeneration of *version.F90, because it is already shipped in a source distribution archive.
io_netcdf_version.F90:
 endif

.FORCE: 

.PHONY: .FORCE

#-------------------------------------------------------------------------------
#Includes
AM_FCFLAGS += $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)

FMODULES += \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libIONetcdf_la_SOURCES = \
         io_netcdf_version.F90 \
         netcdf_utils.F90 \
         coordinate_reference_system.F90 \
         meshdata.F90 \
         io_ugrid.F90 \
         io_netcdf.f90
         
#-------------------------------------------------------------------------------
#Libraries

