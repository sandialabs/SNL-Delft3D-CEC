#-------------------------------------------------------------------------------
#   Makefile for ec-module
#
#   Irv Elshoff
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   Edwin.Spee@deltares.nl
#   9 nov 2009
#
#   Copyright (C)  Stichting Deltares, 2011-2018.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

SUBDIRS = \
    meteo

noinst_LTLIBRARIES = libECModule.la

# For source distributions: the tar.gz will contain the correct version numbers in the generated *version.F90
# So, just before packaging the .tar.gz dist, disable version number Regeneration parts of this Makefile.am.
dist-hook:
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.am
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.in


#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/third_party_open/kdtree2/src-f90 \
	-I$(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src

#-------------------------------------------------------------------------------
#   Fortran 90 flags

AM_FCFLAGS += \
    $(FINCLUDES) \
    $(FMODULES)

if USENETCDF
AM_FCFLAGS+=${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}
# Manualy add the netcdff (fortran dll), bug in pkg-config file is not fixed until in NetCDF 4.4 HEAD
LIBS+=${NETCDF_LIBS} ${NETCDF_FORTRAN_LIBS} -lnetcdff
endif

#-------------------------------------------------------------------------------
#Sources
libECModule_la_SOURCES = \
         transform_shiftedpole.f90 \
         ec_module_version.F90 \
         ec_parameters.F90 \
         ec_typedefs.f90 \
         ec_message.f90 \
         ec_alloc.f90 \
         ec_support.f90 \
         ec_netcdf_timeseries.f90 \
         ec_bcreader.f90 \
         ec_field.f90 \
         ec_quantity.f90 \
         ec_elementset.f90 \
         ec_spatial_extrapolation.f90 \
         ec_basic_interpolation.f90 \
         ec_converter.f90 \
         ec_astro.f90 \
         ec_filereader_read.F90 \
         ec_filereader.F90 \
         ec_item.f90 \
         ec_connection.f90 \
         ec_instance.f90 \
         ec_provider.F90 \
         ec_bccollect.f90 \
         ec_module.f90


# Version number generation (only when in SVN source dir, not for tar.gz dist)

REGENERATE_VN=yes

# (the space before ifeq is intentional to fool Automake, and leave this conditional to Make)
 ifeq ($(REGENERATE_VN),yes)
# We're in a working copy build dir, so always update the *version.F90
ec_module_version.F90: ec_module_version.F90.svn .FORCE
	$(top_builddir)/scripts_lgpl/linux/update_version.sh \
	$(builddir)/$@ $(srcdir) $(srcdir)/../include/version_number.ini $(abs_top_builddir)
 else
# Do not force regeneration of *version.F90, because it is already shipped in a source distribution archive.
ec_module_version.F90:
 endif

.FORCE: 

.PHONY: .FORCE

#-------------------------------------------------------------------------------
#Libraries

libECModule_la_LIBADD=\
    $(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo/libMeteo.la

