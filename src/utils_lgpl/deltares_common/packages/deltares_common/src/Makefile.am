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

SUBDIRS = \
	rd_token

noinst_LTLIBRARIES = libDeltaresCommon.la

#-------------------------------------------------------------------------------
#Includes


#-------------------------------------------------------------------------------
#Sources
libDeltaresCommon_la_SOURCES = \
	iso_c_utils.F90 \
	precision_basics.F90 \
	precision.F90 \
	precision_sp.F90 \
	geometry_module.f90 \
	grid_dimens_module.f90 \
	message_module.f90 \
	MessageHandling.f90 \
	string_module.f90 \
	stringsequal.f90 \
	system_utils.F90 \
	mathconsts.f90 \
	tree_struct.f90 \
	array_module.f90 \
	buffer.f90 \
	bilin5.f90 \
	combinepaths.f90 \
	deltares_common_version.F90 \
	distance.f90 \
	time_module.f90 \
	juldat.f90 \
	handles.f90 \
	tables.f90 \
	table_handles.f90 \
	malloc.f90 \
	nan_check_module.f90 \
	newunit.f90 \
	properties.f90 \
	scannr.f90 \
	skipstarlines.f90 \
	small.f90 \
	timers.f \
	util.f90 \
	multi_file_io.f90 \
	wavenr.f90

# For source distributions: the tar.gz will contain the correct version numbers in the generated *version.F90
# So, just before packaging the .tar.gz dist, disable version number REgeneration parts of this Makefile.am.
dist-hook:
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.am
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.in

#-------------------------------------------------------------------------------
#Libraries

libDeltaresCommon_la_LIBADD=\
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libDeltaresCommonC.la \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/libRDToken.la

BUILT_SOURCES = ./always \
	./deltares_common_version.F90

REGENERATE_VN=yes

# (the space before ifeq is intentional to fool Automake, and leave this conditional to Make)
 ifeq ($(REGENERATE_VN),yes)
# We're in a working copy build dir, so always update the *version.F90
./always:
	rm -rf ./deltares_common_version.F90

./deltares_common_version.F90: deltares_common_version.F90.svn ./always
	$(top_builddir)/scripts_lgpl/linux/update_version.sh $(builddir)/deltares_common_version.F90 $(builddir) ../include/version_number.ini $(top_builddir)
 else
# Do not force regeneration of *version.F90, because it is already shipped in a source distribution archive.
./always:

./deltares_common_version.F90:

 endif

.PHONY: ./always
