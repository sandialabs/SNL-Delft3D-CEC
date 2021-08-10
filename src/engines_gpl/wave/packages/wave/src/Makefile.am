#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = wave
lib_LTLIBRARIES = libwave.la

FCLIBS += \
	-lpthread

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/engines_gpl/wave/packages/wave/include

FMODULES += \
	-I$(top_builddir)/engines_gpl/wave/packages/data/src \
	-I$(top_builddir)/engines_gpl/wave/packages/manager/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo \
	-I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync

AM_CPPFLAGS = \
    $(STANDARD_C_INCLUDES) \
    -I../include

#-------------------------------------------------------------------------------
#Sources
wave_SOURCES = \
         wave_exe.f90

libwave_la_SOURCES = \
         wave_bmi.f90

#-------------------------------------------------------------------------------
#Libraries

EXTRALIBS = \
$(top_builddir)/engines_gpl/wave/packages/manager/src/libmanager.la \
$(top_builddir)/engines_gpl/wave/packages/kernel/src/libkernel.la \
$(top_builddir)/engines_gpl/wave/packages/io/src/libio.la \
$(top_builddir)/engines_gpl/wave/packages/data/src/libdata.la \
$(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src/libgridgeom.la \
$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync/libDelftIO.la \
$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la \
$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/libECModule.la \
$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src/libIONetcdf.la \
$(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
$(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src/libkdtreewrapper.la \
$(top_builddir)/third_party_open/kdtree2/src-f90/libkdtree2f.la \
$(top_builddir)/third_party_open/triangle/libtriangle_c.la \
$(FCLIBS) $(CCLIBS)

#$(EXTERNALSOURCES)

wave_LDADD   = \
        $(EXTRALIBS)

libwave_la_LIBADD   = \
        $(EXTRALIBS)

