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
noinst_LTLIBRARIES = libDelftIO.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir)/utils_lgpl/delftio/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libDelftIO_la_SOURCES = \
         d3d_sobek_conf.f90 \
         d3d_sobek.f90 \
         d3s_get_discharges.f90 \
         d3s_put_levels.f90 \
         d3s_setup.f90 \
         putget_dio.f90 \
         sync_couple.f90 \
         sync_flm.f90 \
         sync_flowwave.f90 \
         sync_rtcflow.f90

#-------------------------------------------------------------------------------
#Libraries

libDelftIO_la_LIBADD=\
	$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90/libDIOF90.la \
	$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90Nefis/libDIOF90Nefis.la \
	$(top_builddir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libDelftIOSHM.la

