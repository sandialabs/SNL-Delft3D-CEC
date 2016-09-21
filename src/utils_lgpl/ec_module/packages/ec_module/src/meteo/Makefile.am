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
noinst_LTLIBRARIES = libMeteo.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libMeteo_la_SOURCES = \
         meteo_data.f90 \
         distance2.f90 \
         meteo_read.f90 \
         meteo.f90 \
         checkmeteoheader.f90 \
         numbersonline_m.f90 \
         openexistingfile_meteo.f90 \
         readmeteoheader.f90

#-------------------------------------------------------------------------------
#Libraries
