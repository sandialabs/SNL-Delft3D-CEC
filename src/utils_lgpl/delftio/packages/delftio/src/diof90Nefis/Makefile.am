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
noinst_LTLIBRARIES = libDIOF90Nefis.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir)/utils_lgpl/delftio/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90

#-------------------------------------------------------------------------------
#Sources
libDIOF90Nefis_la_SOURCES = \
         dio-2dfield-nefis.F90 \
         dio-3d-block-nefis.F90 \
         dio-check-nefis-error.F90 \
         dio-const-rw.F90 \
         dio-plt-nefis.F90

#-------------------------------------------------------------------------------
#Libraries
