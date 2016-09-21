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
noinst_LTLIBRARIES = libmanager.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/wave/packages/data/src \
	-I$(top_builddir)/engines_gpl/wave/packages/kernel/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo

#-------------------------------------------------------------------------------
#Sources
libmanager_la_SOURCES = \
         swan_tot.f90

#-------------------------------------------------------------------------------
#Libraries

