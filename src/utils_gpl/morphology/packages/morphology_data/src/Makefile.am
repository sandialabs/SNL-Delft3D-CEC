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
noinst_LTLIBRARIES = libMorphologyData.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libMorphologyData_la_SOURCES = \
         sediment_basics_module.f90 \
         morphology_data_module.f90

#-------------------------------------------------------------------------------
#Libraries

