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
noinst_LTLIBRARIES = libMorphologyWaq.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
        -I$(top_builddir)/utils_gpl/morphology/packages/morphology_data/src

#-------------------------------------------------------------------------------
#Sources
libMorphologyWaq_la_SOURCES = \
         waqsim_module.F90 \
         waqmor_module.f90 \
         wrwaq.F90

#-------------------------------------------------------------------------------
#Libraries
