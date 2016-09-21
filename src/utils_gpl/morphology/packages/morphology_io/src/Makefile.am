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
noinst_LTLIBRARIES = libMorphologyIO.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
        -I$(top_builddir)/utils_gpl/morphology/packages/morphology_data/src \
	-I$(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src

#-------------------------------------------------------------------------------
#Sources
libMorphologyIO_la_SOURCES = \
         rdtrafrm.f90 \
         rdsed.f90 \
         rdmorlyr.f90 \
         rdmor.f90 \
         rdstm.f90

#-------------------------------------------------------------------------------
#Libraries
 
