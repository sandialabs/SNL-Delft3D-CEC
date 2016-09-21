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
noinst_LTLIBRARIES = libKernelDD.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libKernelDD_la_SOURCES = \
         caldpu_dd.f90 \
         chkdry_dd.f90 \
         chkrefinement.f90 \
         inigeo_dd.f90 \
         trakad_dd.f90 \
         upwhu_dd.f90 \
         wangp1.f90 \
         wangp3.f90 \
         xzyz_dd.f90

#-------------------------------------------------------------------------------
#Libraries

