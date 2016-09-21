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
noinst_LTLIBRARIES = libMorphologyKernel.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_gpl/morphology/packages/morphology_data/src

#-------------------------------------------------------------------------------
#Sources
libMorphologyKernel_la_SOURCES = \
         bedcomposition_module.f90 \
         bailtr.f90 \
         bedbc1993.f90 \
         bedbc2004.f90 \
         bedtr1993.f90 \
         bedtr2004.f90 \
         calseddf1993.f90 \
         calseddf2004.f90 \
         compdiam.f90 \
         comphidexp.f90 \
         compbsskin.f90 \
         compsandfrac.f90 \
         eqsettle.f90 \
         eqtran.f90 \
         erosilt.f90 \
         factor3d2d.f90 \
         fgyint.f90 \
         fluff_burial.f90 \
         getfixfac.f90 \
         initsedtra.f90 \
         osmom.f90 \
         sand_mud.f90 \
         shld.f90 \
         soursin_2d.f90 \
         soursin_3d.f90 \
         termfy.f90 \
         termgy.f90 \
         trab11.f90 \
         trab12.f90 \
         trab14.f90 \
         trabg.f90 \
         trabwc.f90 \
         trabwc2.f90 \
         tram1.f90 \
         tram2.f90 \
         tran9t.f90 \
         tranb1.f90 \
         tranb2.f90 \
         tranb3.f90 \
         tranb4.f90 \
         tranb5.f90 \
         tranb7.f90 \
         updmorfac.f90 \
         wave.f90

#-------------------------------------------------------------------------------
#Libraries

