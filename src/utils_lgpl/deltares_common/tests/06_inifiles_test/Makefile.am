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
bin_PROGRAMS = 01_initest

#-------------------------------------------------------------------------------
#Sources
01_initest_SOURCES = \
         initest.f90 \
         main.c

#-------------------------------------------------------------------------------
#Libraries

01_initest_LDADD = \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/libDeltaresCommon.la
