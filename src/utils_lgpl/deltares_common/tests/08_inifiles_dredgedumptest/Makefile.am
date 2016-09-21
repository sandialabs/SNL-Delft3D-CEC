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
bin_PROGRAMS = 03_dredgedumptest

#-------------------------------------------------------------------------------
#Sources
03_dredgedumptest_SOURCES = \
         dredgedumptest.f90 \
         main.c

#-------------------------------------------------------------------------------
#Libraries

03_dredgedumptest_LDADD = \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/libDeltaresCommon.la
