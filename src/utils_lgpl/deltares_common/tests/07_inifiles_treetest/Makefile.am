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
bin_PROGRAMS = 02_treetest

#-------------------------------------------------------------------------------
#Sources
02_treetest_SOURCES = \
         main.c \
         treetest.f90

#-------------------------------------------------------------------------------
#Libraries

02_treetest_LDADD = \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/libDeltaresCommon.la
