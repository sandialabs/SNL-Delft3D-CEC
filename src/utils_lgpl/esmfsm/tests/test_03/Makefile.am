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
bin_PROGRAMS = test_03

#-------------------------------------------------------------------------------
#Sources
test_03_SOURCES = \
         chnull.f90 \
         inull.f90 \
         rnull.f90 \
         test_03.cpp \
         trisimtest.f90

#-------------------------------------------------------------------------------
#Libraries

test_03_LDADD = \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la

