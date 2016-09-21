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
bin_PROGRAMS = test_01

#-------------------------------------------------------------------------------
#Sources
test_01_SOURCES = \
         test_01.c

#-------------------------------------------------------------------------------
#Libraries

test_01_LDADD = \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la

