#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = allocate_local

#-------------------------------------------------------------------------------
#Sources
allocate_local_SOURCES = \
         allocate.cpp

#-------------------------------------------------------------------------------
#Libraries
allocate_local_LDADD = \
         $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la

