#-------------------------------------------------------------------------------
#   Makefile for d_hydro - Flow2D3D Component
#
#   Irv.Elshoff@Deltares.NL
#   31 may 11
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

noinst_LTLIBRARIES = libDDIterators.la

libDDIterators_la_SOURCES = \
    dredgemerge.cpp \
    gawsbarrier.cpp \
    initbarrier.cpp \
    minimumbarrier.cpp \
    rtccomm.cpp

INCLUDES = \
    $(STANDARD_C_INCLUDES) \
    -I../../../include
