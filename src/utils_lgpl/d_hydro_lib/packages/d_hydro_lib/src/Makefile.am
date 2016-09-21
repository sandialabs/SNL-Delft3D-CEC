#-------------------------------------------------------------------------------
#   Makefile for d_hydro_lib
#
#   Irv.Elshoff@deltares.nl
#   2 jun 11
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

noinst_LTLIBRARIES = libDHydroLib.la

#-------------------------------------------------------------------------------
#Includes
AM_CPPFLAGS = \
        -I$(srcdir)/../include \
        -I$(srcdir)/../../../include

#-------------------------------------------------------------------------------
#Sources
libDHydroLib_la_SOURCES = \
    coredump.cpp \
    clock.cpp \
    component.cpp \
    d_hydro_lib_version.cpp \
    exception.cpp \
    log.cpp \
    stringutils.cpp \
    xmltree.cpp

BUILT_SOURCES = \
	$(srcdir)/../include/d_hydro_lib_version.h


#-------------------------------------------------------------------------------
#Libraries
