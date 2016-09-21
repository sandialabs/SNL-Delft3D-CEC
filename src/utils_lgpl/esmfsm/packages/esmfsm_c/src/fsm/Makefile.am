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
noinst_LTLIBRARIES = libFSMC.la

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I$(top_srcdir)/utils_lgpl/esmfsm/include \
           -I$(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/include

#-------------------------------------------------------------------------------
#Sources
libFSMC_la_SOURCES = \
         fsm.cpp

#-------------------------------------------------------------------------------
#Libraries

