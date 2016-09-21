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
noinst_LTLIBRARIES = libESMFSM.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libESMFSM_la_SOURCES = \
         fortapi.f90

#-------------------------------------------------------------------------------
#Libraries

libESMFSM_la_LIBADD = \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libESMC.la \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libFSMC.la \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libESMFSMVersionNumber.la

