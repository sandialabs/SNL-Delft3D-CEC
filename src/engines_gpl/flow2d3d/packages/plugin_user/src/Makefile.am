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
noinst_LTLIBRARIES = libPluginUser.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include

FMODULES += \
        -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libPluginUser_la_SOURCES = \
         u_ppr.f90 \
         u_rdat.f90 \
         u_wdro.f90 \
         u_whis.f90 \
         u_wmap.f90 \
         uckbcc.f90 \
         urdbcc.f90 \
         urdrgs.f90 \
         usrdef.f90 \
         usrptr.f90

#-------------------------------------------------------------------------------
#Libraries

