#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libkernel_f.la

AM_FCFLAGS += -fopenmp $(OPENMP_FCFLAGS)
AM_FFLAGS += -fopenmp $(OPENMP_FFLAGS)

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/part/packages/data_f/src \
        -I$(top_builddir)/engines_gpl/part/packages/io_f/src \
        -I$(top_builddir)/engines_gpl/part/packages/utils_f/src \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libkernel_f_la_SOURCES = \
         vdisp.f90 \
         mudrel.f90 \
         extout.f90 \
         partfl.f90 \
         oildsp.f90 \
         umagi.f90 \
         p10cor.f90 \
         inipart_asc.f90 \
         inipart.f90 \
         densty.f90 \
         part01.f90 \
         part10.f90 \
         plotgr.f90 \
         part03.f90 \
         part21.f90 \
         part14.f90 \
         part06.f90 \
         part15.f90 \
         partur.f90 \
         part08.f90 \
         part17.f90 \
         partwq.f90 \
         part09.f90 \
         part18.f90 \
         partvs.f90 \
         p10ddb.f90 \
         diffus.f90 \
         delpar.F90

#-------------------------------------------------------------------------------
#Libraries
