#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libio_f.la

AM_FCFLAGS += -fopenmp $(OPENMP_FCFLAGS)
AM_FFLAGS += -fopenmp $(OPENMP_FFLAGS)

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I../../data_f/src \
	-I../../utils_f/src \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libio_f_la_SOURCES = \
         delete_file.f90 \
         dd_prepare.f90 \
         genfil.f90 \
         putget.f90 \
         filldm.f90 \
         dlpr12.f90 \
         dlwqbl.f90 \
         parttd.f90 \
         dlwqfl.f90 \
         filtyp.F90 \
         m_sferic.f90 \
         meteo1temphelpers.f90 \
         meteo1.f90 \
         rdwnd.f90 \
         openfl.f90 \
         getdps.f90 \
         part12.f90 \
         part13.f90 \
         part16.f90 \
         parths.f90 \
         rdccol.f90 \
         rdfnam.f90 \
         rdhyd.f90 \
         rdhydr.f90 \
         rdlgri.f90 \
         rdpart.f90 \
         report.f90 \
         report_date_time.f90 \
         writrk.f90 \
         wrttrk.f90

#-------------------------------------------------------------------------------
#Libraries
