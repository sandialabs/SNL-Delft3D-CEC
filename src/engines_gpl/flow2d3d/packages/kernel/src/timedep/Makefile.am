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
noinst_LTLIBRARIES = libKernelTimedep.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/inichk

#-------------------------------------------------------------------------------
#Sources
libKernelTimedep_la_SOURCES = \
         cofrbc.f90 \
         eoferr.f90 \
         incbc.f90 \
         incbcc.f90 \
         incdis.f90 \
         inceva.f90 \
         incmeteo.f90 \
         incrbc.f90 \
         incsdu.f90 \
         inctem.f90 \
         incwav.f90 \
         rtc_comm_get.f90 \
         rtc_comm_init.f90 \
         rtc_comm_put.f90 \
         update_nodal_factors.f90 \
         updbcc.f90 \
         updbcq.f90 \
         updbct.f90 \
         upddis.f90 \
         updveg3d.f90 \
         updeva.f90 \
         updtem.f90 \
         wetdis.f90 \
         windtogridc.f90 \
         windtostress.f90

#-------------------------------------------------------------------------------
#Libraries

