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
noinst_LTLIBRARIES = libKernelComputeSediment.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
        -I$(top_builddir)/utils_gpl/morphology/packages/morphology_data/src \
	-I$(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libKernelComputeSediment_la_SOURCES = \
         morstatistics.f90 \
         adjust_bedload.f90 \
         avalan.f90 \
         dredge.f90 \
         bndmorlyr.f90 \
         bott3d.f90 \
         compthick.f90 \
         dredgedump.f90 \
         dwnvel.f90 \
         erosed.f90 \
         red_soursin.f90 \
         shearx.f90 \
         z_bott3d.f90 \
         z_dwnvel.f90 \
         z_erosed.f90 \
         z_red_soursin.f90 \
         upwbed.f90 \
         upbdps.f90 \
         updwaqflxsed.f90

#-------------------------------------------------------------------------------
#Libraries

