#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@wldelft.nl
#   Adri.Mourits@wldelft.nl
#   Fedor.Baart@wldelft.nl
#   26 aug 10
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libDataParallelMPI.la

#-------------------------------------------------------------------------------
#Includes
# TODO:/opt/mpich2/include should be configured....
FINCLUDES += \
        -I$(top_builddir)/utils_lgpl/esmfsm/include \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include \
        -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
	-I$(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src

#-------------------------------------------------------------------------------
#Sources
libDataParallelMPI_la_SOURCES = \
	 initdfparall.F90 \
	 dfinitmpi.F90 \
	 dfsync.F90 \
	 dfexitmpi.F90 \
	 dfbroadc_gdp.f90 \
	 dfsendd_nm_pos1.F90 \
	 dfsendd_nm_pos2.F90 \
	 dfsendi_nm_pos1.F90 \
	 dfsendi_nm_pos2.F90 \
	 dfsendr_nm_pos1.F90 \
	 dfsendr_nm_pos2.F90 \
	 dfupdd_nm_pos1.F90 \
	 dfupdd_nm_pos2.F90 \
	 dfupdi_nm_pos1.F90 \
	 dfupdi_nm_pos2.F90 \
	 dfupdr_nm_pos1.F90 \
	 dfupdr_nm_pos2.F90 \
	 dfwaitd_nm_pos1.F90 \
	 dfwaitd_nm_pos2.F90 \
	 dfwaiti_nm_pos1.F90 \
	 dfwaiti_nm_pos2.F90 \
	 dfwaitr_nm_pos1.F90 \
	 dfwaitr_nm_pos2.F90 \
	 dfsendnb.F90 \
	 dfrecvnb.F90 \
	 dfreduce_gdp.f90 \
	 dfexchg.F90 \
	 dfdecomp.F90 \
	 dfpartit.F90 \
	 dfbladm.F90 \
	 dforb.F90 \
	 dfupdgeo.F90 \
	 dfstrip.F90 \
	 dfmassc.F90 \
     dfgather_module.F90 \
     dfscatter_module.F90 \
     dffunctionals.F90 \
	 dfgather_lowlevel.F90 \
	 dfgather_grddim.F90 \
     dfscatter_lowlevel.F90 \
	 adjlin.F90 \
	 rdencl.F90 \
	 removeDuplicateDDBFiles.f90 \
	 strgrd.F90

#-------------------------------------------------------------------------------
#Libraries

