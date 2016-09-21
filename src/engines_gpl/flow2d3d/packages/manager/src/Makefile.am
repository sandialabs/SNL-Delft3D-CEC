#-------------------------------------------------------------------------------
#   Makefile for d_hydro - Flow2D3D Component
#
#   Adri.Mourits@Deltares.NL
#   Fedor.Baart@Deltares.NL
#   Irv.Elshoff@Deltares.NL
#   17 nov 11
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

noinst_LTLIBRARIES = libFlow2d3dManager.la

#-------------------------------------------------------------------------------
#   Includes

FINCLUDES += \
    -I$(top_builddir)/utils_lgpl/esmfsm/include \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/include

FMODULES += \
    -I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
    -I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
    -I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
    -I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/src \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/inichk

#-------------------------------------------------------------------------------
#   Sources

libFlow2d3dManager_la_SOURCES = \
     tdatmain.F90 \
     tricom_init.F90 \
     tricom_step.F90 \
     tricom_finish.F90 \
     triend.F90 \
     tripoi.F90 \
     trisim_mod.F90 \
     trisim.F90

#-------------------------------------------------------------------------------
#   Libraries
