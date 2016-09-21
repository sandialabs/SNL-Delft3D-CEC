#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@wldelft.nl
#   Adri.Mourits@wldelft.nl
#   Fedor.Baart@wldelft.nl
#   11 jan 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libDataGeneral.la

#-------------------------------------------------------------------------------
#Includes
AM_FCFLAGS += $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)

FMODULES += \
        -I$(NETCDFROOT)/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libDataGeneral_la_SOURCES = \
         griddims.f90 \
         griddims_admin.f90 \
         nc_check_err.f90 \
         nc_def_var.f90

#-------------------------------------------------------------------------------
#Libraries

