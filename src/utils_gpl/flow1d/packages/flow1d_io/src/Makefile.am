#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 sep 10
#
#   Copyright (C) 2007-2010, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libflow1d_io.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
  -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
  -I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
  -I$(top_builddir)/utils/flow1d/packages/flow1d_ec_temporary/src \
  -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
  -I$(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src \
  -I$(builddir)/../../flow1d_core/src \
  -I$(NETCDF_FORTRAN_LIBS)

if USENETCDF
AM_FCFLAGS+=${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}
# Manualy add the netcdff (fortran dll), bug in pkg-config file is not fixed until in NetCDF 4.4 HEAD
LIBS+=${NETCDF_LIBS} ${NETCDF_FORTRAN_LIBS} -lnetcdff
endif

#-------------------------------------------------------------------------------
#Sources
libflow1d_io_la_SOURCES = \
         hash_list_io.f90 \
         1d_networkreader.f90 \
         read_table.f90 \
		 readObservCrossSections.f90 \
         Readstructures.f90 \
         readObservationPoints.f90 \
         readStorageNodes.f90 \
         readSalinityParameters.f90 \
         readspatialdata.f90 \
		 read_roughness.f90 \
         readCrossSections.f90 \
         readModelParameters.f90 \
         flow1d_reader.F90 \
         flow1d_io.F90

# libflow1d_io is a convenience library, which will be linked into toplevel flow1d library.
# Do not include dependent flow1d_core library here already: done in toplevel lib.
libflow1d_io_la_LIBADD=
