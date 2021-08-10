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
noinst_LTLIBRARIES = libdata.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I. \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo \
	-I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
	-I$(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

AM_FCFLAGS+=${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Sources
libdata_la_SOURCES = \
         checkmeteoresult_wave.f90 \
         datetime_to_string.f90 \
         nc_check_err.f90 \
         nc_def_var.f90 \
         wave_data.f90 \
         rdsec.f90 \
         read_grids.f90 \
         swan_input.f90 \
         filldm.f90 \
         hunt.f90 \
         indexx.f90 \
         ipon.f90 \
         mkmap.f90 \
         noextspaces.f90 \
         putgtc.f90 \
         putgti.f90 \
         putgtl.f90 \
         putgtr.f90 \
         skcoma.f90 \
         skcomc.f90 \
         sort.f90 \
         swan_flow_grid_maps.f90 \
         timdat.f90 \
         flow_data.f90 \
         wavestop.f90

#-------------------------------------------------------------------------------
#Libraries

# Manualy add the netcdff (fortran dll), bug in pkg-config file is not fixed until in NetCDF 4.4 HEAD
LIBS+=${NETCDF_LIBS} ${NETCDF_FORTRAN_LIBS} -lnetcdff

