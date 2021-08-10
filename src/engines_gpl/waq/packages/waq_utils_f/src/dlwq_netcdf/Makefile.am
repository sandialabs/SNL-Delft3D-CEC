#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libdlwq_netcdf.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils

AM_FCFLAGS += -DNetCDF4 $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Sources
libdlwq_netcdf_la_SOURCES = \
       dlwq_netcdf.F90

#-------------------------------------------------------------------------------
# Libraries
