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
noinst_LTLIBRARIES = libmanager.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/wave/packages/data/src \
    -I$(top_builddir)/engines_gpl/wave/packages/kernel/src \
    -I$(top_builddir)/engines_gpl/wave/packages/io/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo

#-------------------------------------------------------------------------------
#Sources
libmanager_la_SOURCES = \
         checklicense.f90 \
         swan_tot.f90 \
         usage.f90 \
         wave_main.f90 \
         wave_version.F90

#-------------------------------------------------------------------------------
#Libraries

BUILT_SOURCES = ./always \
	./wave_version.F90

./always:
	rm -rf ./wave_version.F90

./wave_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/wave_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)

