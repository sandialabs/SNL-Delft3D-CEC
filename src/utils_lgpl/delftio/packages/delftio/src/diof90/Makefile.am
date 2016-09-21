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
noinst_LTLIBRARIES = libDIOF90.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir)/utils_lgpl/delftio/include

#-------------------------------------------------------------------------------
#Sources
libDIOF90_la_SOURCES = \
         delftio_version.F90 \
         dio_shm_f2c_f.F90 \
         dio-3d-block.F90 \
         dio-prop.F90 \
         dio-ds-config.F90 \
         dio-streams.F90 \
         dio-ds.F90 \
         dio-2dfield-rw.F90 \
         dio-2dfield-shm.F90 \
         dio-ini.F90 \
         dio-plt-rw.F90 \
         dio-plt-f77.F90 \
         dio-plt-shm.F90 \
         dio-sync-support.F90 \
         dio-sync.F90 \
         dio-time-support.F90 \
         open_mi_dio.f90

EXTRA_DIST = \
	delftio_version.F90.svn

BUILT_SOURCES = ./always \
	./delftio_version.F90

./always:
	rm -rf ./delftio_version.F90

./delftio_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/delftio_version.F90 $(srcdir) ../../include/version_number.ini $(top_builddir)


#-------------------------------------------------------------------------------
#Libraries
