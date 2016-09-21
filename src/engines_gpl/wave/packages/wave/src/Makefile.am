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

bin_PROGRAMS = wave.exe

FCLIBS += \
	-lpthread

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/engines_gpl/wave/packages/wave/include

FMODULES += \
	-I$(top_builddir)/engines_gpl/wave/packages/data/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync

#-------------------------------------------------------------------------------
#Sources
wave_exe_SOURCES = \
         checklicense.f90 \
         usage.f90 \
         wave_version.F90 \
         wave.f90

#-------------------------------------------------------------------------------
#Libraries

wave_exe_LDADD   = \
$(top_builddir)/engines_gpl/wave/packages/manager/src/libmanager.la \
$(top_builddir)/engines_gpl/wave/packages/kernel/src/libkernel.la \
$(top_builddir)/engines_gpl/wave/packages/io/src/libio.la \
$(top_builddir)/engines_gpl/wave/packages/data/src/libdata.la \
$(top_builddir)/engines_gpl/wave/packages/io/src/libio.la \
$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync/libDelftIO.la \
$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la \
$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/libECModule.la \
$(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
$(EXTERNALSOURCES) \
$(FCLIBS) $(CCLIBS)


BUILT_SOURCES = ./always \
	./wave_version.F90

./always:
	rm -rf ./wave_version.F90

./wave_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/wave_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)
