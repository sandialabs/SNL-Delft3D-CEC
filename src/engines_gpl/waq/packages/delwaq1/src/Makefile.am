#-------------------------------------------------------------------------------
#   Makefile for delwaq 1
#
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = delwaq1

AM_FCFLAGS += -fopenmp $(OPENMP_FCFLAGS)
AM_FFLAGS += -fopenmp $(OPENMP_FFLAGS)
LDFLAGS += -fopenmp

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_kernel/src \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
        -I$(top_builddir)/engines_gpl/part/packages/data_f/src \
        -I$(top_builddir)/engines_gpl/part/packages/io_f/src \
        -I$(top_builddir)/engines_gpl/part/packages/utils_f/src \
        -I$(top_builddir)/engines_gpl/part/packages/kernel_f/src \
        -I$(top_builddir)/engines_gpl/waq/packages/delwaq/src \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src


#-------------------------------------------------------------------------------
#Sources
delwaq1_SOURCES = \
       delwaq1_version.F90 \
       dlwq1.f

#-------------------------------------------------------------------------------
# Libraries
delwaq1_LDADD = \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/libRDToken.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libDelftIOSHM.la \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90/libDIOF90.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings/libmod_timings.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/libmod_couplib.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_c/src/libwaq_utils_c.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_plugin_wasteload/src/libwaq_plugin_wasteload.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/libWaqKernel.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/waq_io/libwaq_io.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/ods/libods.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_process/src/proc_preprocess/libproc_preprocess.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_process/src/waq_process/libwaq_process.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_process/src/bloom/libbloom.la \
    $(top_builddir)/engines_gpl/waq/packages/delwaq/src/libdelwaq.la \
    $(top_builddir)/engines_gpl/part/packages/utils_f/src/libutils_f.la \
    $(top_builddir)/engines_gpl/part/packages/kernel_f/src/libkernel_f.la \
    $(top_builddir)/engines_gpl/part/packages/io_f/src/libio_f.la \
    $(top_builddir)/engines_gpl/part/packages/data_f/src/libdata_f.la \
    -lpthread  -luuid \
    -ldl

delwaq1_LDFLAGS = -export-dynamic 

# To be added when using Intel 11:
#    $(FCLIBS)

BUILT_SOURCES = ./always \
	./delwaq1_version.F90

./always:
	rm -rf ./delwaq1_version.F90

./delwaq1_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/delwaq1_version.F90 $(srcdir) $(top_srcdir)/engines_gpl/waq/version/version_number.ini $(top_builddir)
