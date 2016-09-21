#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   26 aug 10
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = delwaq2

AM_FCFLAGS += $(OPENMP_FCFLAGS)
AM_FFLAGS += $(OPENMP_FFLAGS)

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_kernel \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
        -I$(top_builddir)/engines_gpl/waq/packages/delwaq2_lib/src

#-------------------------------------------------------------------------------
#Sources
delwaq2_SOURCES = block.f \
       getidentification.f \
       dlwq2.f

#-------------------------------------------------------------------------------
# Libraries
delwaq2_LDADD = \
    $(top_builddir)/engines_gpl/waq/packages/delwaq2_lib/src/libDelwaq2.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/libRDToken.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
    $(top_builddir)/engines_gpl/waq/packages/delwaq2_version_number/src/libdelwaq2_version_number.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_kernel/libWaqKernel.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_process/libwaq_process.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_plugin_wasteload/src/libwaq_plugin_wasteload.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/waq_io/libwaq_io.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/proc_preprocess/libproc_preprocess.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/bloom/libbloom.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/charon/libcharon.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/waq_io/libwaq_io.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_data/src/proc_data/libproc_data.la \
    $(top_builddir)/engines_gpl/part/packages/kernel_f/src/libkernel_f.la \
    $(top_builddir)/engines_gpl/part/packages/io_f/src/libio_f.la \
    $(top_builddir)/engines_gpl/part/packages/utils_f/src/libutils_f.la \
    $(top_builddir)/engines_gpl/part/packages/data_f/src/libdata_f.la \
    $(top_builddir)/engines_gpl/part/packages/version_number/src/libpart_version_number.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/libmod_couplib.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings/libmod_timings.la \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90/libDIOF90.la \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libDelftIOSHM.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/ods/libods.la \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/interact/libinteract.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_c/src/libwaq_utils_c.la \
    $(top_builddir)/engines_gpl/waq/packages/delwaq2_version_number/src/libdelwaq2_version_number.la \
    -lpthread \
    -ldl

delwaq2_LDFLAGS = -export-dynamic 

# To be added when using Intel 11:
#    $(FCLIBS)
