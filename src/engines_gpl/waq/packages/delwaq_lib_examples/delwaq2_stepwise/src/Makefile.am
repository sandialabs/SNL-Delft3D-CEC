#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   26 aug 10
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = delwaq2
 
#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_kernel \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
        -I$(top_builddir)/engines_gpl/waq/packages/delftio/src/diof90

#-------------------------------------------------------------------------------
#Sources
delwaq2_SOURCES = block.f \
       dlwqmain_stepwise.f90 \
       ../../delwaq2/src/d_tr_d3d.f90

#-------------------------------------------------------------------------------
# Libraries

delwaq2_LDADD = \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_kernel/libwaq_kernel.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_process/libwaq_process.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_plugin_wasteload/src/libwaq_plugin_wasteload.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/waq_io/libwaq_io.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/proc_preprocess/libproc_preprocess.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/bloom/libbloom.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/charon/libcharon.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/waq_io/libwaq_io.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_data/src/proc_data/libproc_data.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/libmod_couplib.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings/libmod_timings.la \
    $(top_builddir)/utils/delftio/packages/delftio/src/diof90/libDIOF90.la \
    $(top_builddir)/utils/delftio/packages/delftio_shm/src/diof90/libDelftIOSHM.la \
    $(top_builddir)/utils/delftio/packages/delftio_version_number/src/libdelftio_version_number.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/ods/libods.la \
    $(top_builddir)/utils/nefis/packages/nefis/src/libnefis.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/interact/libinteract.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_c/src/libwaq_utils_c.la \
    $(top_builddir)/engines_gpl/waq/packages/delwaq2_version_number/src/libdelwaq2_version_number.la \
    $(top_builddir)/utils/nefis/packages/nefis_version_number/src/libnefis_version_number.la \
    $(top_builddir)/utils/esmfsm/packages/esmfsm/src/fsm/libfsm.la \
    $(top_builddir)/utils/esmfsm/packages/esmfsm_c/src/fsm/libfsm_c.la \
    $(top_builddir)/utils/esmfsm/packages/esmfsm_c/src/esm/libesm_c.la \
    $(top_builddir)/utils/esmfsm/packages/esmfsm_version_number/src/libesmfsm_version_number.la \
    -lpthread

# To be added when using Intel 11:
#    $(FCLIBS)
