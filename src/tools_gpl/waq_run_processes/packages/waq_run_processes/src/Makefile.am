#-------------------------------------------------------------------------------
#   Makefile for run_processes
#
#   arjen.markus@deltares.nl
#   26 aug 10
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = run_processes
 
#-------------------------------------------------------------------------------
#Includes
#FMODULES +=

MPILIBS += $(MPILIBS_ADDITIONAL)

#-------------------------------------------------------------------------------
#Sources
run_processes_SOURCES = run_processes.f90

#-------------------------------------------------------------------------------
# Libraries

run_processes_LDADD = \
    $(top_srcdir)/tools/waq_run_processes/packages/waq_run_processes_version_number/src/libwaq_run_processes_version_number.la \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/waq_kernel/libwaq_kernel.la \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/waq_process/libwaq_process.la \
    $(top_srcdir)/engines/waq/packages/waq_plugin_wasteload/src/libwaq_plugin_wasteload.la \
    $(top_srcdir)/engines/waq/packages/waq_io/src/waq_io/libwaq_io.la \
    $(top_srcdir)/engines/waq/packages/waq_io/src/proc_preprocess/libproc_preprocess.la \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/bloom/libbloom.la \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/charon/libcharon.la \
    $(top_srcdir)/engines/waq/packages/waq_io/src/waq_io/libwaq_io.la \
    $(top_srcdir)/engines/waq/packages/waq_data/src/proc_data/libproc_data.la \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/mod_couplib/libmod_couplib.la \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/mod_timings/libmod_timings.la \
	$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90/libDIOF90.la \
	$(top_srcdir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libDelftIOSHM.la \
	$(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la \
	$(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libFSMC.la \
	$(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libESMC.la \
	$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync/libDelftIO.la \
	$(top_srcdir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
	$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
	$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libDeltaresCommonC.la \
    $(top_srcdir)/engines/waq/packages/waq_io/src/ods/libods.la \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/interact/libinteract.la \
    $(top_srcdir)/engines/waq/packages/waq_utils_c/src/libwaq_utils_c.la \
    $(top_srcdir)/engines/waq/packages/delwaq2_version_number/src/libdelwaq2_version_number.la \
    $(top_srcdir)/third_party/wlauth/bin/linux/libWL_auth.a \
    $(MPILIBS)  \
    -lpthread

# To be added when using Intel 11:
#    $(FCLIBS)
