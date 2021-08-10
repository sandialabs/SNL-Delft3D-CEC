include $(top_srcdir)/common.am
bin_PROGRAMS = test_dflowfm_kernel

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ftnunit/packages/ftnunit/src \
	-I$(top_builddir)/utils_gpl/flow1d/packages/flow1d_core/src \
	-I$(top_builddir)/utils_gpl/flow1d/packages/flow1d_io/src \
	-I$(top_builddir)/engines_gpl/dflowfm/packages/dflowfm_kernel/src

#-------------------------------------------------------------------------------
#Sources 
test_dflowfm_kernel_SOURCES = \
	test_observations.f90 \
	test_cross_section.f90 \
	test_roughness.f90 \
	test_observ_Cross_Sections.f90 \
	test_iniField_1dField.f90 \
	test_MDU_File_Version.f90 \
   test_storage_nodes.f90 \
	test_dflowfm_kernel_f.f90

#-------------------------------------------------------------------------------
#Libraries

EXTRALIBS=\
        $(top_builddir)/utils_gpl/flow1d/packages/flow1d/src/libflow1d.la \
        $(top_builddir)/utils_gpl/morphology/packages/morphology_plugins_c/src/libMorphologyPluginsC.la \
        $(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src/libMorphologyKernel.la \
        $(top_builddir)/utils_gpl/morphology/packages/morphology_io/src/libMorphologyIO.la \
        $(top_builddir)/utils_gpl/morphology/packages/morphology_data/src/libMorphologyData.la \
        $(top_builddir)/utils_gpl/trachytopes/packages/trachytopes_kernel/src/libTrachytopesKernel.la \
        $(top_builddir)/utils_gpl/trachytopes/packages/trachytopes_io/src/libTrachytopesIO.la \
        $(top_builddir)/engines_gpl/waq/packages/waq_process/src/proc_preprocess/libproc_preprocess.la \
        $(top_builddir)/engines_gpl/waq/packages/waq_process/src/bloom/libbloom.la \
        $(top_builddir)/engines_gpl/waq/packages/waq_process/src/waq_process/libwaq_process.la \
        $(top_builddir)/engines_gpl/waq/packages/waq_plugin_wasteload/src/libwaq_plugin_wasteload.la \
        $(top_builddir)/engines_gpl/waq/packages/waq_utils_c/src/libwaq_utils_c.la \
        $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
        $(top_builddir)/engines_gpl/waq/packages/wq_processes/src/libwqprocesses.la \
        $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
        $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
        $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src/libDeltaresCommonMpi.la \
        $(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src/libkdtreewrapper.la \
        $(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/libECModule.la \
        $(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src/libIONetcdf.la \
        $(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src/libgridgeom.la \
        $(top_builddir)/utils_lgpl/metistools/packages/metisoptions/src/libmetisoptions.la \
        $(top_builddir)/third_party_open/triangle/libtriangle_c.la \
        $(top_builddir)/third_party/interacter_stub/src/libintstub.la \
        $(top_builddir)/third_party_open/kdtree2/src-f90/libkdtree2f.la \
        $(top_builddir)/third_party_open/polypack/src/libpolypack.la \
        $(top_builddir)/third_party_open/libsigwatch/libsigwatch.la \
        $(CCLIBS) -luuid

test_dflowfm_kernel_LDADD = \
	$(top_builddir)/engines_gpl/dflowfm/packages/dflowfm_kernel/src/libdflowfm.la \
	$(top_builddir)/utils_lgpl/ftnunit/packages/ftnunit/src/libFtnUnit.la \
	$(EXTRALIBS)
