#-------------------------------------------------------------------------------
#   Makefile for d_hydro - Flow2D3D Component
#
#   Irv.Elshoff@Deltares.NL
#   4 jul 12
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

SUBDIRS = \
    dd

lib_LTLIBRARIES = libflow2d3d.la

INCLUDES = \
    $(STANDARD_C_INCLUDES) \
    -I../include

MPILIBS += $(MPILIBS_ADDITIONAL)

#-------------------------------------------------------------------------------
#Sources
libflow2d3d_la_SOURCES = \
    flow2d3d.cpp \
    flowol.cpp \
    flow2d3d_version.cpp

#-------------------------------------------------------------------------------
#Libraries
libflow2d3d_la_LIBADD = \
    $(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/libDD.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/manager/src/libFlow2d3dManager.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/main/libKernelMain.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/non_hydro/libKernelNonHydro.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/taylor/libKernelTaylor.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/timedep/libKernelTimedep.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute/libKernelCompute.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/libKernelComputeNearfar.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute_roller/libKernelComputeRoller.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/libKernelComputeSediment.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel_dd_f/src/libKernelDDF.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io/src/preprocessor/libIOPreprocessor.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io/src/output/libIOOutput.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/inichk/libKernelInichk.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/dd/libKernelDD.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io/src/input/libIOInput.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/general/libKernelGeneral.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/src/libIODolF.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/plugin_user/src/libPluginUser.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/plugin_culvert_c/src/libPluginCulvertC.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/allocation/libDataAllocation.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/general/libDataGeneral.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/gdp/libDataGDP.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/libDataParallelMPI.la \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics/libDataBasics.la \
    $(top_builddir)/utils_gpl/morphology/packages/morphology_io/src/libMorphologyIO.la \
    $(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src/libMorphologyKernel.la \
    $(top_builddir)/utils_gpl/morphology/packages/morphology_plugins_c/src/libMorphologyPluginsC.la \
    $(top_builddir)/utils_gpl/morphology/packages/morphology_data/src/libMorphologyData.la \
    $(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/libECModule.la \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync/libDelftIO.la \
    $(top_builddir)/utils_lgpl/d_hydro_lib/packages/d_hydro_lib/src/libDHydroLib.la \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la \
    $(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src/libIONetcdf.la \
    $(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src/libIOHyd.la \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src/libDeltaresCommonMpi.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
    $(top_builddir)/utils_lgpl/delftonline/src/delftonline/libDelftOnline.la \
    $(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src/libkdtreewrapper.la \
    $(top_builddir)/third_party_open/kdtree2/src-f90/libkdtree2f.la \
    $(MPILIBS) \
    $(NETCDF_LIBS) $(NETCDF_FORTRAN_LIBS) -lnetcdff \
    $(FCLIBS) $(CCLIBS)

BUILT_SOURCES = \
	./always \
	$(srcdir)/../include/flow2d3d_version.h

./always:
	rm -f $(srcdir)/../include/flow2d3d_version.h

../include/flow2d3d_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/../include/flow2d3d_version.h $(srcdir) $(srcdir)/../include/version_number.ini $(top_builddir)
