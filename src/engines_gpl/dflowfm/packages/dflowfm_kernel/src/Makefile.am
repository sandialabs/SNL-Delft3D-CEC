lib_LTLIBRARIES=libdflowfm.la
#AM_CXXFLAGS = $(INTI_CFLAGS)

# Import optimization and OPENMP (in AM_FCFLAGS) from common.am
include $(top_srcdir)/common.am

INCLUDES = \
    -I$(builddir)/../include \
    -I$(builddir)/../../dflowfm_lib/include


# these are generated using:
# cd $(srcdir)/src
# $(top_srcdir)/scripts/generate.py modules.f90 network_data.f90 --template-dir=templates

EXTRA_DIST = \
	$(builddir)/../../dflowfm_lib/include/bmi_get_var_count.inc \
	$(builddir)/../../dflowfm_lib/include/bmi_get_var_name.inc \
	$(builddir)/../../dflowfm_lib/include/bmi_get_var.inc \
	$(builddir)/../../dflowfm_lib/include/bmi_get_var_location.inc \
	$(builddir)/../../dflowfm_lib/include/bmi_get_var_rank.inc \
	$(builddir)/../../dflowfm_lib/include/bmi_get_var_shape.inc \
	$(builddir)/../../dflowfm_lib/include/bmi_get_var_type.inc \
	$(builddir)/../../dflowfm_lib/include/bmi_set_var.inc \
	$(builddir)/../../dflowfm_lib/include/bmi_set_var_slice.inc \
	$(builddir)/../include/RF.inc \
	$(builddir)/../include/blasfm.inc

EXTRA_DIST += $(builddir)/../../dflowfm_lib/templates 

# default to empty so we can conditionally expand it
include_HEADERS=

EXTRALIBS=

# For source distributions: the tar.gz will contain the correct version numbers in the generated *version.F90
# So, just before packaging the .tar.gz dist, disable version number REgeneration parts of this Makefile.am.
dist-hook:
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.am
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.in

if USEPETSC
PETSOURCES=solve_petsc.F90
else
PETSOURCES=
endif

libdflowfm_la_SOURCES=\
        unstruc_version.F90 \
        unstruc_messages.f90 \
        unstruc_ini.f90 \
        xbeach_typesandkinds.F90 \
        dredge_data.f90 \
        bedform_data.f90 \
        d3ddimens.f90 \
        modules.f90 \
        dfm_signals.F90 \
        inquire_flowgeom.f90 \
        oned_functions.f90 \
        d_flooding.f90 \
        partition.F90 \
        unstruc_structures.f90 \
        unstruc_netcdf.f90 \
        network.f90 \
        monitoring.f90 \
        manholes.f90 \
        splines.f90 \
        step_to_screen.f90 \
        unstruc_opengl.F90 \
        unstruc_display.F90 \
        hrfft.f \
        shaec.f \
        shsec.f \
        sphcom.f \
        caching.f90 \
        unstruc_model.f90 \
        meteo1.f90 \
        fourier_analysis.f90 \
        unstruc_files.f90 \
        unstruc_netcdf_incremental.f90 \
        unstruc_shapefile.F90 \
        dredgepolygon.f90 \
        bedform_io.f90 \
        unstruc_boundaries.f90 \
        unstruc_inifields.f90 \
        unstruc_startup.f90 \
        unstruc_opengis.f90 \
        wrwaq.F90 \
        dem.f90 \
        rgfstuff.f90 \
        filez.f90 \
        model_specific.f90 \
        rcm.f90 \
        unstruc_api.F90 \
        rest.F90 \
        xbeach_paramsconst.f90 \
        xbeach_netcdf.f90 \
        xbeach_errorhandling.f90 \
        xbeach_filefunctions.F90 \
        xbeach_interp.F90 \
        xbeach_math_tools.F90 \
        xbeach_readkey.F90 \
        xbeach_wave_boundary_datastore.f90 \
        xbeach_wave_boundary_init.f90 \
        xbeach_wave_boundary_update.f90 \
        xbeach_wave_boundary_main.f90 \
        xbeachwaves.f90 \
        net.F90 \
        gui.F90 \
        saadf.F \
        saadf90.F90 \
        calibration.f90
# Ok now we can't include $(PETSOURCES) here, the reason why is hard to explain......
# http://www.gnu.org/software/automake/manual/html_node/Conditional-Sources.html

# So to add PETSOURCES we do a sidestep into an automake conditional.
# now in the middle, add PETSOURCES
if USEPETSC
libdflowfm_la_SOURCES+=$(PETSOURCES)
endif

# then the final part of the source
libdflowfm_la_SOURCES+=\
        solve_parms.F90 \
        solve_guus.F90 \
        waves.f90 \
        fm_bedform.f90 \
	fm_morstatistics.f90 \
        fm_dredge.f90 \
        fm_erosed.f90 \
        fm_wq_processes.f90 \
        fm_wq_processes_mba.f90 \
        fm_thahbc.f90 \
        tecplot.F90 \
        transport.f90 \
        part.f90 \
        advec.f90 \
	m_filter.f90 \
	filter.F90 \
        unstruc.F90
# and the library interface
libdflowfm_la_SOURCES+=\
        $(builddir)/../../dflowfm_lib/src/unstruc_bmi.F90


# This line would prevent warnings from gfortran about assigned goto's (but introduce warning from ifort about unknown flag)
#saadf.o : FFLAGS += -std=legacy

# Version number generation (only when in SVN source dir, not for tar.gz dist)

REGENERATE_VN=yes

# (the space before ifeq is intentional to fool Automake, and leave this conditional to Make)
 ifeq ($(REGENERATE_VN),yes)
# We're in a working copy build dir, so always update the *version.F90
unstruc_version.F90: unstruc_version.F90.svn .FORCE
	$(top_builddir)/scripts_lgpl/linux/update_version.sh \
	$(builddir)/unstruc_version.F90 $(srcdir) $(builddir)/../../../res/version_number.ini $(abs_top_builddir)
 else
# Do not force regeneration of *version.F90, because it is already shipped in a source distribution archive.
unstruc_version.F90:
 endif

.FORCE: 

.PHONY: .FORCE

MOSTLYCLEANFILES = *.mod


#-------------------------------------------------------------------------------
#   Fortran flags

FINCLUDES += \
	-I$(builddir)/../include

FMODULES += \
    -I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
    -I$(top_builddir)/utils_gpl/morphology/packages/morphology_data/src \
    -I$(top_builddir)/utils_gpl/morphology/packages/morphology_io/src \
    -I$(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src \
    -I$(top_builddir)/utils_gpl/trachytopes/packages/trachytopes_io/src \
    -I$(top_builddir)/utils_gpl/trachytopes/packages/trachytopes_kernel/src \
    -I$(top_builddir)/utils_gpl/flow1d/packages/flow1d_core/src \
    -I$(top_builddir)/utils_gpl/flow1d/packages/flow1d_io/src \
    -I$(top_builddir)/third_party_open/kdtree2/src-f90 \
    -I$(top_builddir)/third_party_open/libsigwatch \
    -I$(top_builddir)/third_party_open/md5/md5file \
    -I$(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src \
    -I$(top_builddir)/engines_gpl/waq/include \
    -I$(top_builddir)/engines_gpl/waq/packages/wq_processes/src \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/bloom \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/charon \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/proc_preprocess \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/waq_process \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

# NOTE:
# includes, modules, optimization and openmp are all added to AM_FCFLAGS by common.am

#-------------------------------------------------------------------------------
#   Fortran 90 flags

AM_FCFLAGS+= \
    -DHAVE_DISPLAY=0 \
    -DNetCDF4

if USENETCDF
AM_FCFLAGS+=${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}
# Manualy add the netcdff (fortran dll), bug in pkg-config file is not fixed until in NetCDF 4.4 HEAD
LIBS+=${NETCDF_LIBS} ${NETCDF_FORTRAN_LIBS} -lnetcdff
endif

if USEMPI
AM_FCFLAGS+=${MPI_CFLAGS}
AM_FCFLAGS+=${MPI_FCFLAGS}
# Manualy add the MPI (fortran dll)
LIBS+=${MPILIBS}
endif

if USEPETSC
AM_FCFLAGS+=${PETSc_CFLAGS}
LIBS+=${PETSc_LIBS}
endif

if USEPROJ
AM_FCFLAGS+=${PROJ_CFLAGS}
LIBS+=${PROJ_LIBS}
USEFORTRANGIS=TRUE
endif

if USESHAPELIB
AM_FCFLAGS+=${SHAPELIB_CFLAGS}
LIBS+=${SHAPELIB_LIBS}
USEFORTRANGIS=TRUE
endif

if USEFORTRANGIS
FMODULES += \
    -I$(top_builddir)/third_party_open/fortrangis/libfortrangis
EXTRALIBS += \
    $(top_builddir)/third_party_open/fortrangis/libfortranc/libfortranc.la \
    $(top_builddir)/third_party_open/fortrangis/libfortrangis/libfortrangis.la
endif

if USEMETIS
AM_FCFLAGS+=${METIS_CFLAGS}
LIBS+=${METIS_LIBS}
endif

#if USETECIO
#AM_FCFLAGS+=${TECIO_FCFLAGS}
#LIBS+=${TECIO_LIBS}
#endif

if USEPARMS
LIBS+=-L../third_party_open/pARMS/pARMS_3.2/lib -lparms
LIBS+=-L${blasdir}/lib -lrefblas
endif

if USEMKL
LIBS+=${MKL_LIBS}
AM_FCFLAGS+=${MKL_FCFLAGS}
endif

# if USEESMF
# ESMFSOURCES=unstruc_esmf.F90
# else
# ESMFSOURCES=
# endif



EXTRALIBS+=\
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
        $(top_builddir)/third_party_open/md5/md5file/libmd5file.la \
        $(top_builddir)/third_party_open/md5/md5digest/libmd5digest.la \
        $(CCLIBS) -luuid

# dflowfm_LDADD = \
#         libdflowfm.la		#
# dflowfm_LDFLAGS=-static
libdflowfm_la_LIBADD=\
        $(EXTRALIBS)
