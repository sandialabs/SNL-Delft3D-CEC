bin_PROGRAMS = dflowfm
#AM_CXXFLAGS = $(INTI_CFLAGS)

# Import optimization and OPENMP (in AM_FCFLAGS) from common.am
include $(top_srcdir)/common.am

# default to empty so we can conditionally expand it
include_HEADERS=

INCLUDES = \
	-I$(builddir)/../../dflowfm_kernel/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src \
	-I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src

# For source distributions: the tar.gz will contain the correct version numbers in the generated *version.F90
# So, just before packaging the .tar.gz dist, disable version number REgeneration parts of this Makefile.am.
dist-hook:
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.am
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.in

dflowfm_SOURCES=\
        net_main.F90

# This line would prevent warnings from gfortran about assigned goto's (but introduce warning from ifort about unknown flag)
#saadf.o : FFLAGS += -std=legacy

.FORCE: 

.PHONY: .FORCE

MOSTLYCLEANFILES = *.mod


#-------------------------------------------------------------------------------
#   Fortran flags

AM_FFLAGS += \
     $(OPENMP_FFLAGS)

FINCLUDES +=

#FMODULES += \
#    -I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
#    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
#    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
#    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
#    -I$(top_builddir)/utils_gpl/morphology/packages/morphology_data/src \
#    -I$(top_builddir)/utils_gpl/morphology/packages/morphology_io/src \
#    -I$(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src \
#    -I$(top_builddir)/utils_gpl/trachytopes/packages/trachytopes_io/src \
#    -I$(top_builddir)/utils_gpl/trachytopes/packages/trachytopes_kernel/src \
#    -I$(top_builddir)/third_party_open/kdtree2/src-f90 \
#    -I$(top_builddir)/third_party_open/libsigwatch


# NOTE:
# includes, modules, optimization and openmp are all added to AM_FCFLAGS by common.am

#-------------------------------------------------------------------------------
#   Fortran 90 flags

AM_FCFLAGS+= \
	$(OPENMP_FCFLAGS) \
    -DHAVE_DISPLAY=0

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

if USEMETIS
AM_FCFLAGS+=${METIS_CFLAGS}
LIBS+=${METIS_LIBS}
endif

#if USETECIO
#AM_FCFLAGS+=${TECIO_FCFLAGS}
#LIBS+=${TECIO_LIBS}
#endif

if USEMKL
LIBS+=${MKL_LIBS}
AM_FCFLAGS+=${MKL_FCFLAGS}
endif


EXTRALIBS=\
        $(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src/libkdtreewrapper.la \
        $(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src/libgridgeom.la \
        $(CCLIBS)

#dflowfm_LDFLAGS=-static
dflowfm_LDADD=\
        $(top_builddir)/engines_gpl/dflowfm/packages/dflowfm_kernel/src/libdflowfm.la \
        $(EXTRALIBS)



