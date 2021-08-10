bin_PROGRAMS = ec_module_test
#AM_CXXFLAGS = $(INTI_CFLAGS)

# Import optimization and OPENMP (in AM_FCFLAGS) from common.am
include $(top_srcdir)/common.am

# default to empty so we can conditionally expand it
include_HEADERS=

INCLUDES = \
	-I$(builddir)/../../packages/dhydrology_io/dhydrology_io/src \
   -I$(builddir)/../../packages/dhydrology_kernel/dhydrology_kernel/src  \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src \
	-I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src

# For source distributions: the tar.gz will contain the correct version numbers in the generated *version.F90
# So, just before packaging the .tar.gz dist, disable version number REgeneration parts of this Makefile.am.
dist-hook:
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.am
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.in

dhydrology_test_SOURCES=\
        cmdlargs.f90 \
        dhydrology_test.F90

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

# NOTE:
# includes, modules, optimization and openmp are all added to AM_FCFLAGS by common.am

#-------------------------------------------------------------------------------
#   Fortran 90 flags

if USENETCDF
AM_FCFLAGS+=${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}
# Manualy add the netcdff (fortran dll), bug in pkg-config file is not fixed until in NetCDF 4.4 HEAD
LIBS+=${NETCDF_LIBS} ${NETCDF_FORTRAN_LIBS} -lnetcdff
endif

if USEMKL
LIBS+=${MKL_LIBS}
AM_FCFLAGS+=${MKL_FCFLAGS}
endif

EXTRALIBS=\
        $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
        $(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src/libkdtreewrapper.la \
        $(CCLIBS)

#ec_module_test_LDFLAGS=-static
dhydrology_test_LDADD=\
        $(top_builddir)/utils_lgpl/dhydrology/packages/dhydrology_kernel/src/libDhydrologyKernel.la \
        $(top_builddir)/utils_lgpl/dhydrology/packages/dhydrology_io/src/libDhydrologyIO.la \
        $(EXTRALIBS)



