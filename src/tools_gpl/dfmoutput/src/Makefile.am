bin_PROGRAMS = dfmoutput

# Import optimization and OPENMP (in AM_FCFLAGS) from common.am
include $(top_srcdir)/common.am

# default to empty so we can conditionally expand it
include_HEADERS=

# For source distributions: the tar.gz will contain the correct version numbers in the generated *version.F90
# So, just before packaging the .tar.gz dist, disable version number REgeneration parts of this Makefile.am.
dist-hook:
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.am
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.in

dfmoutput_SOURCES=\
	dfmoutput_version.F90 \
	dfm_params.F90 \
	dfm_merge.F90 \
	dfm_max25_read_nc_histories.f90 \
	dfm_max25_write_extremes_his.f90 \
	dfm_max25_getdata.f90 \
	dfm_gen_filter.f90 \
	dfmoutput_main.F90

# Version number generation (only when in SVN source dir, not for tar.gz dist)

REGENERATE_VN=yes

# (the space before ifeq is intentional to fool Automake, and leave this conditional to Make)
 ifeq ($(REGENERATE_VN),yes)
# We're in a working copy build dir, so always update the *version.F90
dfmoutput_version.F90: dfmoutput_version.F90.svn .FORCE
	$(top_builddir)/scripts_lgpl/linux/update_version.sh \
	$(builddir)/dfmoutput_version.F90 $(srcdir) $(srcdir)/../res/version_number.ini $(abs_top_builddir)
 else
# Do not force regeneration of *version.F90, because it is already shipped in a source distribution archive.
dfmoutput_version.F90:
 endif

.FORCE: 

.PHONY: .FORCE

MOSTLYCLEANFILES = *.mod


#-------------------------------------------------------------------------------
#   Fortran flags

FINCLUDES +=

FMODULES += \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
    -I$(top_builddir)/third_party_open/FLAP/Test_Driver/mod
#    -I$(top_builddir)/third_party_open/FLAP/src


# NOTE:
# includes, modules, optimization and openmp are all added to AM_FCFLAGS by common.am

#-------------------------------------------------------------------------------
#   Fortran 90 flags

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

EXTRALIBS=\
        $(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src/libIONetcdf.la \
        $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
        $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src/libDeltaresCommonMpi.la \
        $(top_builddir)/third_party_open/FLAP/Test_Driver/obj/ir_precision.o \
        $(top_builddir)/third_party_open/FLAP/Test_Driver/obj/data_type_command_line_interface.o \
        $(CCLIBS)
#        $(top_builddir)/third_party_open/FLAP/src/libFLAP.la

dfmoutput_LDFLAGS=-static

dfmoutput_LDADD=\
        $(EXTRALIBS)
