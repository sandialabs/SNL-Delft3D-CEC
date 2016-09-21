#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------
include $(top_srcdir)/common.am

MYC_INCLUDES=$(top_srcdir)/include

bin_PROGRAMS= delpar

AM_FCFLAGS += $(OPENMP_FCFLAGS)
AM_FFLAGS += $(OPENMP_FFLAGS)

delpar_SOURCES= \
	main.f

# some libraries are loaded twice to find all undefined symbols

delpar_LDADD =\
	$(top_srcdir)/engines_gpl/part/packages/kernel_f/src/libkernel_f.la \
	$(top_srcdir)/engines_gpl/part/packages/io_f/src/libio_f.la \
	$(top_srcdir)/engines_gpl/part/packages/utils_f/src/libutils_f.la \
	$(top_srcdir)/engines_gpl/part/packages/data_f/src/libdata_f.la \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/libRDToken.la \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
	$(top_srcdir)/engines_gpl/part/packages/version_number/src/libpart_version_number.la \
	$(top_srcdir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
	-ldl \
	$(FCLIBS) \
    -lpthread
