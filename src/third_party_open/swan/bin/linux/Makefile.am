#-------------------------------------------------------------------------------
#   Makefile
#
#   Copyright (C)  Stichting Deltares, 2011-2018.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
#-------------------------------------------------------------------------------
#Includes
dist_bin_SCRIPTS = \
	swan_mpi.exe \
	swan_omp.exe

install-exec-hook:
	cp -p $(top_srcdir)/third_party_open/swan/bin/linux/libnetcdf.so.4 $(libdir)
	cp -p $(top_srcdir)/third_party_open/swan/bin/linux/libnetcdff.so.4 $(libdir)
