#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = stream01test

#-------------------------------------------------------------------------------
#Sources
stream01test_SOURCES = \
	smalltest.cpp

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/stream/include

#-------------------------------------------------------------------------------
#Libraries

stream01test_LDADD = \
	$(top_builddir)/utils_lgpl/stream/packages/stream/src/libstream.la \
	$(top_builddir)/utils_lgpl/stream/packages/stream_version_number/src/libstream_version_number.la


#	-ldl \
#	$(FCLIBS) -limf
