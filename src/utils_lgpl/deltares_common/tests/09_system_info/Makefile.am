#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = system_info

#-------------------------------------------------------------------------------
#Sources
system_info_SOURCES = \
	main.cpp

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/include

#-------------------------------------------------------------------------------
#Libraries

system_info_LDADD = \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libDeltaresCommonC.la


#	-ldl \
#	$(FCLIBS) -limf
