#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   02 dec 2008
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = system_info.exe

#-------------------------------------------------------------------------------
#Sources
system_info_exe_SOURCES = \
	main.cpp

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/include

#-------------------------------------------------------------------------------
#Libraries

system_info_exe_LDADD = \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libDeltaresCommonC.la


#	-ldl \
#	$(FCLIBS) -limf
