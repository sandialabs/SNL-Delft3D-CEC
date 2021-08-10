#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = mormerge

#-------------------------------------------------------------------------------
#Includes

FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src 

#-------------------------------------------------------------------------------
#Sources
mormerge_SOURCES = \
         mormerge.f90 \
         merge.f90

#-------------------------------------------------------------------------------
#Libraries

mormerge_LDADD   = \
$(top_builddir)/tools_gpl/mormerge/packages/mormerge_version_number/src/libmormerge_version_number.la \
$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la  \
$(FCLIBS) $(CCLIBS)

