#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libdata_f.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libdata_f_la_SOURCES = \
         precision.f90 \
         typos_mod.f90 \
         file_info.f90 \
         modeldim.f90 \
         partmem.f90 \
         global_data.f90 \
         alloc.f90 \
         dealloc.f90 \
         dealloc_part.f90

#-------------------------------------------------------------------------------
#Libraries

