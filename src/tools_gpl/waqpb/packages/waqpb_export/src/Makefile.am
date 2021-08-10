#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = waqpbexport

#AM_FCFLAGS += -fopenmp $(OPENMP_FCFLAGS)
#AM_FFLAGS += -fopenmp $(OPENMP_FFLAGS)
#LDFLAGS += -fopenmp

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/tools_gpl/waqpb/packages/include

#-------------------------------------------------------------------------------
#Sources
waqpbexport_SOURCES = \
       waqpb_export.f

#-------------------------------------------------------------------------------
# Libraries

waqpbexport_LDADD = \
    $(top_builddir)/tools_gpl/waqpb/packages/waqpb_lib/src/libwaqpb.la \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la
