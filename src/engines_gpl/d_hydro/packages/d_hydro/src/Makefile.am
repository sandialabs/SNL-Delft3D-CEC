#-------------------------------------------------------------------------------
#   Makefile for d_hydro
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am


bin_PROGRAMS = d_hydro

d_hydro_SOURCES = \
    d_hydro_version.cpp \
    d_hydro.cpp


AM_CPPFLAGS = \
    -I$(STANDARD_C_INCLUDES) \
    -I../include

#-------------------------------------------------------------------------------
#   Libraries


d_hydro_LDADD = \
    $(top_builddir)/utils_lgpl/d_hydro_lib/packages/d_hydro_lib/src/libDHydroLib.la \
    -ldl -lpthread

d_hydro_LDFLAGS = -export-dynamic

#    -lmcheck


#-------------------------------------------------------------------------------

BUILT_SOURCES = \
    ./always \
    ../include/d_hydro_version.h

./always:
	rm -f ../include/d_hydro_version.h

../include/d_hydro_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh ../include/d_hydro_version.h $(srcdir) ../include/version_number.ini $(top_builddir)
