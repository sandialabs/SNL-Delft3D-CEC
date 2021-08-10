#-------------------------------------------------------------------------------
#   Makefile for dimr
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am


bin_PROGRAMS = dimr

#-------------------------------------------------------------------------------
#Sources
dimr_SOURCES = \
    dimr_exe_version.cpp \
    dimr_exe.cpp

#-------------------------------------------------------------------------------
#Includes
AM_CPPFLAGS = \
    -I$(STANDARD_C_INCLUDES) \
    -I../include

AM_CFLAGS += -fPIC
AM_CXXFLAGS += -fPIC

#-------------------------------------------------------------------------------
#   Libraries


dimr_LDADD = \
    $(top_builddir)/engines_gpl/dimr/packages/dimr_lib/src/libdimr.la \
    -ldl \
    $(NETCDF_LIBS) $(FCLIBS)

# dimr_LDFLAGS = -export-dynamic

#    -lmcheck


#-------------------------------------------------------------------------------

BUILT_SOURCES = \
    ./always \
    ../include/dimr_exe_version.h

./always:
	rm -f ../include/dimr_exe_version.h

../include/dimr_exe_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh ../include/dimr_exe_version.h $(srcdir) ../include/version_number.ini $(top_builddir) 
