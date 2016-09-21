#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Jan. Mooiman@deltares.nl
#   19 june 2009
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libNefisSO.la

#-------------------------------------------------------------------------------
#Includes
AM_CFLAGS += \
    -I../include \
    -D_FILE_OFFSET_BITS=64 \
    -DPTR8
AM_CXXFLAGS += \
    -I../include \
    -D_FILE_OFFSET_BITS=64 \
    -DPTR8

#-------------------------------------------------------------------------------
#Sources
libNefisSO_la_SOURCES = \
	../src/c2c.c \
	../src/df.c \
	../src/er.c \
	../src/f2c.c \
	../src/gp.c \
	../src/gt.c \
	../src/hs.c \
        ../src/nefis_version.cpp \
	../src/oc.c \
	../src/pt.c \
	../src/rt.c \
	../src/wl-xdr.c

EXTRA_DIST = \
         ../include/nefis_version.h.svn

#-------------------------------------------------------------------------------
#Libraries

#libNefisSO_la_LIBADD = \
#	-lltdl

BUILT_SOURCES = ./always \
	../include/nefis_version.h

./always:
	rm -rf ../include/nefis_version.h

../include/nefis_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/../include/nefis_version.h $(srcdir) ../include/version_number.ini $(top_builddir)

