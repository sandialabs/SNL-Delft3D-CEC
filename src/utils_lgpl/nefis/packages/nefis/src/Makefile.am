#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libNefis.la

#-------------------------------------------------------------------------------
#Includes
AM_CFLAGS += \
    -I../include \
    -D_FILE_OFFSET_BITS=64 \
    -DLINUX64 \
    -DPTR8
AM_CXXFLAGS += \
    -I../include \
    -D_FILE_OFFSET_BITS=64 \
    -DLINUX64 \
    -DPTR8

#-------------------------------------------------------------------------------
#Sources
libNefis_la_SOURCES = \
         c2c.c \
         df.c \
         er.c \
         f2c.c \
         gp.c \
         gt.c \
         hs.c \
         nefis_version.cpp \
         oc.c \
         pt.c \
         rt.c \
         wl-xdr.c

EXTRA_DIST = \
         ../include/nefis_version.h.svn

#-------------------------------------------------------------------------------
#Libraries

BUILT_SOURCES = ./always \
	../include/nefis_version.h

./always:
	rm -rf ../include/nefis_version.h

../include/nefis_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/../include/nefis_version.h $(srcdir) ../include/version_number.ini $(top_builddir)
