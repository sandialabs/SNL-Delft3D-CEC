#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Adri.Mourits@deltares.nl
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = vs

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I$(top_builddir)/tools_gpl/vs/packages/vs/include

#-------------------------------------------------------------------------------
#Sources
vs_SOURCES = \
	cl.c    \
	df.c    \
	el.c    \
	ex.c    \
	fm.c    \
	fu.c    \
	gen.c   \
	gr.c    \
	intr.c  \
	pp.c    \
	sm.c    \
	val.c   \
	vl.c    \
	vr.c    \
	wr.c    \
	vs_y.y  \
	vs_l.l  \
	viewsel.c

vs_LDADD = $(LDADD) \
	$(top_builddir)/tools_gpl/vs/packages/vs_version_number/src/libvs_version_number.la \
	$(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la

AM_LFLAGS = -L
AM_YFLAGS = -d

