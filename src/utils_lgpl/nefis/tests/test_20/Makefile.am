#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   26 April 2017
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------
include $(top_srcdir)/common.am
noinst_PROGRAMS = test_20

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_20_SOURCES = \
	test_20.c

test_20_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)

