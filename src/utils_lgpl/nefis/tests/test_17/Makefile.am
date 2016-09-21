#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------
include $(top_srcdir)/common.am
bin_PROGRAMS = test_17

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_17_SOURCES = \
	test_17.c
test_17_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
