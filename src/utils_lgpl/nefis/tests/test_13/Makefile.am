#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_13

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_13_SOURCES = \
	test_13.c
test_13_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(CCLIBS)
