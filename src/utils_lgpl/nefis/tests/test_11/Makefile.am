#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_11

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_11_SOURCES = \
	test_11.c
test_11_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(CCLIBS)
