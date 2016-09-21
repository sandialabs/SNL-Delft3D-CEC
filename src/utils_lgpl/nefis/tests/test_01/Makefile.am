#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_01

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_01_SOURCES = \
	test_01.c
test_01_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(CCLIBS)
