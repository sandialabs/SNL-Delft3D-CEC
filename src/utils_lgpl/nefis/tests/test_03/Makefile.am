#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_03

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_03_SOURCES = \
	test_03.c
test_03_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(CCLIBS)
