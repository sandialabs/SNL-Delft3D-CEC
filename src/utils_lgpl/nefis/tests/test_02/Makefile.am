#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_02

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_02_SOURCES = \
	test_02.c
test_02_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(CCLIBS)
