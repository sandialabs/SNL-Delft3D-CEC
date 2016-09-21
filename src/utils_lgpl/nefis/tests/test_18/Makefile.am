#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   18 sep 2008
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------
include $(top_srcdir)/common.am
bin_PROGRAMS = test_18

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_18_SOURCES = \
	test_18.c
test_18_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
