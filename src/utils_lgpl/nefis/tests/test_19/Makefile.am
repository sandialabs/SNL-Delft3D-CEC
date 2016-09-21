#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   18 sep 2008
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------
include $(top_srcdir)/common.am
bin_PROGRAMS = test_19

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_19_SOURCES = \
	test_19.c
test_19_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
