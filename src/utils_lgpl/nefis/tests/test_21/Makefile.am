#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   26 April 2017
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------
include $(top_srcdir)/common.am
noinst_PROGRAMS = test_21

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_21_SOURCES = \
	test_21.c
    
test_21_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
