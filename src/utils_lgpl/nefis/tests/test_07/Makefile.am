#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_07_dependent

test_07_dependent_SOURCES = \
	test_07.f

test_07_dependent_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
