#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_04_dependent

test_04_dependent_SOURCES = \
	test_04.f

test_04_dependent_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
