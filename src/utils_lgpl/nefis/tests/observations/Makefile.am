#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   26 oct 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = observ

observ_SOURCES = \
	observ.f
observ_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
