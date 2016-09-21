include $(top_srcdir)/common.am
bin_PROGRAMS = datsel

datsel_SOURCES = \
	main.cpp

#-------------------------------------------------------------------------------
#Libraries

datsel_LDADD = \
	$(top_builddir)/tools_gpl/datsel/packages/datsel_f/src/libdatsel_f.la \
	$(top_builddir)/tools_gpl/datsel/packages/datsel_version_number/src/libdatsel_version_number.la \
	$(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
	-ldl \
	$(FCLIBS) $(CCLIBS)

