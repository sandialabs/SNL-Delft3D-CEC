include $(top_srcdir)/common.am
bin_PROGRAMS = test_waq_utils_f

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
	-I$(top_builddir)/utils_lgpl/ftnunit/packages/ftnunit/src

#-------------------------------------------------------------------------------
#Sources 
test_waq_utils_f_SOURCES = \
	basic_tests.f90 \
	test_waq_utils_f.f90

#-------------------------------------------------------------------------------
#Libraries

test_waq_utils_f_LDADD = \
	$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
	$(top_builddir)/utils_lgpl/ftnunit/packages/ftnunit/src/libFtnUnit.la
