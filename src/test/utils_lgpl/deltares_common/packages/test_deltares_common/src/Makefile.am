include $(top_srcdir)/common.am
bin_PROGRAMS = test_deltares_common

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ftnunit/packages/ftnunit/src

#-------------------------------------------------------------------------------
#Sources 
test_deltares_common_SOURCES = \
	test_bilin5.f90 \
	test_combinepaths.f90 \
	test_distance.f90 \
	test_geometry_module.f90 \
	test_hash_search.f90 \
	test_precision_basics.f90 \
	test_properties.f90 \
	test_sorting_algorithms.f90 \
	test_string_module.f90 \
	test_deltares_common.f90

#-------------------------------------------------------------------------------
#Libraries

test_deltares_common_LDADD = \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
	$(top_builddir)/utils_lgpl/ftnunit/packages/ftnunit/src/libFtnUnit.la
