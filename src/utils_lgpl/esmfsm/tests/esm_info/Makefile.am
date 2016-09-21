include $(top_srcdir)/common.am
bin_PROGRAMS = esm_info

esm_info_SOURCES = \
	esm_info.c

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/esmfsm/include

#-------------------------------------------------------------------------------
#Libraries

esm_info_LDADD = \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la

