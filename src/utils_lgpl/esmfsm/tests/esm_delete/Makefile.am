include $(top_srcdir)/common.am
bin_PROGRAMS = esm_delete

esm_delete_SOURCES = \
	esm_delete.c

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/esmfsm/include

#-------------------------------------------------------------------------------
#Libraries

esm_delete_LDADD = \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la

