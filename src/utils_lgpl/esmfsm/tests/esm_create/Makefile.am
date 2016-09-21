include $(top_srcdir)/common.am
bin_PROGRAMS = esm_create

esm_create_SOURCES = \
	esm_create.c

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/esmfsm/include

#-------------------------------------------------------------------------------
#Libraries

esm_create_LDADD = \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la


#	-ldl \
#	$(FCLIBS) -limf
