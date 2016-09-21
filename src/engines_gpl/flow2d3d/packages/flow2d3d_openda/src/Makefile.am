#-------------------------------------------------------------------------------
#   Makefile for flow2d3d
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   18 oct 2011
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libflow2d3d_openda.la

INCLUDES = \
    $(STANDARD_C_INCLUDES) \
	-I../include

FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include \
	-I$(top_builddir)/utils_lgpl/esmfsm/include @ODA_INCDIR@

FMODULES += \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/gdp \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/manager/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/inichk

#-------------------------------------------------------------------------------
#Sources
libflow2d3d_openda_la_SOURCES = \
         m_d3d_state.f90 \
         m_d3dstate_2_openda.f90 \
         d3dstate_2_d3d.f90 \
         flow2d3d_f.f90 \
         flow2d3d_openda.f90 \
         flow2d3d_openda_dll.f90 \
         localization.f90 \
         flow2d3d_openda_version.cpp

#-------------------------------------------------------------------------------
#Libraries
libflow2d3d_openda_la_LIBADD = \
	$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d/src/libflow2d3d.la @LTL_CTA@

libflow2d3d_openda_la_LDFLAGS = -version-info 0:0:0 @ODA_LIB@


BUILT_SOURCES = \
	./always \
	../include/flow2d3d_openda_version.h

./always:
	rm -f ../include/flow2d3d_openda_version.h

../include/flow2d3d_openda_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh ../include/flow2d3d_openda_version.h $(srcdir) ../include/version_number.ini $(top_builddir)
