#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libIOOutput.la

#-------------------------------------------------------------------------------
#Includes
AM_FCFLAGS += $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)

FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/utils_gpl/morphology/packages/morphology_data/src \
	-I$(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/general


#-------------------------------------------------------------------------------
#Sources
libIOOutput_la_SOURCES = \
         wrtarray.f90 \
         calbed.f90 \
         dirint.f90 \
         dp999.f90 \
         fouana.f90 \
         fouvecmax.f90 \
         frdint.f90 \
         getfield.f90 \
         getfpt.f90 \
         inippr.f90 \
         findnm_kcs.f90 \
         findnm_kcs_flowwrapper.f90 \
         ksieta2xy.f90 \
         layer_interfaces.f90 \
         pinpol.f90 \
         postpr.f90 \
         postpr_hdt.f90 \
         pppsol.f90 \
         prhead.f90 \
         prihis.f90 \
         prthis.f90 \
         prtmap.f90 \
         rdtimc.f90 \
         rdtimw.f90 \
         realfileversion.f90 \
         rwbotc.f90 \
         rwbotc_double.f90 \
         setwav.f90 \
         tcross.f90 \
         tstat.f90 \
         tstat_bed.f90 \
         update_stat_locations.f90 \
         updbnddps.f90 \
         uv2zeta.f90 \
         waq2flow.f90 \
         wrboun.f90 \
         wrcomi.f90 \
         wrcomt.f90 \
         wrcomwind.f90 \
         wrcurt.f90 \
         wrdwqt.f90 \
         wrfou.f90 \
         wrfous.f90 \
         wrfouv.f90 \
         wrgrid.f90 \
         wrhfluff.f90 \
         wribot.f90 \
         wrd_main.f90 \
         wrh_main.f90 \
         wrm_main.f90 \
         wridoc.f90 \
         wridro.f90 \
         wrihis.f90 \
         wrihisbal.f90 \
         wrihisdad.f90 \
         wrihisdis.f90 \
         wrimapdad.f90 \
         wrimap.f90 \
         wrirst.f90 \
         wrkenc.f90 \
         wrkent.f90 \
         wrmfluff.f90 \
         wrmorm.f90 \
         wrmorst.f90 \
         wrparm.f90 \
         wrplot.f90 \
         wrrolm.f90 \
         wrrouf.f90 \
         wrsedd.f90 \
         wrsedh.f90 \
         wrsedm.f90 \
         wrsedmavg.f90 \
         wrsedmgrp.f90 \
         wrsedwaqm.f90 \
         wrspcp.f90 \
         wrtdro.f90 \
         wrthis.f90 \
         wrthisbal.f90 \
         wrthisdad.f90 \
         wrthisdis.f90 \
         wrtmap.f90 \
         wrwaqapnt.f90 \
         wrwaqbal.f90 \
         wrwaqcco.F90 \
         wrwaqfil.F90 \
         wrwaqflo.f90 \
         wrwaqhyd.f90 \
         wrwaqld0.f90 \
         wrwaqlenaggr.f90 \
         wrwaqlennoag.f90 \
         wrwaqload.f90 \
         wrwaqmakpnt.f90 \
         wrwaqnpnt.f90 \
         wrwaqpnt.F90 \
         wrwaqsrf.F90 \
         wrwaqvol.f90 \
         wrwavh.f90 \
         zrtc.f90

#-------------------------------------------------------------------------------
#Libraries

