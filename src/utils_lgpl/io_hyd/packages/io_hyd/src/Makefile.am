#-------------------------------------------------------------------------------
#   Makefile io_hyd
#
#   14 jun 2016
#
#   Copyright (C)  Stichting Deltares, 2011-2016.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libIOHyd.la

#-------------------------------------------------------------------------------
#Includes
AM_FCFLAGS += $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)

FMODULES += \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(NETCDF_FORTRAN_LIBS)

#-------------------------------------------------------------------------------
#Sources
libIOHyd_la_SOURCES = \
         dlwqgrid_mod.f90 \
         dlwq_hyd_data.f90 \
         domain_mod.f90 \
         utils_waqgeom.f90 \
         filmod.f90 \
         wstmod.f90 \
         hydmod.f90 \
         write_waqgeom.f90 \
         read_waqgeom.f90 \
         hyd_waqgeom_mod_old.f90 \
         hyd_waqgeom_old.f90 \
         aggregate_waqgeom.f90 \
         wrwaqgeomcl.f90 \
         read_atr.f90 \
         read_bnd.f90 \
         read_cco.f90 \
         read_dwq.f90 \
         read_hyd.f90 \
         read_lga.f90 \
         read_poi.f90 \
         read_src.f90 \
         read_src_tmp.f90 \
         read_srf.f90 \
         read_vag.f90 \
         read_hyd_init.f90 \
         read_hyd_step.f90 \
         copy_hyd_step.f90 \
         merge_domains.f90 \
         set_hyd.f90 \
         write_atr.f90 \
         write_bnd.f90 \
         write_cco.f90 \
         write_data.f90 \
         write_ddp.f90 \
         write_grd.f90 \
         write_hyd.f90 \
         write_lga.f90 \
         write_lgt.f90 \
         write_poi.f90 \
         write_rfl.f90 \
         write_src.f90 \
         write_srf.f90 \    
         write_hyd_init.f90 \
         write_hyd_step.f90
         
#-------------------------------------------------------------------------------
#Libraries

