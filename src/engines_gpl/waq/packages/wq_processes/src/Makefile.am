#-------------------------------------------------------------------------------
#   Makefile for wq_processes
#
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libwqprocesses.la
 
AM_FCFLAGS += $(OPENMP_FCFLAGS) $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}
AM_FFLAGS  += $(OPENMP_FFLAGS)  $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I${NETCDFROOT}/include \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/bloom \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/charon \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/proc_preprocess \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/waq_process \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

MPILIBS += $(MPILIBS_ADDITIONAL)

#-------------------------------------------------------------------------------
#Sources
libwqprocesses_la_SOURCES = \
       wq_processes_version.F90 \
       getidentification.f \
       wq_processes_data.f90 \
       wq_processes_initialise.f90 \
       wq_processes_pmsa_size.f90 \
       wq_processes_derivatives.f90 \
       wq_processes_integrate_fluxes.f90 \
       wq_processes_integrate_velocities.f90 \
       wq_processes_velocities.f90 \
       wq_processes_proces.f90

#-------------------------------------------------------------------------------
BUILT_SOURCES = ./always \
	./wq_processes_version.F90

./always:
	rm -rf ./wq_processes_version.F90

./wq_processes_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/wq_processes_version.F90 $(srcdir) $(top_srcdir)/engines_gpl/waq/version/version_number.ini $(top_builddir)
