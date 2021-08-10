#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libwaq_utils.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
	    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
	    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libwaq_utils_la_SOURCES =  \
       julian.f \
       waqmem.f90 \
       progress.f90 \
       fileutils.f90 \
       dlwq1_grid_mod.f \
       dlwq_data.f \
       dwq_proces_set.f \
       rd_stt.f90 \
       rd_sub.f90 \
       dlwq0t.f \
       dhcommand.f90 \
       DHGARG.F \
       SRSTOP.F \
       unlock.f90 \
       avundf_linux.f \
       cnvper.f \
       cnvtim.f \
       d00sub.f \
       dattim.f \
       delwaq_loads.f \
       dhagg2.f \
       dhaggr.f \
       dhagkm.f \
       dhc20m.f \
       dhcarg.f \
       dhcmov.f \
       dhctim.f \
       dhcwrd.f \
       dhdag2.f \
       dhdagg.f \
       dhdelf.f \
       dherrs.f \
       dhfext.f \
       dhggd.f \
       dhgnam.f \
       dhgpoi.f \
       dhgvar.f \
       dhgwrd.f \
       dhimov.f \
       dhisys.f \
       dhkmrk.f \
       dhkmst.f \
       partition_arrays.f90 \
       dhltim.f \
       dhnlun.f \
       dhnolay.f \
       dhnoseg.f \
       dhopnf.F \
       dhpath.f \
       dhralloc.f \
       dhrmis.f \
       dhscal.f \
       dhslen.f \
       dhsuma.f \
       dhswtr.f \
       dhucas.f \
       dhzerc.f \
       dhzeri.f \
       errsys.f \
       fffind.f \
       filldm.f \
       getcom.f \
       gkwini.f \
       makpnt.f \
       monsys.f \
       movchr.f \
       move.f \
       movint.f \
       movrl.f \
       output_mod.f \
       putget.f \
       putgtc.f \
       rapdef.f \
       rbpdef.f \
       rdvers.f \
       rdwrk3.f \
       rdwrk4.f \
       reptim.f \
       stasub.f \
       strip.f \
       usedcp.f \
       wapdef.f \
       wbpdef.f \
       wrwrk3.f \
       zero.f \
       zoek.f

#-------------------------------------------------------------------------------
# Libraries
libwaq_utils_la_LIBADD=\
	$(top_builddir)/utils_lgpl/nefis/packages/nefis/src_so/libnefis.la
