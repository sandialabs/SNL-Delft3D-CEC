#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libbloom.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
         -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/waq_process \
         -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libbloom_la_SOURCES = \
       bloom_data.f90 \
       blclmort.f \
       blfile.f \
       blinit.f \
       blinpu.f \
       blmort.f \
       bloom.f \
       bloutc.f \
       blprim.f \
       bvect.f \
       constr.f \
       dynrun.f \
       ebcalc.f \
       exclud.f \
       fixinf.f \
       iblbal.f \
       readfrm.f \
       maxgro.f \
       maxprd.f \
       natmor.f \
       prinma.f \
       prinsu.f \
       print6.f \
       qslp.f \
       setabc.f \
       solvlp.f \
       spcsd.f  
#-------------------------------------------------------------------------------
# Libraries
