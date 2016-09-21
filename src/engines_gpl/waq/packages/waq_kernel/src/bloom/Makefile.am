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
         -I$(top_builddir)/engines_gpl/waq/packages/waq_data/src/proc_data \
         -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libbloom_la_SOURCES = blclmort.f \
       blfile.f \
       blinit.f \
       blinpu.f \
       blmort.f \
       bloom.f \
       bloutc.f \
       blprim.f \
       bvect.f \
       calend.f \
       ccons.f \
       change.f \
       chhelp.f \
       consb2.f \
       constr.f \
       cspgro.f \
       cspsto.f \
       cvrblm.f \
       czoopl.f \
       dayeu.f \
       diel.f \
       domina.f \
       dynade.f \
       dynrun.f \
       ebcalc.f \
       edit.f \
       exclud.f \
       fixinf.f \
       getal.f \
       getcoe.f \
       getnam.f \
       graph.f \
       grazin.f \
       hdrblm.f \
       headin.f \
       iblbal.f \
       inptdt.f \
       inptnm.f \
       input1.f \
       input2.f \
       input3.f \
       input4.f \
       intpol_bloom.f \
       lenst.f \
       maxgra.f \
       maxgro.f \
       maxmor.f \
       maxprd.f \
       natmor.f \
       news.f \
       nutfea.f \
       ophelp.f \
       option.f \
       plotgr.f \
       pools.f \
       prinma.f \
       prinsu.f \
       print6.f \
       prinun.f \
       produc.f \
       promes.f \
       prompt.f \
       prplot.f \
       qslp.f \
       record.f \
       select.f \
       setabc.f \
       setuni.f \
       solvlp.f \
       spcsd.f \
       spindi.f \
       videoc.f

#-------------------------------------------------------------------------------
# Libraries
