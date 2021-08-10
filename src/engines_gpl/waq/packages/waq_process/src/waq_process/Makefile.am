#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libwaq_process.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
     -I$(top_builddir)/engines_gpl/waq/include \
     -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings \
     -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
     -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/bloom \
     -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libwaq_process_la_SOURCES = \
       bottom_set.f \
       get_effi.f \
       adspo4.f \
       advtra.f \
       apatit.f \
       atmdep.f \
       attout.f \
       bacmrt.f \
       bloom_3dl.F90 \
       bodcod.f \
       botmin.f \
       burial.f \
       calchz.f \
       calsed.f \
       caltau.f \
       caltem.f \
       calwav.f \
       cascad.f90 \
       clcrad.f \
       consbl.f \
       cselac.f \
       d40blo.f \
       dayl.f \
       dayrad.f \
       ddepth.f \
       debgrz.f \
       decbod.f \
       decdet.f \
       decpc5.f \
       dectra.f \
       degmp.f \
       densed.f \
       denwat.f \
       depave.f \
       diggin.f \
       dissi.f \
       dlalg.f \
       dmvol.f \
       dsptra.f \
       dsurf.f \
       ebuch4.f \
       effave.f \
       effblo.f \
       emersi.f \
       ESpace.f90 \
       extina.f \
       extinc.f \
       flxfrc.f90 \
       harves.f \
       hdisp.f \
       hdispa.f \
       hdispv.f \
       heatfl.f \
       intpol.f \
       makpoc.f \
       meteo.f \
       methox.f \
       nh3fre.f \
       nitrif.f \
       nlalg.f \
       nralgs.f \
       nutrel.f \
       nutupt.f \
       oxymin.f \
       partmp.f \
       phcomp.f \
       phcomb.f \
       plastc.f90 \
       posoxy.f \
       pprlim.f \
       pripro.f \
       procal.f \
       pronrs.f \
       ptewor.f \
       radalg.f \
       rdbalg.f \
       rear.f \
       refl.f \
       resant.f \
       resdm.f \
       restim.f \
       rfpart.f \
       s12tra.f \
       s12tim.f \
       salchl.f \
       satch4.f \
       satco2.f \
       satoxy.f \
       sdppro.f \
       secchi.f \
       sedaap.f \
       sedcar.f \
       sedcom.f \
       sedhm.f \
       sedim.f \
       sednut.f \
       sedomv.f \
       sedox.f \
       sedsod.f \
       set_effi.f \
       simph.f \
       solvesaphe/mod_precision.f90 \
       solvesaphe/mod_chemconst.f90 \
       solvesaphe/mod_acbw_phsolvers.F90 \
       phcarb.f \
       selfcool.f \
       somsed.f \
       ssedph.f \
       staday.f \
       stadpt.f \
       stadsc.f \
       stageo.f \
       staprc.f \
       staqtl.f \
       stox3d.f \
       strear.f \
       sulfox.f \
       sulfpr.f \
       sumfrc.f90 \
       swoxy.f \
       temper.f \
       tempermode.f \
       tfalg.f \
       totdep.f \
       trase2.f \
       trcoef.f \
       trsoxy.f \
       uit_zi.f \
       ulfix.f \
       varoxy.f \
       varsal.f \
       veg2dn.f \
       veg3du.f \
       veg3dx.f \
       vervlu.f \
       vivian.f \
       vtrans.f \
       watage.f \
       wkcomp.f \
       wlcwoc.f \
       zerome.f \
       ironox.f90 \
       ironre.f90 \
       priron.f90 \
       spcarb.f90 \
       specfe.f90 \
       sulfid.f90 \
       sulpho.f90 \
       vbgro.f90 \
       vbmrt.f90 \
       vbstat.f90 \
       vbupt.f90 \
       veloc.f90 \
       macdis.f \
       npps12.f90 \
       mac3du.f90 \
       maxmac.f \
       macrop.f \
       radmac.f \
       grzmac.f \
       macnut.f90 \
       covmac.f \
       respup.f \
       dredge.f \
       floceq.f90

#-------------------------------------------------------------------------------
# Libraries
