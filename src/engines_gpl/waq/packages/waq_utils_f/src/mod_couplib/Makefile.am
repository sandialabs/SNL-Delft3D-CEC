#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   26 aug 10
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libmod_couplib.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings

#-------------------------------------------------------------------------------
#Sources
libmod_couplib_la_SOURCES = \
       m_coupcns.F90 \
       m_ixset.F90 \
       m_prcgrp.F90 \
       m_intfc.F90 \
       m_sndrcv.F90  \
       m_globcomm.F90 \
       reshape_idata.F90 \
       reshape_rdata.F90 \
       m_couplib.F90 \
       reshape_ddata.F90

#-------------------------------------------------------------------------------
# Libraries
