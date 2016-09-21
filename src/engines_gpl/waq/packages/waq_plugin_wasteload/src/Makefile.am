#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libwaq_plugin_wasteload.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
libwaq_plugin_wasteload_la_SOURCES = delwaq_user_wasteloads.f \
       srstop.f \
       zoekns.f

#-------------------------------------------------------------------------------
# Libraries
