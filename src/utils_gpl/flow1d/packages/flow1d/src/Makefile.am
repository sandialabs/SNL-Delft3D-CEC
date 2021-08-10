#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 sep 10
#
#   Copyright (C) 2007-2010, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libflow1d.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
  -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
  -I$(builddir)/../../flow1d_core/src \
  -I$(builddir)/../../flow1d_io/src

#-------------------------------------------------------------------------------
#Sources
libflow1d_la_SOURCES = \
  flow1d_api.F90

libflow1d_la_LIBADD=\
  $(builddir)/../../flow1d_core/src/libflow1d_core.la \
  $(builddir)/../../flow1d_io/src/libflow1d_io.la

# Do not include deltares_common here: leave that to toplevel exe/dll(so) projects,
# to prevent multiple definitions of the same routines.
# $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la
