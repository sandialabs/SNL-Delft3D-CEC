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
noinst_LTLIBRARIES = libDelftIOSHM.la

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I$(top_srcdir)/utils_lgpl/delftio/packages/delftio_shm/include \
	-I$(top_srcdir)/utils_lgpl/esmfsm/include

#-------------------------------------------------------------------------------
#Sources
libDelftIOSHM_la_SOURCES = \
         dio-sync-ux.cpp \
         dio_shm.cpp \
         dio_shm_datablock.cpp \
         dio_shm_f2c_c.cpp \
         dio_shm_handle.cpp \
         dio_shm_sync.cpp

#-------------------------------------------------------------------------------
#Libraries
