#-------------------------------------------------------------------------------
#   Makefile for ds/third_party
#
#   Adri.Mourits@deltares.nl
#   25 sep 2008
#
#   Copyright (C) 2008, Deltares
#
#   To enable compilation in third_party:
#   Add third_party to the SUBDIRS line in file ds/Makefile.am
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

# Descending into FortranGIS is optional, since its prerequisites
# --with-shapelib and --with-proj are optional.
if USEFORTRANGIS
  MAYBE_FORTRANGIS = fortrangis
endif

SUBDIRS = \
    esmf \
    triangle \
    version_number \
    libsigwatch \
    FLAP \
    polypack \
    kdtree2 \
    swan \
    md5 \
    $(MAYBE_FORTRANGIS)
