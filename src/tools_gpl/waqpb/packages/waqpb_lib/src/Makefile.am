#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libwaqpb.la

AM_FCFLAGS +=  $(FINCLUDES) $(FMODULES)
AM_FFLAGS  +=  $(FINCLUDES) $(FMODULES)


#-------------------------------------------------------------------------------
#Includes


FMODULES += \
        -I../../include

#-------------------------------------------------------------------------------
#Sources
libwaqpb_la_SOURCES = \
        ch1to1.f \
        chksto.f \
        cldept.f \
        clrcar.f \
        coefed.f \
        dattim.f \
        dhdelf.f \
        finuni.f \
        makind.f \
        pdfnef.f \
        readdb.f \
        sortst.f \
        upditm.f \
        wridoc.f \
        wripdf.f \
        wrisub.f \
        writex.f \
        writrm.f \
        wr_filid.f \
        wr_indic.f \
        wr_tabm1.f \
        wr_tabp1.f \
        wr_tabp2.f \
        wr_tabp3.f \
        wr_tabp4.f \
        wr_tabp5.f \
        wr_tabr1.f \
        wr_tabr2.f \
        wr_tabr3.f \
        wr_tabr4.f \
        wr_tabr5.f \
        wr_tabr6.f \
        wr_tabr7.f \
        wr_tabr8.f \
        wr_tabr9.f \
        zoek.f

#-------------------------------------------------------------------------------
# Libraries
