
include $(top_srcdir)/common.am
#noinst_LTLIBRARIES = libODS.la
lib_LTLIBRARIES = libODS.la

#-------------------------------------------------------------------------------
#Sources

libODS_la_LIBADD = ../srcf/libODSf.la ../../../../nefis/packages/nefis/src_so/libNefisSO.la

# Language: C language
libODS_la_SOURCES =           \
		dlwbin.c     \
                dlwbingr.c   \
                dlwgrid.c    \
                dlwnef.c     \
                equal.c	     \
                geotext.c    \
                getdim.c     \
                getgrd.c     \
                getloc.c     \
                getmat.c     \
                getpar.c     \
		gettme.c     \
		getval.c     \
		gregor.c     \
		gregor.h     \
		gx_db3.c     \
		iftype.c     \
		itrans.c     \
		itrans.h     \
		jspost.c     \
		julian.c     \
		julian.h     \
		m3hbuf.c     \
		mappix.c     \
		morbagr.c    \
		morcom.c     \
		ods_bna.c    \
		ods_conv.c   \
		ods_gnf.c    \
		ods_spcf.c   \
		odserr.c     \
		odsver.c     \
		opnclose.c   \
		pharmap.c    \
		phidi2c.c    \
		readline.c   \
		samples.c    \
		shyfem.c     \
		tekasc.c     \
		tekasc2d.c   \
		tridro.c     \
		trinint.c    \
		utils.c	     \
		waspro.c

INCLUDES = -I${srcdir}/../include -I${srcdir}/../../../../nefis/packages/nefis/include -Dlinux
