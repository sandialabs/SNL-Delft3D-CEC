include $(top_srcdir)/common.am
bin_PROGRAMS = nesthd1

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
        -I$(top_srcdir)/tools_gpl/nesthd1/packages/nesthd1/include

FMODULES += \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

nesthd1_SOURCES = \
        nesthd1_version.F90 \
        distance.f90 \
        nesthd1.f90 \
        small.f90 \
        newlun.f90 \
        dattim.f90 \
        detang.f \
        detnst.f \
        detxy.f \
        dimbnd.f \
        dimgrd.f \
        files.f \
        findmn.f \
        getdim.f \
        increm.f \
        inigrd_n.f \
        nearmn.f \
        pinpol.f \
        reabnd.f \
        reargf.f \
        wrinst.f \
        wrista.f

#-------------------------------------------------------------------------------
#Libraries

nesthd1_LDADD = \
        $(top_srcdir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
        $(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
        -ldl \
        $(FCLIBS) $(CCLIBS)


BUILT_SOURCES = ./always \
	./nesthd1_version.F90

./always:
	rm -rf ./nesthd1_version.F90

./nesthd1_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/nesthd1_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)
