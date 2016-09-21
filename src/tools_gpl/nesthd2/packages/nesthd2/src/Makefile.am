include $(top_srcdir)/common.am
bin_PROGRAMS = nesthd2

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
        -I$(top_srcdir)/tools_gpl/nesthd2/packages/nesthd2/include

nesthd2_SOURCES = \
        nesthd2_version.F90 \
        nesthd2.f90 \
        small.f90 \
        newlun.f90 \
        dattim.f90 \
        addinp.f \
        check.f90 \
        chkdry.f \
        corcon.f \
        detcon.f \
        d3stop.f \
        dethyd.f \
        dimbnd.f \
        files.f \
        getdim.f \
        getwgh.f \
        hyd2dh.f \
        nest_hd2.f \
        nefis.f \
        reabnd.f \
        wricon.f \
        wricon2.f \
        wrihyd.f \
        wrihyd2.f \
        wrista.f

#-------------------------------------------------------------------------------
#Libraries

nesthd2_LDADD = \
        $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
        $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
        -ldl \
        $(FCLIBS) $(CCLIBS)

BUILT_SOURCES = ./always \
	./nesthd2_version.F90

./always:
	rm -rf ./nesthd2_version.F90

./nesthd2_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/nesthd2_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)

