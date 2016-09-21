#! /bin/bash

#-------------------------------------------------------------------------------
#   Top-Level Clean Script for d_hydro
#
#   A "make distclean" doesn't do it all (yet), so we help it.
#
#   Irv.Elshoff@Deltares.NL
#   28 jun 12
#-------------------------------------------------------------------------------


if [ -f Makefile ]; then
    make ds-clean
fi


find . \
    -name Makefile.in  -o \
    -name .deps -o \
    -name version_number.h -o \
    -name '[0-9]' -o \
    -name '[0-9][0-9]' -o \
    -name '[0-9][0-9][0-9]' -o \
    -name '[0-9][0-9][0-9][0-9]' \
        | xargs rm -rf

find . \
    -name Makefile \
        |   while read f; do
                if [ -f $f.am ]; then
                    rm -f $f
                fi
            done

rm -rf \
    ../bin \
    aclocal.m4 \
    autom4te.cache \
    bin \
    config \
    config.h.in \
    config.log \
    configure \
    engines_gpl/d_hydro/packages/d_hydro/include/d_hydro_version.h \
    engines_gpl/flow2d3d/packages/flow2d3d/include/flow2d3d_version.h \
    engines_gpl/flow2d3d/packages/flow2d3d_openda/include/flow2d3d_openda_version.h \
    engines_gpl/waq/packages/delwaq1_version_number/include/version_number.h \
    engines_gpl/waq/packages/delwaq2_version_number/include/version_number.h \
    engines_gpl/wave/packages/wave/src/wave_version.F90 \
    lib \
    logs \
    m4/libtool.m4 \
    m4/ltoptions.m4 \
    m4/ltsugar.m4 \
    m4/ltversion.m4 \
    m4/lt~obsolete.m4 \
    third_party_open/version_number/bin/linux/version_number.exe \
    tools_gpl/datsel/packages/datsel_version_number/include/version_number.h \
    tools_gpl/kubint/packages/kubint_version_number/include/version_number.h \
    tools_gpl/lint/packages/lint_version_number/include/version_number.h \
    tools_gpl/mormerge/packages/mormerge_version_number/include/version_number.h \
    tools_gpl/nesthd1/packages/nesthd1/src/nesthd1_version.F90 \
    tools_gpl/nesthd2/packages/nesthd2/src/nesthd2_version.F90 \
    tools_gpl/vs/packages/vs_version_number/include/version_number.h \
    tools_gpl/vs/packages/vs/src/vs_l.c \
    tools_gpl/vs/packages/vs/src/vs_y.c \
    tools_gpl/vs/packages/vs/src/vs_y.h \
    tools_gpl/waq_run_processes/packages/waq_run_processes_version_number/include/version_number.h \
    utils_lgpl/d_hydro_lib/packages/d_hydro_lib/include/d_hydro_lib_version.h \
    utils_lgpl/delftio/packages/delftio/src/diof90/delftio_version.F90 \
    utils_lgpl/delftonline/src/delftonline/delftonline_version.h \
    utils_lgpl/deltares_common/packages/deltares_common/src/deltares_common_version.F90 \
    utils_lgpl/ec_module/packages/ec_module/src/ec_module_version.F90 \
    utils_lgpl/esmfsm/packages/esmfsm_version_number/include/version_number.h \
    utils_lgpl/nefis/packages/nefis/include/nefis_version.h \

