#!/bin/bash

globalErrorLevel=0

# This script uses the command ldd to collect all dynamic libraries used:
gatherScript=scripts_lgpl/linux/gatherlibraries.rb
# The following libraries must be removed from the list created by gatherScript:
# - system dependent libraries in the directories /lib and /lib64
# - libraries generated in the oss tree itself
gatherExcludeFilter="-e '^/lib/' -e '^/lib64/' -e 'flow2d3d' -e 'DelftOnline'"
gatherIncludeFilter="-e 'expat' -e 'libssl' -e 'libcrypto'"

# ===============================
# === copyFile: handles error ===
# ===============================
function copyFile () {
    # This function can handle wild characters in the arguments,
    # as long as they are quoted
    # example: copyFile "bin/*" targetdir

    # handle the error
    for file in $1
    do
        eval cp -fp $file $2
        if [ $? != 0 ]; then
            echo "can't copy \"$file\" to \"$2\"" 1>&2
            globalErrorLevel=1
        fi
    done

    return
}

# ===================
# === INSTALL_ALL ===
# ===================
function install_all () {
    echo "installing all open source projects . . ."

    d_hydro
    flow2d3d
    # flow2d3d_openda is currently not added to the Linux installation
    #flow2d3d_openda
	part
    waq
    wave
    plugin_culvert
    plugin_delftflow_traform
    datsel
    kubint
    lint
    mormerge
    vs
    nesthd1
    nesthd2

    return
}



# ============================
# === INSTALL_DELFT3D_FLOW ===
# ============================
function delft3d_flow () {
    echo "installing delft3d-flow . . ."

    d_hydro
    flow2d3d
    # flow2d3d_openda is currently not added to the Linux installation
    #flow2d3d_openda
    plugin_culvert
    plugin_delftflow_traform
    mormerge
    vs

    return
}



# =======================
# === INSTALL_D_HYDRO ===
# =======================
function d_hydro () {
    echo "installing d_hydro . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"
    dest_menu="$dest_main/lnx64/menu/bin"

    mkdir -p $dest_bin
    mkdir -p $dest_menu

    copyFile "$prefix/bin/d_hydro.exe" 					    $dest_bin
    copyFile "$srcdir/engines_gpl/d_hydro/scripts/create_config_xml.tcl"    $dest_menu

    echo "Gathering libraries for d_hydro..."
    cp -u `$gatherScript $prefix/bin/d_hydro.exe | eval grep -v $gatherExcludeFilter` $dest_bin
    cp -u `$gatherScript $prefix/bin/d_hydro.exe | eval grep $gatherIncludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/d_hydro.exe

    return
}



# ========================
# === INSTALL FLOW2D3D ===
# ========================
function flow2d3d () {
    echo "installing flow2d3d . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"
    dest_default="$dest_main/lnx64/flow2d3d/default"
    dest_scripts="$dest_main/lnx64/flow2d3d/scripts"
    dest_plugins="$dest_main/lnx64/plugins/bin"

    mkdir -p $dest_bin
    mkdir -p $dest_default
    mkdir -p $dest_scripts
    mkdir -p $dest_plugins

    if [ -f $prefix/lib/libflow2d3d.so ]; then
        copyFile "$prefix/lib/libflow2d3d.so"                           $dest_bin
        # chrpath -r \$ORIGIN $dest_bin/libflow2d3d.so
    fi
    if [ -f $prefix/lib/libflow2d3d_sp.so ]; then
        copyFile "$prefix/lib/libflow2d3d_sp.so"                        $dest_bin
        # chrpath -r \$ORIGIN $dest_bin/libflow2d3d_sp.so
    fi
    # For some reason, libflow2d3d.so depends on libDelftOnline.so.0 instead of libDelftOnline.so. Both are links to libDelftOnline.so.0.0.0
    copyFile "$prefix/lib/libDelftOnline.so.0"                      $dest_bin
    copyFile "$prefix/lib/libDelftOnline.so.0"                      $dest_plugins
    copyFile "$srcdir/engines_gpl/flow2d3d/scripts/meteo_old2new.m" $dest_scripts
    copyFile "$prefix/bin/esm_create"                               $dest_bin
    copyFile "$prefix/bin/esm_delete"                               $dest_bin
    copyFile "$prefix/bin/esm_info"                                 $dest_bin
    copyFile "$srcdir/engines_gpl/flow2d3d/default/*"               $dest_default

    echo "Gathering libraries for flow2d3d..."
    cp -u `$gatherScript $prefix/lib/libflow2d3d*.so $prefix/lib/libDelftOnline.so $prefix/bin/esm_* | eval grep -v $gatherExcludeFilter` $dest_bin
    cp -u `$gatherScript $prefix/lib/libDelftOnline.so | eval grep -v $gatherExcludeFilter` $dest_plugins

    return
}



# ===============================
# === INSTALL FLOW2D3D_OPENDA ===
# ===============================
function flow2d3d_openda () {
    echo "installing flow2d3d_openda . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"
    dest_default="$dest_main/lnx64/flow2d3d/default"
    dest_scripts="$dest_main/lnx64/flow2d3d/scripts"
    dest_plugins="$dest_main/lnx64/plugins/bin"

    mkdir -p $dest_bin
    mkdir -p $dest_default
    mkdir -p $dest_scripts
    mkdir -p $dest_plugins

    if [ -f $prefix/lib/libflow2d3d_openda.so ]; then
        copyFile "$prefix/lib/libflow2d3d_openda.so"                    $dest_bin
        # chrpath -r \$ORIGIN $dest_bin/libflow2d3d_openda.so
    fi
    if [ -f $prefix/lib/libflow2d3d_openda_sp.so ]; then
        copyFile "$prefix/lib/libflow2d3d_openda_sp.so"                 $dest_bin
        # chrpath -r \$ORIGIN $dest_bin/libflow2d3d_openda_sp.so
    fi
    # For some reason, libflow2d3d.so depends on libDelftOnline.so.0 instead of libDelftOnline.so. Both are links to libDelftOnline.so.0.0.0
    copyFile "$prefix/lib/libDelftOnline.so.0"                      $dest_bin
    copyFile "$prefix/lib/libDelftOnline.so.0"                      $dest_plugins
    copyFile "$srcdir/engines_gpl/flow2d3d/scripts/meteo_old2new.m" $dest_scripts
    copyFile "$prefix/bin/esm_create"                               $dest_bin
    copyFile "$prefix/bin/esm_delete"                               $dest_bin
    copyFile "$prefix/bin/esm_info"                                 $dest_bin
    copyFile "$srcdir/engines_gpl/flow2d3d/default/*.*"             $dest_default

    echo "Gathering libraries for flow2d3d_openda..."
    cp -u `$gatherScript $prefix/lib/libflow2d3d_openda*.so $prefix/lib/libDelftOnline.so $prefix/bin/esm_* | eval grep -v $gatherExcludeFilter` $dest_bin
    cp -u `$gatherScript $prefix/lib/libDelftOnline.so | eval grep -v $gatherExcludeFilter` $dest_plugins

    return
}



# ======================
# === INSTALL DELWAQ ===
# ======================
function waq () {
    echo "installing delwaq . . ."

    dest_bin="$dest_main/lnx64/waq/bin"
    dest_default="$dest_main/lnx64/waq/default"

    mkdir -p $dest_bin
    mkdir -p $dest_default

    copyFile "$prefix/bin/delwaq1"                    $dest_bin
    copyFile "$prefix/bin/delwaq2"                    $dest_bin
    copyFile "$prefix/lib/libwaq_plugin_wasteload.so" $dest_bin
    copyFile "$srcdir/engines_gpl/waq/default/*"          $dest_default

    echo "Gathering libraries for delwaq..."
    cp -u `$gatherScript $prefix/bin/delwaq1 $prefix/bin/delwaq2 | eval grep -v $gatherExcludeFilter` $dest_bin
    cp -u `$gatherScript $prefix/bin/delwaq1 $prefix/bin/delwaq2 | eval grep $gatherIncludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/delwaq1
    # chrpath -r \$ORIGIN $dest_bin/delwaq2

    return
}



# ======================
# === INSTALL DELPAR ===
# ======================
function part () {
    echo "installing delpar . . ."

    dest_bin="$dest_main/lnx64/part/bin"

    mkdir -p $dest_bin

    copyFile "$prefix/bin/delpar"                    $dest_bin
    copyFile "$prefix/bin/delpar"                    $dest_bin

    echo "Gathering libraries for delpar..."
    cp -u `$gatherScript $prefix/bin/delpar | eval grep -v $gatherExcludeFilter` $dest_bin
    cp -u `$gatherScript $prefix/bin/delpar | eval grep $gatherIncludeFilter` $dest_bin

    return
}



# ====================
# === INSTALL WAVE ===
# ====================
function wave () {
    echo "installing wave . . ."

    dest_bin="$dest_main/lnx64/wave/bin"
    dest_default="$dest_main/lnx64/wave/default"
    dest_swan_bin="$dest_main/lnx64/swan/bin"
    dest_swan_scripts="$dest_main/lnx64/swan/scripts"

    mkdir -p $dest_bin
    mkdir -p $dest_default
    mkdir -p $dest_swan_bin
    mkdir -p $dest_swan_scripts

    copyFile "$prefix/bin/wave.exe"                                  $dest_bin
    copyFile "$srcdir/engines_gpl/flow2d3d/default/dioconfig.ini"    $dest_default
    copyFile "$srcdir/third_party_open/swan/bin/linux/*.*"           $dest_swan_bin
    copyFile "$srcdir/third_party_open/swan/scripts/swan_install.sh" $dest_swan_scripts/swan.sh

    echo "Gathering libraries for wave..."
    cp -u `$gatherScript $prefix/bin/wave.exe | eval grep -v $gatherExcludeFilter` $dest_bin
    echo "Gathering libraries for swan..."
    cp -u `$gatherScript $srcdir/third_party_open/swan/bin/linux/*.exe | eval grep -v $gatherExcludeFilter` $dest_swan_bin

    # chrpath -r \$ORIGIN $dest_bin/wave.exe

    return
}



# ==============================
# === INSTALL PLUGIN_CULVERT ===
# ==============================
function plugin_culvert () {
    echo "installing plugin_culvert . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"

    mkdir -p $dest_bin

    copyFile "$prefix/lib/libplugin_culvert.so" $dest_bin/plugin_culvert.so

    echo "Gathering libraries for plugin_culvert..."
    cp -u `$gatherScript $prefix/lib/libplugin_culvert.so | eval grep -v $gatherExcludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/plugin_culvert.so

    return
}



# ========================================
# === INSTALL PLUGIN_DELFTFLOW_TRAFORM ===
# ========================================
function plugin_delftflow_traform () {
    echo "installing plugin_delftflow_traform . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"

    mkdir -p $dest_bin

    copyFile "$prefix/lib/libplugin_delftflow_traform.so" $dest_bin/plugin_delftflow_traform.so

    echo "Gathering libraries for plugin_delftflow_traform..."
    cp -u `$gatherScript $prefix/lib/libplugin_delftflow_traform.so | eval grep -v $gatherExcludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/plugin_delftflow_traform.so

    return
}



# ======================
# === INSTALL DATSEL ===
# ======================
function datsel () {
    echo "installing datsel . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"

    mkdir -p $dest_bin

    copyFile "$prefix/bin/datsel" $dest_bin

    echo "Gathering libraries for datsel..."
    cp -u `$gatherScript $prefix/bin/datsel | eval grep -v $gatherExcludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/datsel

    return
}



# ======================
# === INSTALL KUBINT ===
# ======================
function kubint () {
    echo "installing kubint . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"

    mkdir -p $dest_bin

    copyFile "$prefix/bin/kubint" $dest_bin

    echo "Gathering libraries for kubint..."
    cp -u `$gatherScript $prefix/bin/kubint | eval grep -v $gatherExcludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/kubint

    return
}



# ====================
# === INSTALL LINT ===
# ====================
function lint () {
    echo "installing lint . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"

    mkdir -p $dest_bin

    copyFile "$prefix/bin/lint" $dest_bin

    echo "Gathering libraries for lint..."
    cp -u `$gatherScript $prefix/bin/lint | eval grep -v $gatherExcludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/lint

    return
}



# ========================
# === INSTALL MORMERGE ===
# ========================
function mormerge () {
    echo "installing mormerge . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"
    dest_scripts="$dest_main/lnx64/flow2d3d/scripts"

    mkdir -p $dest_bin
    mkdir -p $dest_scripts

    copyFile "$srcdir/engines_gpl/flow2d3d/scripts/mormerge.tcl" $dest_scripts
    copyFile "$prefix/bin/mormerge.exe"                          $dest_bin

    echo "Gathering libraries for mormerge..."
    cp -u `$gatherScript $prefix/bin/mormerge.exe | eval grep -v $gatherExcludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/mormerge.exe

    return
}



# ==================
# === INSTALL VS ===
# ==================
function vs () {
    echo "installing vs . . ."

    dest="$dest_main/lnx64/util/bin"

    mkdir -p $dest

    copyFile "$prefix/bin/vs" $dest

    echo "Gathering libraries for vs..."
    cp -u `$gatherScript $prefix/bin/vs | eval grep -v $gatherExcludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest/vs

    return
}



# =======================
# === INSTALL NESTHD1 ===
# =======================
function nesthd1 () {
    echo "installing nesthd1 . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"

    mkdir -p $dest_bin

    copyFile "$prefix/bin/nesthd1" $dest_bin

    echo "Gathering libraries for nesthd1..."
    cp -u `$gatherScript $prefix/bin/nesthd1 | eval grep -v $gatherExcludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/nesthd1

    return
}



# =======================
# === INSTALL NESTHD2 ===
# =======================
function nesthd2 () {
    echo "installing nesthd2 . . ."

    dest_bin="$dest_main/lnx64/flow2d3d/bin"

    mkdir -p $dest_bin

    copyFile "$prefix/bin/nesthd2" $dest_bin

    echo "Gathering libraries for nesthd2..."
    cp -u `$gatherScript $prefix/bin/nesthd2 | eval grep -v $gatherExcludeFilter` $dest_bin

    # chrpath -r \$ORIGIN $dest_bin/nesthd2

    return
}



# ============
# === MAIN ===
# ============

echo oss-install...

# Example calls:
# > install.cmd <prefix> <dest directory>              # Install entire solution
# > install.cmd <prefix> <dest directory> flow2d3d     # Install only project flow2d3d (and its dependencies)

# 0. defaults:
prefix=$1
dest_main=$2
project=$3
srcdir=

if [ "$prefix" == '' ]; then
    echo "ERROR: No prefix directory specified as argument of oss-install.sh"
    exit 1
fi

if [ "$dest_main" == '' ]; then
    echo "ERROR: No destination directory specified as argument of oss-install.sh"
    exit 1
fi

if [ "$project" == '' ]; then
    # Install all engines
    project=install_all
fi

echo Prefix          : $prefix
echo Target directory: $dest_main
echo Project         : $project


# Change to directory tree where this batch file resides (necessary when oss-install.sh is called from outside of oss/trunk/src)
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
cd $scriptdir/../..
srcdir=`pwd`

$project

cd $srcdir


exit $globalErrorLevel

