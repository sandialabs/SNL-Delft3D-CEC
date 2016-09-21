#!/bin/bash

# Example calls:
# > install.cmd <prefix>               # Install entire solution
# > install.cmd <prefix> flow2d3d      # Install only project flow2d3d (and its dependencies)


prefix=
project=
srcdir=
dest_main=

if [ "$1" == '' ]; then
    echo "ERROR: No prefix directory specified as argument of install.sh"
    exit 1
fi
prefix=$1

if [ "$2" == '' ]; then
    # Install all engines
    project=install_all
else
    project=$2
fi

# Change to directory where this batch file resides (necessary when oss-install.sh is called from outside of oss/trunk/src)
# This is assumed to be srcdir
curdir=`pwd`
scriptdirname=`readlink \-f \$0`
srcdir=`dirname $scriptdirname`
cd $srcdir

# Set dest_main
if [ "$prefix" == "$srcdir" ]; then
    # This was the default. Changing this to "dest_main=$prefix" will cause too much confusion.
    dest_main=$prefix/../bin
else
    # Also a bit awkward: we are going to copy binaries from $prefix/bin and $prefix/lib to $prefix/lnx.
    dest_main=$prefix
fi


scripts_lgpl/linux/oss-install.sh $prefix $dest_main $project

cd $curdir

exit 0
