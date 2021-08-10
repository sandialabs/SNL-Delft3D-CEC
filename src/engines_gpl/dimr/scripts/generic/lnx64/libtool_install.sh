#!/bin/bash

function fcheck {
    if [ ! -f "$1" ]; then
        echo "ERROR: file $1 does not exist"
        exit 1
    fi
}

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
cd $scriptdir/../lib
workdir=`pwd`
libtooldir=$scriptdir

echo "Workdir:$workdir"
mkdir tmp
mkdir tmp/.libs
cp libdimr.la tmp/
mv libdimr.* tmp/.libs/
cp ../share/delft3d/libdimr.lai tmp/.libs/
cd tmp
$libtooldir/libtool --mode=install install -c libdimr.la `pwd`/../libdimr.la
$libtooldir/libtool --finish `pwd`/../libdimr.la
cp .libs/libdimr.lai ..

cd ..
rm -rf tmp

fcheck libdimr.a
fcheck libdimr.la
fcheck libdimr.lai
fcheck libdimr.so
fcheck libdimr.so.0
fcheck libdimr.so.0.0.0

echo "libtool_install.sh finished normally"

