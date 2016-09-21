#!/bin/sh
# This shell script runs the ncdump tests.
# $Id: run_tests.sh 5406 2015-09-10 14:52:05Z mourits $

echo "*** Testing ncgen and ncdump using test0.cdl."
set -e
echo "*** creating test0.nc from test0.cdl..."
../ncgen/ncgen -b $srcdir/test0.cdl
echo "*** creating test1.cdl from test0.nc..."
./ncdump -n test1 test0.nc > test1.cdl
echo "*** creating test1.nc from test1.cdl..."
../ncgen/ncgen -b test1.cdl
echo "*** creating test2.cdl from test1.nc..."
./ncdump test1.nc > test2.cdl
cmp test1.cdl test2.cdl
echo "*** All tests of ncgen and ncdump using test0.cdl passed!"
exit 0
