#! /bin/bash

export ARCH=lnx64
export HOSTNAME=$HOSTNAME


   # If the directory where this script is located contains a "mormerge.tcl": use that
   # Else use another one
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
scriptname=$scriptdir/mormerge.tcl
if [ ! -f $scriptname ]; then 
  scriptname=../../../bin/lnx64/flow2d3d/scripts/mormerge.tcl
fi

$scriptname -i basin_linux.mm -s $scriptname

