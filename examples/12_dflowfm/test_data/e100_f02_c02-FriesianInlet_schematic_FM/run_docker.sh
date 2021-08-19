#!/bin/bash

ulimit -s unlimited
export OMP_NUM_THREADS=1

/opt/delft3dfm_latest/lnx64/bin/run_dimr.sh
