#!/bin/bash


echo "Running testcase 01_standard ..."
cd 01_standard
./run_flow2d3d.sh >screen.log 2>&1


echo "Running testcase 01_standard parallel ..."
./run_flow2d3d_parallel.sh >screen_parallel.log 2>&1
cd ..


echo "Running testcase 02_domaindecomposition ..."
cd 02_domaindecomposition
./run_flow2d3d.sh >screen.log 2>&1
cd ..


echo "Running testcase 03_flow-wave ..."
cd 03_flow-wave
./run_flow2d3d.sh >screen.log 2>&1


echo "Running testcase 03_flow-wave parallel ..."
./run_flow2d3d_parallel_wave.sh >screen_parallel.log 2>&1
cd ..


echo "Running testcase 04_fluidmud ..."
cd 04_fluidmud
./run_flow2d3d_flm.sh >screen.log 2>&1
cd ..


echo "Running testcase 05_mormerge ..."
cd 05_mormerge/merge
./run_flow2d3d_wave_mormerge.sh >screen.log 2>&1
cd ../..


echo "Running testcase 06_delwaq ..."
cd 06_delwaq
./run_delwaq.sh >screen.log 2>&1
cd ..


echo "Running testcase 07_wave ..."
cd 07_wave
./run_wave.sh >screen.log 2>&1
cd ..


echo "Running testcase 08_part-tracer ..."
cd 08_part-tracer
./run_part.sh >screen.log 2>&1
cd ..


echo "Running testcase 09_part-oil ..."
cd 09_part-oil
./run_part.sh >screen.log 2>&1
cd ..


echo "Running testcase 10_delwaq-part-tracer ..."
cd 10_delwaq-part-tracer
./run_delwaq_part.sh >screen.log 2>&1
cd ..


echo "Running testcase 11_standard_netcdf ..."
cd 11_standard_netcdf
./run_flow2d3d.sh >screen.log 2>&1
cd ..


echo ...finished

