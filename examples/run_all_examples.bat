@ echo off

   rem Relative from INSIDE a testcase:
set TCL_EXE=..\..\src\third_party_open\tcl\bin\win32\tclkit.exe


echo "Running testcase 01_standard ..."
cd 01_standard
call run_flow2d3d.bat >screen.log 2>&1


echo "Running testcase 01_standard parallel ..."
call run_flow2d3d_parallel.bat >screen_parallel.log 2>&1
cd ..


echo "Running testcase 02_domaindecomposition ..."
cd 02_domaindecomposition
call run_flow2d3d.bat >screen.log 2>&1
cd ..


echo "Running testcase 03_flow-wave ..."
cd 03_flow-wave
call run_flow2d3d.bat >screen.log 2>&1


echo "Running testcase 03_flow-wave parallel ..."
call run_flow2d3d_parallel_wave.bat >screen_parallel.log 2>&1
cd ..


echo "Running testcase 04_fluidmud ..."
cd 04_fluidmud
call run_flow2d3d_flm.bat >screen.log 2>&1
cd ..


echo "Running testcase 05_mormerge ..."
cd 05_mormerge\merge
call run_flow2d3d_wave_mormerge.bat >screen.log 2>&1
cd ..\..


echo "Running testcase 06_delwaq ..."
cd 06_delwaq
call run_delwaq.bat >screen.log 2>&1
cd ..


echo "Running testcase 07_wave ..."
cd 07_wave
call run_wave.bat >screen.log 2>&1
cd ..


echo "Running testcase 08_part-tracer ..."
cd 08_part-tracer
call run_part.bat >screen.log 2>&1
cd ..


echo "Running testcase 09_part-oil ..."
cd 09_part-oil
call run_part.bat >screen.log 2>&1
cd ..


echo "Running testcase 10_delwaq-part-tracer ..."
cd 10_delwaq-part-tracer
call run_delwaq_part.bat >screen.log 2>&1
cd ..


echo "Running testcase 11_standard_netcdf ..."
cd 11_standard_netcdf
call run_flow2d3d.bat >screen.log 2>&1
cd ..

echo ...finished
pause
