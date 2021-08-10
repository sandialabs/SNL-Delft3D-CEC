@ echo off
title run_dflow2d3d_parallel
    rem
    rem This script runs Delft3D-FLOW in parallel mode on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    set numpar=%NUMBER_OF_PROCESSORS%
    set argfile=config_d_hydro.xml
) else (
    if [%1] EQU [--help] (
        goto usage
    )
    set numpar=%1
    if [%2] EQU [] (
        set argfile=config_d_hydro.xml
    ) else (
        set argfile=%2
    )
)
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)

echo number of partitions: %numpar%

set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..\..\..

rem Remove "\dflow2d3d\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-27%
rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set dflow2d3ddir=%D3D_HOME%\%ARCH%\dflow2d3d\bin
set sharedir=%D3D_HOME%\%ARCH%\share\bin


    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%dflow2d3ddir%;%sharedir%
echo executing: "%sharedir%\mpiexec.exe" -n %numpar% -localonly "%dflow2d3ddir%\d_hydro.exe" %argfile%
"%sharedir%\mpiexec.exe" -n %numpar% -localonly "%dflow2d3ddir%\d_hydro.exe" %argfile%

goto end

:usage
echo Usage:
echo run_dflow2d3d_parallel.bat [--help] [n] [config_d_hydro.xml]
echo     --help         : (Optional) show this usage
echo     n              : (Optional) integer, number of partitions.
echo                      Default value: %NUMBER_OF_PROCESSORS%
echo     config_d_hydro.xml: (Optional) default: config_d_hydro.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
