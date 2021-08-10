@ echo off
title run_flow2d3d
    rem
    rem This script runs Delft3D-FLOW on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    set argfile=config_d_hydro.xml
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set argfile=%1
    )
)
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)


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
echo executing: "%dflow2d3ddir%\d_hydro.exe" %argfile%
"%dflow2d3ddir%\d_hydro.exe" %argfile%

goto end

:usage
echo Usage:
echo run_flow2d3d.bat [--help] [config_d_hydro.xml]
echo     --help            : (Optional) show this usage
echo     config_d_hydro.xml: (Optional) default: config_d_hydro.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
