@ echo off
title run_flow2d3d_dwaves
    rem
    rem This script runs Delft3D-FLOW online with Delft3D-WAVE on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

set argfile= 
set mdwfile= 
    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [-w] (
        set argfile=config_d_hydro.xml
        if [%2] EQU [] (
            goto usage
        )
        set mdwfile=%2
    ) else (
        if [%1] EQU [--help] (
            goto usage
        ) else (
            set argfile=%1
            if [%2] EQU [-w] (
                if [%3] EQU [] (
                    goto usage
                ) else (
                    set mdwfile=%3
                )
            ) else (
                goto usage
            )
        )
    )
)
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)
echo mdw-file:%mdwfile%
if not exist %mdwfile% (
    echo ERROR: mdw-file "%mdwfile%" does not exist
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
set swanbatdir=%D3D_HOME%\%ARCH%\swan\scripts
set waveexedir=%D3D_HOME%\%ARCH%\dwaves\bin


    rem
    rem No adaptions needed below
    rem

    rem Start FLOW
set PATH=%dflow2d3ddir%;%sharedir%
echo executing in separate window: "%dflow2d3ddir%\d_hydro.exe" %argfile%
start "Delft3D-FLOW" "%dflow2d3ddir%\d_hydro.exe" %argfile%

    rem Start WAVE
title Delft3D-WAVE simulation
set PATH=%waveexedir%;%swanbatdir%;%sharedir%
echo executing in this window: "%waveexedir%\wave.exe" %mdwfile% 1
"%waveexedir%\wave.exe" %mdwfile% 1
title %CD%

goto end

:usage
echo Usage:
echo "run_dflow2d3d_dwaves.bat [--help] [config_d_hydro.xml] -w <mdw-file>"
echo     --help            : (Optional) show this usage
echo     config_d_hydro.xml: (Optional) default: config_d_hydro.xml
echo     -w <mdw-file>     : (Mandatory) Delft3D-WAVE input file

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
