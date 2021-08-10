@ echo off
title run_dimr_parallel
    rem
    rem This script runs dimr in parallel mode on Windows
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 03 Feb 2017
    rem 
    rem
setlocal enabledelayedexpansion
    rem debuglevel=0:silent 8:major 32:detail
set debuglevel=8

    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    set numpar=%NUMBER_OF_PROCESSORS%
    set argfile=dimr_config.xml
) else (
    if [%1] EQU [--help] (
        goto usage
    )
    set numpar=%1
    if [%2] EQU [] (
        set argfile=dimr_config.xml
    ) else (
        if [%2] EQU [-d] (
            set debuglevel=%3
            set argfile=%4
        ) else (
            set argfile=%2
            if [%3] EQU [-d] (
                set debuglevel=%4
            )
        )
    )
)
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)
rem Sets the number of threads if it is not defined
if defined OMP_NUM_THREADS (
echo OMP_NUM_THREADS is already defined
) else ( 
   rem Getting and setting the number of physical cores  
   for /F "tokens=2 delims==" %%C in ('wmic cpu get NumberOfCores /value ^| findstr NumberOfCores') do set NumberOfPhysicalCores=%%C
   set /A OMP_NUM_THREADS=%NumberOfPhysicalCores%-2
   if /I OMP_NUM_THREADS LEQ 2 set OMP_NUM_THREADS=2
)
echo OMP_NUM_THREADS is %OMP_NUM_THREADS%

echo number of partitions: %numpar%

set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..\..

rem Remove "\scripts\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-14%
rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set delwaqexedir=%D3D_HOME%\%ARCH%\dwaq\bin
set dflowfmexedir=%D3D_HOME%\%ARCH%\dflowfm\bin
set dimrexedir=%D3D_HOME%\%ARCH%\dimr\bin
set esmfexedir=%D3D_HOME%\%ARCH%\esmf\bin
set esmfbatdir=%D3D_HOME%\%ARCH%\esmf\scripts
set flow1dexedir=%D3D_HOME%\%ARCH%\dflow1d\bin
set flow1d2dexedir=%D3D_HOME%\%ARCH%\dflow1d2d\bin
set rrexedir=%D3D_HOME%\%ARCH%\drr\bin
set rtctoolsexedir=%D3D_HOME%\%ARCH%\drtc\bin
set swanexedir=%D3D_HOME%\%ARCH%\swan\bin
set swanbatdir=%D3D_HOME%\%ARCH%\swan\scripts
set shareddir=%D3D_HOME%\%ARCH%\shared
set waveexedir=%D3D_HOME%\%ARCH%\dwaves\bin


    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%dimrexedir%;%delwaqexedir%;%dflowfmexedir%;%flow1dexedir%;%flow1d2dexedir%;%rtctoolsexedir%;%rrexedir%;%waveexedir%;%swanbatdir%;%swanexedir%;%esmfbatdir%;%esmfexedir%;%shareddir%
echo executing: "%shareddir%\mpiexec.exe" -n %numpar% -localonly "%dimrexedir%\dimr.exe" -d %debuglevel% %argfile%
"%shareddir%\mpiexec.exe" -n %numpar% -localonly "%dimrexedir%\dimr.exe" -d %debuglevel% %argfile%

goto end

:usage
echo Usage:
echo run_dimr_parallel.bat [--help] [n] [-d debuglevel] [dimr_config.xml]
echo     --help         : (Optional) show this usage
echo     n              : (Optional) integer, number of partitions. Must match with the prepared D-Flow FM calculation.
echo                      Default value: %NUMBER_OF_PROCESSORS%
echo     -d debuglevel  : (optional) debuglevel=0:silent, 8:major(default), 32:detail
echo     dimr_config.xml: (Optional) default: dimr_config.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
