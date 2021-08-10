@ echo off

setlocal enabledelayedexpansion

set globalErrorLevel=0
set d3d_open_only=0

  rem Jump to the directory where this build.cmd script is
cd %~dp0

  rem Single precision build (or forced high precision code conversion):
if [%1] EQU [/sp] (
    cd utils_lgpl\deltares_common\scripts
    call singleprecision.bat
    if NOT %ErrorLevel% EQU 0 (
        echo "Error in executing singleprecision.bat: %ErrorLevel%"
        set globalErrorLevel=%ErrorLevel%
        cd %~dp0
        goto end
    )
    cd %~dp0
)
if [%1] EQU [/hp] (
    cd utils_lgpl\deltares_common\scripts
    call doubleprecision.bat
    if NOT %ErrorLevel% EQU 0 (
        echo "Error in executing doubleprecision.bat: %ErrorLevel%"
        set globalErrorLevel=%ErrorLevel%
        cd %~dp0
        goto end
    )
    cd %~dp0
)
  
  rem Quiet removal of output file build.log. Do not report any problems with this deletion
del /F/Q build*.log > del.log 2>&1
del /F/Q del.log


  rem Set environment parameters for VisualStudio
call "%VS140COMNTOOLS%..\..\VC\vcvarsall.bat" amd64

  rem The path to devenv.exe is now added to PATH: no full path specification needed on next line.


rem ===========================================================================
rem dimr.sln
devenv.exe dimr_open.sln /Build "Release|x64" /Out build_dimr.log
if NOT %ErrorLevel% EQU 0 (
    echo "Error in compiling dimr_open.sln: %ErrorLevel%"
    set globalErrorLevel=%ErrorLevel%
)



:finished

  rem In build.log, replace "error" by TeamCity messages
third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build_dimr.log 


:end

if NOT %globalErrorLevel% EQU 0 (
    echo An error occurred in one or more compilation steps
    echo Returning with error number %globalErrorLevel%
    exit %globalErrorLevel%
)
