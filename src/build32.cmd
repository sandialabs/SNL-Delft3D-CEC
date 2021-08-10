@ echo off

  rem Jump to the directory where this build.cmd script is
cd %~dp0

  rem Single precision build (or forced high precision code conversion):
if [%1] EQU [/sp] (
    cd utils_lgpl\deltares_common\scripts
    call singleprecision.bat
    cd %~dp0
)
if [%1] EQU [/hp] (
    cd utils_lgpl\deltares_common\scripts
    call doubleprecision.bat
    cd %~dp0
)
  
  rem Quiet removal of output file build.log. Do not report any problems with this deletion
del /F/Q build.log > del.log 2>&1
del /F/Q del.log


  rem Set environment parameters for VisualStudio
call "%VS110COMNTOOLS%..\..\VC\vcvarsall.bat" amd64

  rem The path to devenv.exe is now added to PATH: no full path specification needed on next line.
devenv.exe delft3d_open.sln /Build "Release|Win32" /Out build.log
devenv.exe io_netcdf.sln /Build "Release|Win32" /Out build.log

  rem In build.log, replace "error" by TeamCity messages
third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build.log 
