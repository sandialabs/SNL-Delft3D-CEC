@ echo off
    rem
    rem This script is an example for running DELWAQ D-Water Quality
    rem Adapt and use it for your own purpose
    rem
    rem michel.jeuken@deltares.nl
    rem 11 Mar 2013
    rem 
    rem
    rem This script starts a 3D DELWAQ D-Water Quality computation on Windows
    rem

    rem
    rem Set the config file here
    rem 
set inpfile=fti_delwaq-part.inp

set currentdir=%~dp0%
set argfile=%currentdir%%inpfile%

    rem
    rem Set the directory containing delwaq1.exe and delwaq2.exe and
    rem the directory containing the proc_def and bloom files here
    rem
set exedir=%currentdir%..\..\bin\win64\waq\bin
  rem set exedir=c:\Program Files (x86)\Deltares\Delft3D 4.01.00\win32\waq\bin
set procfile=%currentdir%..\..\bin\win64\waq\default\proc_def
  rem set procfile=c:\Program Files (x86)\Deltares\Delft3D 4.01.00\win32\waq\default\proc_def

    rem
    rem Run delwaq 1
    rem
echo.
"%exedir%\delwaq1.exe" "%argfile%" -p "%procfile%"

    rem
    rem Wait for any key to run delwaq 2
    rem
if %ERRORLEVEL% neq 0 (
    echo.
    echo Delwaq1 did not run correctly, ending calculation
    goto :end )
echo. 
echo Delwaq1 did run without errors.
echo.

    rem
    rem Run delwaq 2
    rem
echo.
"%exedir%\delwaq2.exe" "%argfile%"

if %ERRORLEVEL% neq 0 (
    echo.
    echo Delwaq2 did not run correctly
    goto :end )
echo. 
echo Delwaq2 did run without errors.

:end
echo.
