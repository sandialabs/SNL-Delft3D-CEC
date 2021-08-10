@ echo off

setlocal enabledelayedexpansion

set globalErrorLevel=0
  
  rem Quiet removal of output file build.log. Do not report any problems with this deletion
del /F/Q build*.log > del.log 2>&1
del /F/Q del.log


  rem Set environment parameters for VisualStudio
call "%VS140COMNTOOLS%..\..\VC\vcvarsall.bat" amd64

  rem The path to devenv.exe is now added to PATH: no full path specificitation needed on next line.


rem ===========================================================================
rem io_netcdf.sln
rem We must use Any Cpu because we set the x64 environment to amd64 architecture. 
rem If we build the Managed DLL in x64 we create it with amd64 architecture which is incompatible 
rem to other managed dlls. Need to build the managed dll in MSIL. The kernel (io_netcdf) is still
rem build with amd64 architecture as it always is.
devenv.exe io_netcdf.sln /Build "Release|Any Cpu" /Out build_io_netcdf.log
if NOT %ErrorLevel% EQU 0 (
    echo "Error in compiling io_netcdf.sln: %ErrorLevel%"
    set globalErrorLevel=%ErrorLevel%
)

:finished

:end

if NOT %globalErrorLevel% EQU 0 (
    echo An error occurred in one or more compilation steps
    echo Returning with error number %globalErrorLevel%
    exit %globalErrorLevel%
)
