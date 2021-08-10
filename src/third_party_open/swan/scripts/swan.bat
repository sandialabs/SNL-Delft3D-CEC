@echo off

if exist swan_bat.log del swan_bat.log
@echo screen output of swan.bat is written to this file >swan_bat.log
@echo and will be overwritten everytime that swan.bat is executed >>swan_bat.log
@echo >>swan_bat.log

set swanexec=%~dp0\..\bin\swan_omp.exe
set PATH=%~dp0\..\bin;%PATH%

rem
set OMP_NUM_THREADS_BACKUP=%OMP_NUM_THREADS%
rem swan40.72AB and newer runs parallel, using the total number of cores on the machine by default
rem Two ways to force the number of parallel processes:
rem 1. Define environment parameter OMP_NUM_THREADS_SWAN with the correct number of processes
rem 2. Put a number behind the =-sign on the line "set OMP_NUM_THREADS=" below
if "%OMP_NUM_THREADS_SWAN%" == "" (
    set OMP_NUM_THREADS=
) else (
    set OMP_NUM_THREADS=%OMP_NUM_THREADS_SWAN%
)

@echo SWAN batchfile executed for Delft3D >>swan_bat.log
@echo Using swan.bat in directory %~dp0 >>swan_bat.log
@echo Using %swanexec% >>swan_bat.log
@echo Performing wave computation for: %1.swn >>swan_bat.log

if exist PRINT (
del PRINT >>swan_bat.log 2>&1
)
if exist INPUT (
del INPUT >>swan_bat.log 2>&1
)
if exist swaninit (
del swaninit >>swan_bat.log 2>&1
)
if exist Errfile (
del Errfile >>swan_bat.log 2>&1
)
if exist errpts (
del errpts >>swan_bat.log 2>&1
)
if exist %1.erf (
del %1.erf >>swan_bat.log 2>&1
)
if exist %1.erp (
del %1.erp >>swan_bat.log 2>&1
)
if not exist %1.swn goto error1
if not exist "%swanexec%" goto error2
copy %1.swn INPUT >>swan_bat.log 2>&1

"%swanexec%" >>swan_bat.log

copy PRINT %1.prt >>swan_bat.log 2>&1
if exist errfile (
copy errfile %1.erf >>swan_bat.log 2>&1
)
if exist errpts (
copy errpts %1.erp >>swan_bat.log 2>&1
)
if exist swaninit (
del swaninit >>swan_bat.log 2>&1
)
goto finish
:error1
@echo >>swan_bat.log
@echo     ************************************************************** >>swan_bat.log
@echo                SWAN input file %1.swn does not exist >>swan_bat.log
@echo     ************************************************************** >>swan_bat.log
rem pause
goto finish
:error2
@echo >>swan_bat.log
@echo     ************************************************************** >>swan_bat.log
@echo                SWAN executable does not exist >>swan_bat.log
@echo                (%swanexec%) >>swan_bat.log
@echo     ************************************************************** >>swan_bat.log
rem pause
goto finish
:finish
set OMP_NUM_THREADS=%OMP_NUM_THREADS_BACKUP%

@echo on
rem exit
