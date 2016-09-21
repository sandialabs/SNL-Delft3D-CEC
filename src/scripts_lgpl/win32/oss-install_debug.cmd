@echo off

echo oss-install_debug...

rem Example calls:
rem > install.cmd               # Install all dlls in the directory of the executable to be debugged
rem > install.cmd flow2d3d      # Install only project flow2d3d (and its dependencies)

rem 0. defaults:
set project=

rem  The next statement is needed in order for the set commands to work inside the if statement
setlocal enabledelayedexpansion

if [%1] EQU [] (
    rem Install all engines.

    set project=install_all
    echo Source          : all engines
) else (
    rem Install the package/engine specified by the first argument.

    set project=%1
    echo Source          : package/engine !project!
)

rem Change to directory tree where this batch file resides (necessary when oss-install.cmd is called from outside of oss/trunk/src)
cd %~dp0\..\..

call :!project!

goto end

rem  Actual install "routines"


rem ============================================================
rem === if the command before a call to handle_error returns ===
rem === an error, the script will return with an error       ===
rem ============================================================
:handle_error
    if NOT %ErrorLevel% EQU 0 (
        set globalErrorLevel=%ErrorLevel%
    )
    rem go back to call site
goto :endproc

rem =============================================================
rem === copyFile takes two arguments: the name of the file to ===
rem === copy to the destiny directory                         ===
rem ===                                                       ===
rem === NOTE: errors will be reported and the script will     ===
rem === with an error code after executing the rest of its    ===
rem === statements                                            ===
rem =============================================================
:copyFile
    set fileName=%~1
    set dest=%~2
    rem
    rem "echo f |" is (only) needed when dest does not exist
    rem and does not harm in other cases
    rem 
    echo f | xcopy "%fileName%" %dest% /F /Y
    if NOT !ErrorLevel! EQU 0 (
        echo ERROR: while copying "!fileName!" to "!dest!"
    )
    call :handle_error
goto :endproc

rem =============================================================
rem === copyNetcdf copy the appropriate netcdf.dll            ===
rem =============================================================
:copyNetcdf
    call :copyFile "third_party_open\netcdf\src\win32\2005\libsrc\Debug\netcdf.dll" !dest_bin!
goto :endproc


rem ===============
rem === INSTALL_ALL
rem ===============
:install_all
    echo "    installing all open source projects (debug) . . ."

    call :d_hydro
    call :flow2d3d
    call :flow2d3d_openda
	call :delwaq1
	call :delwaq1_lib
	call :delwaq2
	call :delwaq2_lib
	call :delwaq2_openda_lib
	call :waq_plugin_wasteload
    call :wave
    call :plugin_culvert
    call :plugin_delftflow_traform
    call :datsel
    call :kubint
    call :lint
    call :mormerge
    call :vs

goto :endproc



rem ==========================
rem === INSTALL_D_HYDRO
rem ==========================
:d_hydro
    echo "installing d_hydro . . ."
    set dest_bin="engines_gpl\d_hydro\bin\Debug"

    if not exist !dest_bin!     mkdir !dest_bin!
    
goto :endproc



rem ====================
rem === INSTALL_FLOW2D3D
rem ====================
:flow2d3d
    echo "installing flow2d3d . . ."

    set dest_bin="engines_gpl\d_hydro\bin\Debug"

    if not exist !dest_bin!     mkdir !dest_bin!

    copy engines_gpl\flow2d3d\bin\Debug\flow2d3d.dll                                     !dest_bin!
    copy engines_gpl\flow2d3d\bin\Debug\flow2d3d_sp.dll                                  !dest_bin!
       rem One of these two dlls will not exist and cause an ErrorLevel=1. Reset it.
    set ErrorLevel=0
    copy third_party_open\DelftOnline\lib\Debug\DelftOnline.dll                          !dest_bin!
    copy third_party_open\pthreads\bin\win32\pthreadVCE2.dll                             !dest_bin!
    copy third_party_open\pthreads\bin\win32\pthreadvce.dll                              !dest_bin!
    copy third_party_open\mpich2\bin\*.exe                                               !dest_bin!
    copy third_party_open\mpich2\lib\*.dll                                               !dest_bin!
    copy third_party_open\expat\win32\bin\Release\libexpat.dll                           !dest_bin!
    copy utils_lgpl\delftonline\lib\Debug\dynamic\delftonline.dll                        !dest_bin!
    call :copyNetcdf
goto :endproc



rem ===========================
rem === INSTALL_FLOW2D3D_OPENDA
rem ===========================
:flow2d3d_openda
    echo "installing flow2d3d_openda . . ."

    set dest_bin="engines_gpl\d_hydro\bin\Debug"

    if not exist !dest_bin!     mkdir !dest_bin!

    copy engines_gpl\flow2d3d\bin\Debug\flow2d3d_openda.dll                              !dest_bin!
    copy engines_gpl\flow2d3d\bin\Debug\flow2d3d_openda_sp.dll                           !dest_bin!
       rem One of these two dlls will not exist and cause an ErrorLevel=1. Reset it.
    set ErrorLevel=0
    copy third_party_open\DelftOnline\lib\Debug\DelftOnline.dll                          !dest_bin!
    copy third_party_open\pthreads\bin\win32\pthreadVCE2.dll                             !dest_bin!
    copy third_party_open\pthreads\bin\win32\pthreadvce.dll                              !dest_bin!
    copy third_party_open\mpich2\bin\*.exe                                               !dest_bin!
    copy third_party_open\mpich2\lib\*.dll                                               !dest_bin!
    copy third_party_open\expat\win32\bin\Release\libexpat.dll                           !dest_bin!
    copy third_party_open\netcdf\lib\win32\release\netcdf.dll                            !dest_bin!
    copy third_party_open\openda\core\native\lib\win32\*.dll                             !dest_bin!
    copy utils_lgpl\delftonline\lib\Debug\dynamic\delftonline.dll                        !dest_bin!
    call :copyNetcdf
goto :endproc



rem ===================
rem === INSTALL_DELWAQ1
rem ===================
:delwaq1
    echo "installing delwaq1 . . ."
    echo "... nothing to be done"
goto :endproc



rem =======================
rem === INSTALL_DELWAQ1_LIB
rem =======================
:delwaq1_lib
    echo "installing delwaq1_lib . . ."

    set dest_bin="engines_gpl\waq\bin\debug"
    
    if not exist !dest_bin!     mkdir !dest_bin!
	
    copy engines_gpl\waq\default\bloom.spe                           !dest_bin!
    copy engines_gpl\waq\default\bloominp.d09                        !dest_bin!
    copy engines_gpl\waq\default\proc_def.dat                        !dest_bin!
    copy engines_gpl\waq\default\proc_def.def                        !dest_bin!
goto :endproc



rem ===================
rem === INSTALL_DELWAQ2
rem ===================
:delwaq2
    echo "installing delwaq2 . . ."
    echo "... nothing to be done"
goto :endproc



rem =======================
rem === INSTALL_DELWAQ2_LIB
rem =======================
:delwaq2_lib
    echo "installing delwaq2_lib . . ."
    echo "... nothing to be done"
goto :endproc



rem ==============================
rem === INSTALL_DELWAQ2_OPENDA_LIB
rem ==============================
:delwaq2_openda_lib
    echo "installing delwaq2_openda_lib . . ."
    echo "... nothing to be done"
goto :endproc



rem ================================
rem === INSTALL_WAQ_PLUGIN_WASTELOAD
rem ================================
:waq_plugin_wasteload
    echo "installing waq_plugin_wasteload . . ."
    echo "... nothing to be done"
goto :endproc



rem ================
rem === INSTALL_WAVE
rem ================
:wave
    echo "installing wave . . ."

    set dest_bin="engines_gpl\wave\bin\Debug"

    if not exist !dest_bin!     mkdir !dest_bin!

    copy third_party_open\pthreads\bin\win32\pthreadVCE2.dll                             !dest_bin!
    copy third_party_open\pthreads\bin\win32\pthreadvce.dll                              !dest_bin!
goto :endproc



rem ==========================
rem === INSTALL_PLUGIN_CULVERT
rem ==========================
:plugin_culvert
    echo "installing plugin_culvert . . ."

    set dest_bin="engines_gpl\d_hydro\bin\Debug"

    if not exist !dest_bin!     mkdir !dest_bin!

    copy plugins_lgpl\plugin_culvert\bin\Debug\plugin_culvert.dll                        !dest_bin!
goto :endproc



rem ====================================
rem === INSTALL_PLUGIN_DELFTFLOW_TRAFORM
rem ====================================
:plugin_delftflow_traform
    echo "installing plugin_delftflow_traform . . ."

    set dest_bin="engines_gpl\d_hydro\bin\Debug"

    if not exist !dest_bin!     mkdir !dest_bin!

    copy plugins_lgpl\plugin_delftflow_traform\bin\Debug\plugin_delftflow_traform.dll    !dest_bin!
goto :endproc



rem ==================
rem === INSTALL_DATSEL
rem ==================
:datsel
    echo "installing datsel . . ."
    echo "... nothing to be done"
goto :endproc



rem ==================
rem === INSTALL_KUBINT
rem ==================
:kubint
    echo "installing kubint . . ."
    echo "... nothing to be done"
goto :endproc



rem ================
rem === INSTALL_LINT
rem ================
:lint
    echo "installing lint . . ."
    echo "... nothing to be done"
goto :endproc



rem ====================
rem === INSTALL_MORMERGE
rem ====================
:mormerge
    echo "installing mormerge . . ."
    echo "... nothing to be done"
goto :endproc



rem ==============
rem === INSTALL_VS
rem ==============
:vs
    echo "installing vs . . ."
    echo "... nothing to be done"
goto :endproc






:end
if NOT %ErrorLevel% EQU 0 (
    rem
    rem Only jump to :end when the script is completely finished
    rem 
    exit %ErrorLevel%
)

:endproc
   rem
   rem No exit here
   rem Otherwise the script exits directly at the first missing artefact
