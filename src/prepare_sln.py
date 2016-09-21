import argparse
import os
import sys
import shutil
if sys.version_info<(3,0,0):
   # To avoid problems with encoding:
   # - Use codecs.open instead of open (Python 2.x only)
   # - open files with encoding='utf-8' (Both Python 2.x and 3.x)
   # - Do not use str(line) on lines read from file
   from codecs import open as open
   from Tkinter import *
else:
   from tkinter import *

#
# This script can be used to convert the VisualStudio solution and project files
# to the proper version. It is only intended to be used for:
# https://svn.oss.deltares.nl/repos/delft3d
# Adapt and use it for your own purpose.
#
# adri.mourits@deltares.nl
# 21 Oct 2015
#
# Usage:
# 1. Install Python on your machine https://www.python.org/downloads/
#    In case Python does not work: check that the path to "python.exe" is added to your environment parameter "PATH"
# 2. Execute "prepare_sln.py" (double-clicking it in your file browser will do). A window will pop-up in which you can select the
#    VisualStudio version you are using and the Intel Fortran compiler version
#    Optional usage without pop-up window:
#    python prepare_sln.py -vs 2013 -ifort 15
# 3. Click the "Apply" button
#    Solution file "delft3d_open.sln" will be created,
#    All project files it refers to will be updated
# 4. Open the sln file in VisualStudio and "Build Solution"
# 5. If this script does not behave as expected: Please report the problem and help to improve this script.
#


vs = -999
ifort = -999

#
# libdir specifies the directory containing the ifort compiler libraries
# The string to be added can be set depending on:
# - fortran/c project file to place it in
# - ifort version
# - 32/64 bit settings
libdir = {}
libdir["fortran1132"] = "$(IFORT_COMPILER11)\\compiler\\lib\\ia32"
libdir["c1132"] = libdir["fortran1132"]
libdir["fortran1164"] = "$(IFORT_COMPILER11)\\compiler\\lib\\intel64"
libdir["c1164"] = libdir["fortran1164"]
libdir["fortran1232"] = "$(IFORT_COMPILER12)\\compiler\\lib\\ia32"
libdir["c1232"] = libdir["fortran1232"]
libdir["fortran1264"] = "$(IFORT_COMPILER12)\\compiler\\lib\\intel64"
libdir["c1264"] = libdir["fortran1264"]
libdir["fortran1332"] = "$(IFORT_COMPILER13)\\compiler\\lib\\ia32"
libdir["c1332"] = libdir["fortran1332"]
libdir["fortran1364"] = "$(IFORT_COMPILER13)\\compiler\\lib\\intel64"
libdir["c1364"] = libdir["fortran1364"]
libdir["fortran1432"] = "$(IFORT_COMPILER14)\\compiler\\lib\\ia32"
libdir["c1432"] = libdir["fortran1432"]
libdir["fortran1464"] = "$(IFORT_COMPILER14)\\compiler\\lib\\intel64"
libdir["c1464"] = libdir["fortran1464"]
libdir["fortran1532"] = "$(IFORT_COMPILER15)\\compiler\\lib\\ia32"
libdir["c1532"] = libdir["fortran1532"]
libdir["fortran1564"] = "$(IFORT_COMPILER15)\\compiler\\lib\\intel64"
libdir["c1564"] = libdir["fortran1564"]
libdir["fortran1632"] = "$(IFORT_COMPILER16)\\compiler\\lib\\ia32"
libdir["c1632"] = libdir["fortran1632"]
libdir["fortran1664"] = "$(IFORT_COMPILER16)\\compiler\\lib\\intel64"
libdir["c1664"] = libdir["fortran1664"]
libdir["fortran1732"] = "$(IFORT_COMPILER17)\\compiler\\lib\\ia32"
libdir["c1732"] = libdir["fortran1732"]
libdir["fortran1764"] = "$(IFORT_COMPILER17)\\compiler\\lib\\intel64"
libdir["c1764"] = libdir["fortran1764"]

#
#
# redistdir specifies the directory containing the ifort redistributable dlls
# The string to be added can be set depending on:
# - fortran/c project file to place it in
# - ifort version
# - 32/64 bit settings
redistdir = {}
redistdir["fortran1132"] = "$(IFORT_COMPILER11)redist\\ia32\\compiler\\&quot"
redistdir["c1132"] = "$(IFORT_COMPILER11)redist\\ia32\\compiler\\"
redistdir["fortran1164"] = "$(IFORT_COMPILER11)redist\\intel64\\compiler\\&quot"
redistdir["c1164"] = "$(IFORT_COMPILER11)redist\\intel64\\compiler\\"
redistdir["fortran1232"] = "$(IFORT_COMPILER12)redist\\ia32\\compiler\\&quot"
redistdir["c1232"] = "$(IFORT_COMPILER12)redist\\ia32\\compiler\\"
redistdir["fortran1264"] = "$(IFORT_COMPILER12)redist\\intel64\\compiler\\&quot"
redistdir["c1264"] = "$(IFORT_COMPILER12)redist\\intel64\\compiler\\"
redistdir["fortran1332"] = "$(IFORT_COMPILER13)redist\\ia32\\compiler\\&quot"
redistdir["c1332"] = "$(IFORT_COMPILER13)redist\\ia32\\compiler\\"
redistdir["fortran1364"] = "$(IFORT_COMPILER13)redist\\intel64\\compiler\\&quot"
redistdir["c1364"] = "$(IFORT_COMPILER13)redist\\intel64\\compiler\\"
redistdir["fortran1432"] = "$(IFORT_COMPILER14)redist\\ia32\\compiler\\&quot"
redistdir["c1432"] = "$(IFORT_COMPILER14)redist\\ia32\\compiler\\"
redistdir["fortran1464"] = "$(IFORT_COMPILER14)redist\\intel64\\compiler\\&quot"
redistdir["c1464"] = "$(IFORT_COMPILER14)redist\\intel64\\compiler\\"
redistdir["fortran1532"] = "$(IFORT_COMPILER15)redist\\ia32\\compiler\\&quot"
redistdir["c1532"] = "$(IFORT_COMPILER15)redist\\ia32\\compiler\\"
redistdir["fortran1564"] = "$(IFORT_COMPILER15)redist\\intel64\\compiler\\&quot"
redistdir["c1564"] = "$(IFORT_COMPILER15)redist\\intel64\\compiler\\"
redistdir["fortran1632"] = "$(IFORT_COMPILER16)redist\\ia32\\compiler\\&quot"
redistdir["c1632"] = "$(IFORT_COMPILER16)redist\\ia32\\compiler\\"
redistdir["fortran1664"] = "$(IFORT_COMPILER16)redist\\intel64\\compiler\\&quot"
redistdir["c1664"] = "$(IFORT_COMPILER16)redist\\intel64\\compiler\\"
redistdir["fortran1732"] = "$(IFORT_COMPILER17)redist\\ia32\\compiler\\&quot"
redistdir["c1732"] = "$(IFORT_COMPILER17)redist\\ia32\\compiler\\"
redistdir["fortran1764"] = "$(IFORT_COMPILER17)redist\\intel64\\compiler\\&quot"
redistdir["c1764"] = "$(IFORT_COMPILER17)redist\\intel64\\compiler\\"

#
#
# toolsversion specifies the vs toolsversion number
toolsversion = {}
toolsversion[2010] = "4.0"
toolsversion[2012] = "4.0"
toolsversion[2013] = "12.0"
toolsversion[2014] = "12.0"
toolsversion[2015] = "14.0"
toolsversion[2016] = "14.0"
toolsversion[2017] = "14.0"

#
#
# platformtoolset specifies the vs platformtoolset version number
platformtoolset = {}
platformtoolset[2010] = ""
platformtoolset[2012] = "    <PlatformToolset>v110</PlatformToolset>"
platformtoolset[2013] = "    <PlatformToolset>v120</PlatformToolset>"
platformtoolset[2014] = "    <PlatformToolset>v120</PlatformToolset>"
platformtoolset[2015] = "    <PlatformToolset>v140</PlatformToolset>"
platformtoolset[2016] = "    <PlatformToolset>v140</PlatformToolset>"
platformtoolset[2017] = "    <PlatformToolset>v140</PlatformToolset>"


#
#
# process_project_file ====================================
# process a VisualStudio Project File
def process_project_file(pfile):
    global vs
    global ifort
    global libdir
    global redistdir
    global toolsversion
    global platformtoolset
    # Type (F/C/C#) and related flags are set based on the file extension
    ptype = "unknown"
    config_tag = "unknown"
    config_val32 = "unknown"
    config_val64 = "unknown"
    if pfile.find("vfproj") != -1:
        ptype = "fortran"
        config_tag = "Configuration Name="
        config_val32 = "Win32"
        config_val64 = "x64"
    elif pfile.find("vcxproj") != -1:
        ptype = "c"
        config_tag = "ItemDefinitionGroup Condition="
        config_val32 = "Win32"
        config_val64 = "x64"
    elif pfile.find("vcproj") != -1:
        ptype = "csharp"
        config_tag = "PropertyGroup Condition="
        config_val32 = "x86"
        config_val64 = "x64"
    #
    # Put the full file contents in filin_contents
    with open(pfile, "r", encoding='utf-8') as filinhandle:
        filin_contents = filinhandle.readlines()
    #
    # Scan the contents and rewrite the full file
    configuration = 0
    with open(pfile, "w", encoding='utf-8') as filouthandle:
        for line in filin_contents:
            #
            # ToolsVersion
            # Skip this change when vs=0
            if vs != 0:
                startpos = line.find("ToolsVersion=")
                if startpos != -1:
                    parts = line.split('"')
                    i = 0
                    for part in parts:
                        if part.find("ToolsVersion=") != -1:
                            parts[i+1] = toolsversion[vs]
                        i += 1
                    line = '"'.join(parts)
            #
            # PlatformToolSet:
            # Skip this change when vs=0
            # Search for line with <CharacterSet>
            if vs != 0:
                startpos = line.find("<CharacterSet>")
                if startpos != -1:
                    #
                    # Write line and add put the PlatformToolSet stuff in line,
                    # such that it will be added after the CharacterSet line
                    if platformtoolset[vs] != "":
                        filouthandle.write(line)
                        line = platformtoolset[vs] + "\n"
                elif line.find("PlatformToolset") != -1:
                    #
                    # Remove the original PlatformToolset line (if present)
                    continue
            #
            # config_tag, to set configuration
            startpos = line.find(config_tag)
            if startpos != -1:
                if line.find(config_val32) != -1:
                    configuration = 32
                elif line.find(config_val64) != -1:
                    configuration = 64
            #
            # IFORT_COMPILER ...
            startpos = line.find("$(IFORT_COMPILER")
            if startpos != -1:
                split_char = ";"
                if line.find("oss-install") != -1:
                    #
                    # ... in argument of oss-install
                    if ptype == "c":
                        split_char = '"'
                    parts = line.split(split_char)
                    added = False
                    i = 0
                    for part in parts:
                        if part.find("$(IFORT_COMPILER") != -1:
                            if not added:
                                key = ptype + str(ifort) + str(configuration)
                                parts[i] = redistdir[key]
                                added = True
                                i += 1
                            # else:
                            # remove this part
                        else:
                            parts[i] = part
                            i += 1
                    del parts[i:]
                    line = split_char.join(parts)
                if line.find("AdditionalLibraryDirectories") != -1:
                    #
                    # ... in specification of AdditionalLibrarieDirectories
                    parts = line.split(split_char)
                    added = False
                    i = 0
                    for part in parts:
                        startpos = part.find("$(IFORT_COMPILER")
                        if startpos != -1:
                            if not added:
                                key = ptype + str(ifort) + str(configuration)
                                parts[i] = parts[i][:startpos] + libdir[key]
                                added = True
                                i += 1
                            # else:
                            # remove this part
                        else:
                            parts[i] = part
                            i += 1
                    del parts[i:]
                    line = split_char.join(parts)
            filouthandle.write(line)


#
#
# exit_button_pressed ====================================
# quit the window and this script
def exit_button_pressed():
    global root
    root.quit()


#
#
# do_work ====================================
# Process the selected vs and intel version
def do_work():
    global vs
    global ifort
    global libdir
    global redistdir
    global toolsversion
    global platformtoolset

    if vs == -999 or ifort == -999:
        vs = vs_gui.get()
        ifort = ifort_gui.get()
    sys.stdout.write("Visual Studio Version : " + str(vs) + "\n")
    sys.stdout.write("Intel Fortran Version : " + str(ifort) + "\n")

    # Copy the solution template file to the solution file
    sln = "delft3d_open.sln"
    sys.stdout.write("Creating file " + sln + " ...\n")
    scriptdir = os.path.dirname(os.path.abspath(__file__))
    topdir = scriptdir
    sln = os.path.join(topdir, sln)
    slntemplate = os.path.join(topdir, "scripts_lgpl", "win64", "delft3d_open_template.sln")
    shutil.copyfile(slntemplate, sln)

    # Collect the project files referenced in the solution file
    projectfiles = []
    # Read sln file:
    # Put the full file contents in filin_contents
    with open(sln, "r", encoding='utf-8') as filinhandle:
        filin_contents = filinhandle.readlines()

    # Scan the contents and rewrite the full solution file
    with open(sln, "w", encoding='utf-8') as filouthandle:
        for line in filin_contents:
            # Search for project file references
            pp = line.split('"')
            for subline in pp:
                if max(subline.find(".vfproj"), subline.find(".vcxproj"), subline.find(".vcproj")) != -1:
                    projectfiles.append(subline)
            # Changes to the sln file based on VS version
            startpos = line.find("Microsoft Visual Studio Solution File, Format Version")
            if startpos == 0:
                if vs == 2010:
                    line = "Microsoft Visual Studio Solution File, Format Version 11.00\n"
                elif vs == 2012:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
                elif vs == 2013:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
                elif vs == 2014:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
                elif vs == 2015:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
                elif vs == 2016:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
                elif vs == 2017:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
                # else:
                    # leave line unchanged
            startpos = line.find("# Visual Studio")
            if startpos == 0:
                if vs == 2010:
                    line = "# Visual Studio 2010\n"
                elif vs == 2012:
                    line = "# Visual Studio 2012\n"
                elif vs == 2013:
                    line = "# Visual Studio 2013\n"
                elif vs == 2014:
                    line = "# Visual Studio 2014\n"
                elif vs == 2015:
                    line = "# Visual Studio 2015\n"
                elif vs == 2016:
                    line = "# Visual Studio 2016\n"
                elif vs == 2017:
                    line = "# Visual Studio 2017\n"
                # else:
                    # leave line unchanged
            filouthandle.write(line)

    # Process all project files referenced in the sln file
    for pfile in projectfiles:
        pfile = os.path.join(topdir, pfile)
        sys.stdout.write("Processing file " + pfile + " ...\n")
        if os.path.isfile(pfile):
            process_project_file(pfile)
        else:
            sys.stdout.write("ERROR: File does not exists:" + pfile + "\n")
    sys.stdout.write("...Finished.\n")
    sys.stdout.write('Ready to be used: "' + sln + '"\n')
    
    # Force reading GUI parameters next run
    vs = -999
    ifort = -999

    #   root.quit()


#
#
# build_gui ==================================
# Create GUI
def build_gui():
    global vs_gui
    global ifort_gui
    global root

    root = Tk(className="Choose IDE and compiler")
    root.geometry("600x300")
    
    vs_gui = IntVar()
    ifort_gui = IntVar()
    
    Label(text="Visual Studio Version:", relief=RIDGE, width=20).grid(row=0, column=0)
    
    Radiobutton(root, text="VS 2015 (not tested yet)", variable=vs_gui, value=2015).grid(row=1, column=0, sticky=W)
    Radiobutton(root, text="VS 2013 + .Net Framework 4.5", variable=vs_gui, value=2013).grid(row=2, column=0, sticky=W)
    Radiobutton(root, text="VS 2012 + .Net Framework 4.5", variable=vs_gui, value=2012).grid(row=3, column=0, sticky=W)
    Radiobutton(root, text="VS 2010 + .Net Framework 4.0", variable=vs_gui, value=2010).grid(row=4, column=0, sticky=W)
    # default value
    vs_gui.set(2012)
    
    Label(text="IFORT Version:", relief=RIDGE, width=20).grid(row=0, column=2)
    Radiobutton(root, text="IFORT16: (not tested yet)                      ", variable=ifort_gui, value=16).grid(row=1, column=2, sticky=W)
    Radiobutton(root, text="IFORT15: Intel Parallel Studio XE 2015 Composer", variable=ifort_gui, value=15).grid(row=2, column=2, sticky=W)
    Radiobutton(root, text="IFORT14: Intel Visual Fortran Composer XE 2014 ", variable=ifort_gui, value=14).grid(row=3, column=2, sticky=W)
    Radiobutton(root, text="IFORT13: Intel Visual Fortran Composer XE 2013 ", variable=ifort_gui, value=13).grid(row=4, column=2, sticky=W)
    Radiobutton(root, text="IFORT12: Intel Visual Fortran Composer XE 2011 ", variable=ifort_gui, value=12).grid(row=5, column=2, sticky=W)
    # default value
    ifort_gui.set(13)
    
    Label(text=" ").grid(row=6)
    Label(text="Choose your Visual Studio version and IFORT version and click 'Apply'").grid(row=7, column=0, columnspan=3)
    
    b1 = Button(root, text="Apply", width=20, command=do_work).grid(row=8, column=0, sticky=W)
    b2 = Button(root, text="Exit", width=20, command=exit_button_pressed).grid(row=8, column=2, sticky=E)
    
    # To keep GUI window running
    root.mainloop()


#
#
# MAIN ====================================
if __name__ == "__main__":
    # Check command line arguments
    parser = argparse.ArgumentParser(description='Create sln file and change project files')
    parser.add_argument('-vs', '--visualstudio',
                              help='Specify VisualStudio version')
    parser.add_argument('-ifort', '--ifort',
                              help='Specify Intel Visual Fortran version')
    args = parser.parse_args()
    if args.visualstudio:
        vs = int(args.visualstudio)
    if args.ifort:
        ifort = int(args.ifort)
    
    # Both vs and ifort defined via command line arguments: do_work
    # Else: Create GUI to select them
    if vs == -999 or ifort == -999:
        build_gui()
    else:
        do_work()
