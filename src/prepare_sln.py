import argparse
import os
import glob
import sys
import shutil
import subprocess

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
# This script can be used to create/convert the VisualStudio solution and project files
# for the version you are using.
# Adapt and use it for your own purpose.
#
# adri.mourits@deltares.nl
# 15 Jun 2017
#
# Usage:
# 1. Install Python on your machine https://www.python.org/downloads/
#    Version 2.7.* is most commonly used
# 2. Execute "prepare_sln.cmd" (double-clicking it in your file browser will do). A window will pop-up in which you can select the
#    VisualStudio version you are using and the Intel Fortran compiler version.
#    Optional usage without pop-up window:
#    python prepare_sln.py -vs 2013 -ifort 15
# 3. Click the "Apply" button.
#    Solution file(s) will be created,
#    All project files it refers to will be updated.
# 4. Click the "Exit" button.
# 5. Open the sln file in VisualStudio and "Build Solution"
# 6. If this script does not behave as expected: Please report the problem and help to improve this script.
#

chooseIfort = 1

vs = -999
fw = -999
ifort = -999
templateSolutionPath = ""

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
libdir["fortran1832"] = "$(IFORT_COMPILER18)\\compiler\\lib\\ia32"
libdir["c1832"] = libdir["fortran1832"]
libdir["fortran1864"] = "$(IFORT_COMPILER18)\\compiler\\lib\\intel64"
libdir["c1864"] = libdir["fortran1864"]
libdir["fortran1932"] = "$(IFORT_COMPILER19)\\compiler\\lib\\ia32"
libdir["c1932"] = libdir["fortran1932"]
libdir["fortran1964"] = "$(IFORT_COMPILER19)\\compiler\\lib\\intel64"
libdir["c1964"] = libdir["fortran1964"]

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
redistdir["fortran1832"] = "$(IFORT_COMPILER18)redist\\ia32\\compiler\\&quot"
redistdir["c1832"] = "$(IFORT_COMPILER18)redist\\ia32\\compiler\\"
redistdir["fortran1864"] = "$(IFORT_COMPILER18)redist\\intel64\\compiler\\&quot"
redistdir["c1864"] = "$(IFORT_COMPILER18)redist\\intel64\\compiler\\"
redistdir["fortran1932"] = "$(IFORT_COMPILER19)redist\\ia32\\compiler\\&quot"
redistdir["c1932"] = "$(IFORT_COMPILER19)redist\\ia32\\compiler\\"
redistdir["fortran1964"] = "$(IFORT_COMPILER19)redist\\intel64\\compiler\\&quot"
redistdir["c1964"] = "$(IFORT_COMPILER19)redist\\intel64\\compiler\\"


# redistdir specifies the directory containing the ifort redistributable dlls
# The string to be added can be set depending on:
# - fortran/c project file to place it in
# - ifort version
# - 32/64 bit settings
mkldir = {}
mkldir["fortran1132"] = "$(IFORT_COMPILER11)redist\\ia32\\mkl\\&quot"
mkldir["c1132"] = "$(IFORT_COMPILER11)redist\\ia32\\mkl\\"
mkldir["fortran1164"] = "$(IFORT_COMPILER11)redist\\intel64\\mkl\\&quot"
mkldir["c1164"] = "$(IFORT_COMPILER11)redist\\intel64\\mkl\\"
mkldir["fortran1232"] = "$(IFORT_COMPILER12)redist\\ia32\\mkl\\&quot"
mkldir["c1232"] = "$(IFORT_COMPILER12)redist\\ia32\\mkl\\"
mkldir["fortran1264"] = "$(IFORT_COMPILER12)redist\\intel64\\mkl\\&quot"
mkldir["c1264"] = "$(IFORT_COMPILER12)redist\\intel64\\mkl\\"
mkldir["fortran1332"] = "$(IFORT_COMPILER13)redist\\ia32\\mkl\\&quot"
mkldir["c1332"] = "$(IFORT_COMPILER13)redist\\ia32\\mkl\\"
mkldir["fortran1364"] = "$(IFORT_COMPILER13)redist\\intel64\\mkl\\&quot"
mkldir["c1364"] = "$(IFORT_COMPILER13)redist\\intel64\\mkl\\"
mkldir["fortran1432"] = "$(IFORT_COMPILER14)redist\\ia32\\mkl\\&quot"
mkldir["c1432"] = "$(IFORT_COMPILER14)redist\\ia32\\mkl\\"
mkldir["fortran1464"] = "$(IFORT_COMPILER14)redist\\intel64\\mkl\\&quot"
mkldir["c1464"] = "$(IFORT_COMPILER14)redist\\intel64\\mkl\\"
mkldir["fortran1532"] = "$(IFORT_COMPILER15)redist\\ia32\\mkl\\&quot"
mkldir["c1532"] = "$(IFORT_COMPILER15)redist\\ia32\\mkl\\"
mkldir["fortran1564"] = "$(IFORT_COMPILER15)redist\\intel64\\mkl\\&quot"
mkldir["c1564"] = "$(IFORT_COMPILER15)redist\\intel64\\mkl\\"
mkldir["fortran1632"] = "$(IFORT_COMPILER16)redist\\ia32\\mkl\\&quot"
mkldir["c1632"] = "$(IFORT_COMPILER16)redist\\ia32\\mkl\\"
mkldir["fortran1664"] = "$(IFORT_COMPILER16)redist\\intel64\\mkl\\&quot"
mkldir["c1664"] = "$(IFORT_COMPILER16)redist\\intel64\\mkl\\"
mkldir["fortran1732"] = "$(IFORT_COMPILER17)redist\\ia32\\mkl\\&quot"
mkldir["c1732"] = "$(IFORT_COMPILER17)redist\\ia32\\mkl\\"
mkldir["fortran1764"] = "$(IFORT_COMPILER17)redist\\intel64\\mkl\\&quot"
mkldir["c1764"] = "$(IFORT_COMPILER17)redist\\intel64\\mkl\\"
mkldir["fortran1832"] = "$(IFORT_COMPILER18)redist\\ia32\\mkl\\&quot"
mkldir["c1832"] = "$(IFORT_COMPILER18)redist\\ia32\\mkl\\"
mkldir["fortran1864"] = "$(IFORT_COMPILER18)redist\\intel64\\mkl\\&quot"
mkldir["c1864"] = "$(IFORT_COMPILER18)redist\\intel64\\mkl\\"
mkldir["fortran1932"] = "$(IFORT_COMPILER19)redist\\ia32\\mkl\\&quot"
mkldir["c1932"] = "$(IFORT_COMPILER19)redist\\ia32\\mkl\\"
mkldir["fortran1964"] = "$(IFORT_COMPILER19)redist\\intel64\\mkl\\&quot"
mkldir["c1964"] = "$(IFORT_COMPILER19)redist\\intel64\\mkl\\"

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
toolsversion[2019] = "14.0"

#
#
# frameworkversion specifies the .Net frameworknumber
frameworkversion = {}
frameworkversion[42] = "4.2"
frameworkversion[43] = "4.3"
frameworkversion[44] = "4.4"
frameworkversion[45] = "4.5"
frameworkversion[46] = "4.6"
frameworkversion[47] = "4.7"
frameworkversion[48] = "4.8"

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
platformtoolset[2017] = "    <PlatformToolset>v141</PlatformToolset>"
platformtoolset[2019] = "    <PlatformToolset>v142</PlatformToolset>"

#
#
# Since VisualStudio2015 (Update 3?), it is sometimes necessary to add the location of ucrt.lib
# to the AdditionalLibaryDirectories of the projects linking with Fortran
# The string "$(OSS_UCRTLIBDIR)" is added at these locations by default (it doesn't harm when
# it is not defined), and will be replaced by this prepare_sln.py script to get the proper value vor VS2015 and up.
# "UCRTLIBDIRVERSIONNUMBER" is a place holder that will be replaced in getUCRTVersionNumber()
# $(UniversalCRTSdkDir) is always used in this string (and not replaced by something like c:\Program Files (x86)\Windows Kits\10\Lib\)
ucrtlibdir = {}
ucrtlibdir["-1"] = "$(OSS_UCRTLIBDIR)"
ucrtlibdir["201532"] = "$(UniversalCRTSdkDir)Lib\\UCRTLIBDIRVERSIONNUMBER\\ucrt\\x86"
ucrtlibdir["201564"] = "$(UniversalCRTSdkDir)Lib\\UCRTLIBDIRVERSIONNUMBER\\ucrt\\x64"
ucrtlibdir["201732"] = "$(UniversalCRTSdkDir)Lib\\UCRTLIBDIRVERSIONNUMBER\\ucrt\\x86"
ucrtlibdir["201764"] = "$(UniversalCRTSdkDir)Lib\\UCRTLIBDIRVERSIONNUMBER\\ucrt\\x64"
ucrtlibdir["201932"] = "$(UniversalCRTSdkDir)Lib\\UCRTLIBDIRVERSIONNUMBER\\ucrt\\x86"
ucrtlibdir["201964"] = "$(UniversalCRTSdkDir)Lib\\UCRTLIBDIRVERSIONNUMBER\\ucrt\\x64"

#
#
# To obtain the ucrt directory:
# Execute the matching vcvarsall.bat and in that shell, get the value of environment parameter UniversalCRTSdkDir
# This is combined in the folowing string:
getucrtdir = {}
getucrtdir["2015"] = '"' + str(os.environ.get("VS140COMNTOOLS")) + "..\\..\\VC\\vcvarsall.bat" + '" amd64&&set UniversalCRTSdkDir'
getucrtdir["2017"] = '"' + str(os.environ.get("VS2017INSTALLDIR")) + "\\VC\\Auxiliary\\Build\\vcvarsall.bat" + '" amd64&&set UniversalCRTSdkDir'
getucrtdir["2019"] = '"' + str(os.environ.get("VS2019INSTALLDIR")) + "\\VC\\Auxiliary\\Build\\vcvarsall.bat" + '" amd64&&set UniversalCRTSdkDir'

#
#
# process_solution_file ====================================
# process a VisualStudio Solution File (and underlying projects)
# Pass only file names, no full path names. It assumed that both
# files are in fixed locations (see below).
def process_solution_file(sln, slntemplate):
    global vs
    global fw
    global ifort
    global templateSolutionPath
    global libdir
    global redistdir
    global toolsversion
    global frameworkversion
    global platformtoolset

    # Copy the solution template file to the solution file
    sys.stdout.write("Creating file " + sln + " ...\n")
    scriptdir = os.path.dirname(os.path.abspath(__file__))
    topdir = scriptdir

    # target file:
    sln = os.path.join(topdir, sln)

    # source template file:
    slntemplate = os.path.join(topdir, slntemplate)

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
                if max(subline.find(".vfproj"), subline.find(".vcxproj"), subline.find(".vcproj"), subline.find(".csproj")) != -1:
                    projectfiles.append(subline)
            # Changes to the sln file based on VS version
            startpos = line.find("Microsoft Visual Studio Solution File, Format Version")
            if startpos == 0:
                if vs == 2010:
                    line = "Microsoft Visual Studio Solution File, Format Version 11.00\r\n"
                elif vs == 2012:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\r\n"
                elif vs == 2013:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\r\n"
                elif vs == 2014:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\r\n"
                elif vs == 2015:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\r\n"
                elif vs == 2016:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\r\n"
                elif vs == 2017:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\r\n"
                elif vs == 2019:
                    line = "Microsoft Visual Studio Solution File, Format Version 12.00\r\n"
                # else:
                    # leave line unchanged
            startpos = line.find("# Visual Studio")
            if startpos == 0:
                if vs == 2010:
                    line = "# Visual Studio 2010\r\n"
                elif vs == 2012:
                    line = "# Visual Studio 2012\r\n"
                elif vs == 2013:
                    line = "# Visual Studio 2013\r\n"
                elif vs == 2014:
                    line = "# Visual Studio 2014\r\n"
                elif vs == 2015:
                    line = "# Visual Studio 2015\r\n"
                elif vs == 2016:
                    line = "# Visual Studio 2016\r\n"
                elif vs == 2017:
                    line = "# Visual Studio 2017\r\n"
                elif vs == 2019:
                    line = "# Visual Studio 2019\r\n"
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


#
#
# process_project_file ====================================
# process a VisualStudio Project File
def process_project_file(pfile):
    global vs
    global fw
    global ifort
    global libdir
    global redistdir
    global toolsversion
    global frameworkversion
    global platformtoolset
    global ucrtlibdir
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
    elif (pfile.find("vcproj") != -1 or pfile.find("csproj") != -1):
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
            # FrameworkVersion
            # Skip this change when fw=0
            if fw != 0:
                startpos = line.find("<TargetFrameworkVersion>")
                if startpos != -1:
                    line = line[:startpos+25] + frameworkversion[fw] + line[startpos+28:]
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
                        # Conserve line endings
                        # Assumption: the ">" is the last character on this line, before the line ending character(s)
                        lineEndingStart = str(line).rfind(">")
                        line = platformtoolset[vs] + line[lineEndingStart+1:]
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
                if ifort == -999:
                    sys.exit("ERROR: Fortran compiler specification is being used while not defined.")
                split_char = ";"
                if line.find("oss-install") != -1:
                    #
                    # ... in argument of oss-install
                    if ptype == "c":
                        split_char = '"'
                    parts = line.split(split_char)
                    i = 0
                    lastFound = -1
                    it = 0
                    for part in parts:
                        lastFound = part.find("$(IFORT_COMPILER", lastFound + 1)
                        if(lastFound!= -1):
                            tempStr=""
                            while (lastFound!=-1):
                                key = ptype + str(ifort) + str(configuration)
                                if it == 0:
                                    tempStr += redistdir[key]
                                    lastFound = part.find("$(IFORT_COMPILER", lastFound + 1)
                                elif it == 1:
                                    tempStr += mkldir[key]
                                    lastFound = part.find("$(IFORT_COMPILER", lastFound + 1)
                                elif it>1:
                                    break
                                it += 1
                            parts[i] = tempStr
                            i += 1
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
                else:
                    # Unclear context of using IFORT_COMPILER
                    # Just replace the version number at the end
                    startpos = startpos + 16
                    endpos   = startpos + 2
                    line = line[:startpos] + str(ifort) + line[endpos:]
            #
            # UCRTlibdir
            # Search string to be replaced: two options: "$(OSS_UCRTLIBDIR)" and "$(UniversalCRTSdkDir)lib\..."
            #
            # $(OSS_UCRTLIBDIR) => $(UniversalCRTSdkDir)lib\...
            startpos = line.find(ucrtlibdir["-1"])
            if startpos != -1:
                endpos = startpos + len(ucrtlibdir["-1"])
            else:
                # $(UniversalCRTSdkDir)lib\... => $(OSS_UCRTLIBDIR)
                startpos = line.find(ucrtlibdir["201532"][:21])
                if startpos != -1:
                    quotepos = line[startpos:].find('"')
                    if quotepos == -1:
                        quotepos = 999
                    colonpos = line[startpos:].find(";")
                    if colonpos == -1:
                        colonpos = 999
                    endpos = startpos + min(quotepos, colonpos)
            # Replace by the correct string. Assumption: "UCRTLIBDIRVERSIONNUMBER" is replaced by the correct
            # versionnumber when applicable, by executing getUCRTVersionNumber
            if startpos != -1:
                if vs >= 2015:
                    key = str(vs) + str(configuration)
                else:
                    key = "-1"
                line = line[:startpos] + ucrtlibdir[key] + line[endpos:]
            filouthandle.write(line)


#
#
# getUCRTVersionNumber ===================================
# Note: UniversalCRTSdkDir is resolved to find the version number on this system,
#       $(UniversalCRTSdkDir) is not replaced in ucrtlibdir
def getUCRTVersionNumber():
    global vs
    global ucrtlibdir
    # Only for VS2015 or higher
    if vs >= 2015:
        # To get the ucrt directory: execute the matching getucrtdir string, 
        # catch the stdout of that command,
        # check whether UniversalCRTSdkDir is in that string,
        # if so, get the value behind the '='-sign
        sys.stdout.write("Trying to execute: " + getucrtdir[str(vs)] + " ...\n")
        try:
            result = subprocess.check_output(getucrtdir[str(vs)], shell=True)
        except:
            result = ""
            sys.stdout.write("\n\n *** ERROR:Execution failed; is VisualStudio " + str(vs) + " installed?\n\n\n")
        result = result.decode('utf-8')
        if result.find("UniversalCRTSdkDir") == -1:
            # Fallback: it should be this:
            sys.stdout.write("ucrtdir not found; set to default value\n")
            ucrtdir = "c:\\Program Files (x86)\\Windows Kits\\10\\Lib\\"
        else:
            ucrtdir = result[19:]
            # result may be:
            # ****\nbladibla\n****\nUniversalCRTSdkDir=<value>
            # Get the value
            ucrtpos = ucrtdir.rfind("UniversalCRTSdkDir=")
            if ucrtpos > -1:
                ucrtdir = ucrtdir[ucrtpos+19:]
            # Remove the trailing slash and the newline-character behind it
            lastslash = ucrtdir.rfind("\\")
            if lastslash != -1:
                ucrtdir = ucrtdir[:lastslash]
            sys.stdout.write("ucrtdir found:" + ucrtdir + "\n")
        # Search in subdir Lib for directories starting with a digit and containing at least one "."
        searchstring = os.path.join(ucrtdir, "Lib", "[0-9]*.*")
        versions = glob.glob(searchstring)
        if len(versions) <= 0:
            # Fallback: it should be this:
            ucrtversion = "10.0.10586.0"
            sys.stdout.write("No versions found, using default version:" + ucrtversion + "\n")
        else:
            # Choose the highest version number
            versions.sort(reverse=True)
            ucrtversion = versions[0]
            ucrtversion = os.path.basename(ucrtversion)
            sys.stdout.write("Versions found, using:" + ucrtversion + "\n")
        # Inside ucrtlibdir, replace all occurences of UCRTLIBDIRVERSIONNUMBER by ucrtversion
        for key in iter(ucrtlibdir):
            ucrtlibdir[key] = str(ucrtlibdir[key]).replace("UCRTLIBDIRVERSIONNUMBER", ucrtversion)

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
    global fw
    global ifort
    global libdir
    global redistdir
    global toolsversion
    global platformtoolset

    if vs == -999 or ifort == -999:
        vs = vs_gui.get()
        ifort = ifort_gui.get()
        fw = fw_gui.get()
    if fw == -999:
        # fw does not have to be set. Use a proper guess
        if vs <= 2010:
            fw = 40
        elif vs <= 2014:
            fw = 45
        else:
            fw = 46
    sys.stdout.write("Visual Studio  Version : " + str(vs) + "\n")
    sys.stdout.write(".Net Framework Version : " + str(fw) + "\n")
    sys.stdout.write("Intel Fortran  Version : " + str(ifort) + "\n")
    sys.stdout.write("Solution path : " + templateSolutionPath + "\n")

    # Needed for VS2015 and higher:
    getUCRTVersionNumber()

    if not templateSolutionPath:
        process_solution_file("delft3d_open.sln", os.path.join("scripts_lgpl", "win64", "delft3d_open_template.sln"))
        process_solution_file("dflowfm_open.sln", os.path.join("engines_gpl", "dflowfm", "scripts", "template", "dflowfm_open_template.sln"))
        process_solution_file("dimr_open.sln", os.path.join("engines_gpl", "dimr", "scripts", "template", "dimr_open_template.sln"))

        # TODO: Consider making this optional via cmdline args:
        process_solution_file("io_netcdf.sln",    os.path.join("scripts_lgpl", "win64", "io_netcdf_template.sln"))
        process_solution_file("nefis.sln",        os.path.join("scripts_lgpl", "win64", "nefis_template.sln"))
        process_solution_file("utils_lgpl.sln",   os.path.join("scripts_lgpl", "win64", "utils_lgpl_template.sln"))
        process_solution_file("tests.sln",        os.path.join("scripts_lgpl", "win64", "tests_template.sln"))
    else:
        slnName = os.path.basename(templateSolutionPath).replace("_template","")
        process_solution_file(slnName, templateSolutionPath)

    process_solution_file("ec_module.sln",    os.path.join("scripts_lgpl", "win64", "ec_module_template.sln"))

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
    global fw_gui
    global ifort_gui
    global root
    global chooseIfort

    root = Tk(className="Choose IDE and compiler")
    root.geometry("750x350")
    
    vs_gui = IntVar()
    fw_gui = IntVar()
    ifort_gui = IntVar()
    
    Label(text="Visual Studio Version:", relief=RIDGE, width=20).grid(row=0, column=0)
    
    Radiobutton(root, text="VS 2019                           ", variable=vs_gui, value=2019).grid(row=1, column=0, sticky=W)
    Radiobutton(root, text="VS 2017                           ", variable=vs_gui, value=2017).grid(row=2, column=0, sticky=W)
    Radiobutton(root, text="VS 2015, Update 3                 ", variable=vs_gui, value=2015).grid(row=3, column=0, sticky=W)
    Radiobutton(root, text="VS 2013                           ", variable=vs_gui, value=2013).grid(row=4, column=0, sticky=W)
    Radiobutton(root, text="VS 2012                           ", variable=vs_gui, value=2012).grid(row=5, column=0, sticky=W)
    Radiobutton(root, text="VS 2010                           ", variable=vs_gui, value=2010).grid(row=6, column=0, sticky=W)
    # default value
    vs_gui.set(2015)
    
    Label(text=".Net Framwork Version:", relief=RIDGE, width=20).grid(row=0, column=1)
    
    Radiobutton(root, text=".Net Framework 4.8", variable=fw_gui, value=48).grid(row=1, column=1, sticky=W)
    Radiobutton(root, text=".Net Framework 4.7", variable=fw_gui, value=47).grid(row=2, column=1, sticky=W)
    Radiobutton(root, text=".Net Framework 4.6", variable=fw_gui, value=46).grid(row=3, column=1, sticky=W)
    Radiobutton(root, text=".Net Framework 4.5", variable=fw_gui, value=45).grid(row=4, column=1, sticky=W)
    Radiobutton(root, text=".Net Framework 4.4", variable=fw_gui, value=44).grid(row=5, column=1, sticky=W)
    Radiobutton(root, text=".Net Framework 4.3", variable=fw_gui, value=43).grid(row=6, column=1, sticky=W)
    Radiobutton(root, text=".Net Framework 4.2", variable=fw_gui, value=42).grid(row=7, column=1, sticky=W)
    # default value
    fw_gui.set(46)
    
    if chooseIfort == 1:
        Label(text="IFORT Version:", relief=RIDGE, width=20).grid(row=0, column=2)
        Radiobutton(root, text="IFORT19: Intel Parallel Studio XE 2019         ", variable=ifort_gui, value=19).grid(row=1, column=2, sticky=W)
        Radiobutton(root, text="IFORT18: Intel Parallel Studio XE 2018 Update 4", variable=ifort_gui, value=18).grid(row=2, column=2, sticky=W)
        Radiobutton(root, text="IFORT17: (Not Recommended)                     ", variable=ifort_gui, value=17).grid(row=3, column=2, sticky=W)
        Radiobutton(root, text="IFORT16: Intel Parallel Studio XE 2016 Update 4", variable=ifort_gui, value=16).grid(row=4, column=2, sticky=W)
        Radiobutton(root, text="IFORT15: Intel Parallel Studio XE 2015 Update 6", variable=ifort_gui, value=15).grid(row=5, column=2, sticky=W)
        Radiobutton(root, text="IFORT14: Intel Visual Fortran Composer XE 2014 ", variable=ifort_gui, value=14).grid(row=6, column=2, sticky=W)
        Radiobutton(root, text="IFORT13: Intel Visual Fortran Composer XE 2013 ", variable=ifort_gui, value=13).grid(row=7, column=2, sticky=W)
        Radiobutton(root, text="IFORT12: Intel Visual Fortran Composer XE 2011 ", variable=ifort_gui, value=12).grid(row=8, column=2, sticky=W)
        # default value
        ifort_gui.set(16)
    else:
        ifort_gui.set(-999)
    
    Label(text=" ").grid(row=8)
    if chooseIfort == 1:
        Label(text="Choose your Visual Studio version, .Net Framework version and IFORT version and click 'Apply'").grid(row=9, column=0, columnspan=3)
    else:
        Label(text="Choose your Visual Studio version and click 'Apply'").grid(row=9, column=0, columnspan=3)
    
    b1 = Button(root, text="Apply", width=20, command=do_work).grid(row=10, column=0, sticky=W)
    b2 = Button(root, text="Exit", width=20, command=exit_button_pressed).grid(row=10, column=2, sticky=E)
    
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
    parser.add_argument('-fw', '--framework',
                              help='Specify .Net Framework version')
    parser.add_argument('-ifort', '--ifort',
                              help='Specify Intel Visual Fortran version')
    parser.add_argument('-templatePath', '--templatePath',
                              help='Specify the template solution path to prepare (if not specified => all solutions will be made)')
    args = parser.parse_args()
    if args.visualstudio:
        vs = int(args.visualstudio)
    if args.framework:
        fw = int(args.framework)
    if args.ifort:
        ifort = int(args.ifort)
    if args.templatePath:
        templateSolutionPath = args.templatePath

    # Both vs and ifort defined via command line arguments: do_work
    # Else: Create GUI to select them
    if vs == -999 or ifort == -999:
        build_gui()
    else:
        do_work()
