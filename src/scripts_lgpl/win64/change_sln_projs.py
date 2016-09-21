import os, sys

#=== Visual Studio version ====================================================
vs = 2010
# Don't do vs changes: when opening the solution in a newer vs version, it will be updated automatically
vs = 0

#=== Intel Fortran compiler version ===========================================
ifort = 13

sln = "d_hydro_open_source_vs2010.sln"

scriptdir = os.path.dirname(os.path.abspath(__file__))
topdir = os.path.join(scriptdir, "..", "..")
sln = os.path.join(topdir,sln)

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
# IFORT_COMPILER
# vfproj: Fortran
#   Configuration Name=Win32/x64
#   AdditionalLibraryDirectories ;$(IFORT_COMPILER12)\compiler\lib\ia32;  /  ;$(IFORT_COMPILER12)\compiler\lib\intel64;
#   oss-install         &quot;$(IFORT_COMPILER12)redist\ia32\compiler\&quot;   /   &quot;$(IFORT_COMPILER12)redist\intel64\compiler\&quot;
#
# vcxproj: C/C++
#   ItemDefinitionGroup Condition= Win32/x64
#   AdditionalLibraryDirectories ;$(IFORT_COMPILER12)\compiler\lib\ia32;   /   ;$(IFORT_COMPILER12)\compiler\lib\intel64;
#   oss-install        "$(IFORT_COMPILER12)redist\ia32\compiler\"             /   "$(IFORT_COMPILER12)redist\intel64\compiler\"
#
# vcproj: C#
#   PropertyGroup Condition= x86/x64

# vs2010 : niets
#          <TargetFrameworkVersion>v4.0</TargetFrameworkVersion>
# vs2012 : <PlatformToolset>v110</PlatformToolset>
#          <TargetFrameworkVersion>v4.5</TargetFrameworkVersion>
#====================================
def processProjectFile(pfile):
    if pfile.find("vfproj") != -1:
        ptype = "fortran"
        configTag = "Configuration Name="
        configVal32 = "Win32"
        configVal64 = "x64"
    elif pfile.find("vcxproj") != -1:
        ptype = "c"
        configTag = "ItemDefinitionGroup Condition="
        configVal32 = "Win32"
        configVal64 = "x64"
    elif pfile.find("vcproj") != -1:
        ptype = "csharp"
        configTag = "PropertyGroup Condition="
        configVal32 = "x86"
        configVal64 = "x64"
    filinContents = []
    with open(pfile, "r") as filinhandle:
        filinContents = filinhandle.readlines()
    filinhandle.closed
    configuration = 0
    with open(pfile, "w") as filouthandle:
        for line in filinContents:
            startpos = str(line).find(configTag)
            if startpos != -1:
                if str(line).find(configVal32) != -1:
                    configuration = 32
                elif str(line).find(configVal64) != -1:
                    configuration = 64
            startpos = str(line).find("$(IFORT_COMPILER")
            if startpos != -1:
                splitChar = ";"
                if str(line).find("oss-install") != -1:
                    if ptype == "c":
                        splitChar = "\""
                    parts = str(line).split(splitChar)
                    added = False
                    i = 0
                    for part in parts:
                        if str(part).find("$(IFORT_COMPILER") != -1:
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
                    line = splitChar.join(parts)
                if str(line).find("AdditionalLibraryDirectories") != -1:
                    parts = str(line).split(splitChar)
                    added = False
                    i = 0
                    for part in parts:
                        startpos = str(part).find("$(IFORT_COMPILER")
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
                    line = splitChar.join(parts)
            filouthandle.write(line)
    filouthandle.closed


#
#
# MAIN ====================================
ProjectFiles = []
# Read sln file
with open(sln, "r") as filinhandle:
   filinContents = filinhandle.readlines()
filinhandle.closed
with open(sln, "w") as filouthandle:
    for line in filinContents:
        # Search for project file references
        pp = line.split("\"")
        for subline in pp:
            if max(str(subline).find(".vfproj"),str(subline).find(".vcxproj"),str(subline).find(".vcproj")) != -1:
                ProjectFiles.append(subline)
        startpos = str(line).find("Microsoft Visual Studio Solution File, Format Version")
        if startpos == 0:
            if vs==2010:
                line = "Microsoft Visual Studio Solution File, Format Version 11.00\n"
            elif vs==2012:
                line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
            elif vs==2013:
                line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
            elif vs==2014:
                line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
            elif vs==2015:
                line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
            elif vs==2016:
                line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
            elif vs==2017:
                line = "Microsoft Visual Studio Solution File, Format Version 12.00\n"
            # else:
                # leave line unchanged
        startpos = str(line).find("# Visual Studio")
        if startpos == 0:
            if vs==2010:
                line = "# Visual Studio 2010\n"
            elif vs==2012:
                line = "# Visual Studio 2012\n"
            elif vs==2013:
                line = "# Visual Studio 2013\n"
            elif vs==2014:
                line = "# Visual Studio 2014\n"
            elif vs==2015:
                line = "# Visual Studio 2015\n"
            elif vs==2016:
                line = "# Visual Studio 2016\n"
            elif vs==2017:
                line = "# Visual Studio 2017\n"
            # else:
                # leave line unchanged
        filouthandle.write(line)
filouthandle.closed

sys.stdout.write("Projects:\n")
for pfile in ProjectFiles:
    pfile = os.path.join(topdir, pfile)
    sys.stdout.write(pfile + "\n")
    if os.path.isfile(pfile):
        processProjectFile(pfile)
    else:
        sys.stdout.write("ERROR: File does not exists:" + pfile + "\n")
