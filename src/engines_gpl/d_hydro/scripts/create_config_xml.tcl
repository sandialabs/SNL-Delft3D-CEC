#
# Procedure   : createConfigXML
# Arguments   : -mdf <filename>                 Name of mdf file
#               -ddb <filename>                 Name of ddb file: exactly one of -mdf or -ddb must be specified
#               -out <filename>                 Name of xml file to be generated
#               -sp                             Use single precision Delft3D-FLOW executable
#               -wait "true"/"false" [optional] DelftOnline wait value
# Return value: ""              : No error
#               Any other string: Error message
# Result      : Produces an XML-file (name specified by -out) that can be read by d_hydro.exe (Delft3D-FLOW version 6.xx)
#
# Author      : adri.mourits@deltares.nl
#

proc createConfigXML { args } {
    set version "Deltares, create_config_xml.tcl, Version 1.01"
    set fileversion "1.00"

    set result ""

    set ddb  ""
    set mdf  ""
    set out  ""
    set sp 0
    set wait "false"
    set dol 1

    # Read arguments
    set argcount  [llength $args]
    set argnumber 0 
    while { $argnumber < $argcount } {
        set arg [lindex $args $argnumber ]
        switch -- $arg {
            "-d"      -
            "-ddb" {
                incr argnumber
                set ddb [lindex $args $argnumber]
            }
            "-dol" {
                incr argnumber
                set dol [lindex $args $argnumber]
            }
            "-m"      -
            "-mdf" {
                incr argnumber
                set mdf [lindex $args $argnumber]
            }
            "-o"      -
            "-out" {
                incr argnumber
                set out [lindex $args $argnumber]
            }
            "-sp" {
                set sp 1
            }
            "-w"      -
            "-wait" {
                incr argnumber
                set wait [lindex $args $argnumber]
            }
            default {
                set result "$result ERROR: in createConfigXML: unknown argument \"$arg\".\n"
            }
        }
        incr argnumber
    }

    # Check arguments
    if {[string index $ddb 0] == "-"} {
        set result "$result ERROR: in createConfigXML: the value of argument \"-ddb\" starts with the character \"-\".\n"
    }
    if {[string index $mdf 0] == "-"} {
        set result "$result ERROR: in createConfigXML: the value of argument \"-mdf\" starts with the character \"-\".\n"
    }
    if {[string index $out 0] == "-"} {
        set result "$result ERROR: in createConfigXML: the value of argument \"-out\" starts with the character \"-\".\n"
    }
    if {[string index $wait 0] == "-"} {
        set result "$result ERROR: in createConfigXML: the value of argument \"-wait\" starts with the character \"-\".\n"
    }
    if {$mdf=="" && $ddb==""} {
        set result "$result ERROR: in createConfigXML: argument \"-mdf\" or \"-ddb\" must be present.\n"
    }
    if {$mdf!="" && $ddb!=""} {
        set result "$result ERROR: in createConfigXML: argument \"-mdf\" and \"-ddb\" are both present.\n"
    }
    if {$out==""} {
        set result "$result ERROR: in createConfigXML: argument \"-out\" must be present.\n"
    }
    if {$wait!="true" && $wait!="false"} {
        set result "$result ERROR: in createConfigXML: argument \"-wait\" must be \"true\" or \"false\".\n"
    }
    if {$dol!=0 && $dol!=1} {
        set result "$result ERROR: in createConfigXML: argument \"-dol\" must be \"0\" or \"1\".\n"
    }
    if {$result != ""} {
        return $result
    }
    

    if {$mdf!=""} {
        set urlfile "[file rootname $mdf].url"
    } else {
        set urlfile "[file rootname $ddb].url"
    }
    
    # Write XML file
    set outfile [open $out "w"]
    puts $outfile "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>"
    puts $outfile "<deltaresHydro xmlns=\"http://schemas.deltares.nl/deltaresHydro\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://schemas.deltares.nl/deltaresHydro http://content.oss.deltares.nl/schemas/d_hydro-1.00.xsd\">"
    puts $outfile "    <documentation>"
    puts $outfile "        File created by    : $version"
    puts $outfile "        File creation date : [clock format [clock seconds] -format "%d %B %Y, %H:%M:%S"]"
    puts $outfile "        File version       : $fileversion"
    puts $outfile "    </documentation>"
    puts $outfile "    <control>"
    puts $outfile "        <sequence>"
    puts $outfile "            <start>myNameFlow</start>"
    puts $outfile "        </sequence>"
    puts $outfile "    </control>"
    puts $outfile "    <flow2D3D name=\"myNameFlow\">"
    if {$sp} {
        puts $outfile "        <library>flow2d3d_sp</library>"
    } else {
        puts $outfile "        <library>flow2d3d</library>"
    }
    if {$mdf!=""} {
        puts $outfile "        <mdfFile>$mdf</mdfFile>"
    } else {
        puts $outfile "        <ddbFile>$ddb</ddbFile>"
    }
    puts $outfile "        <!--"
    puts $outfile "            Note: exactly one mdfFile (single domain) or ddbFile (domain decomposition)"
    puts $outfile "            element must be present."
    puts $outfile "        -->"
    puts $outfile "        <!--"
    puts $outfile "            Options/alternatives:"
    puts $outfile "            1) DomainDecomposition: replace <mdfFile>f34.mdf</mdfFile> with:"
    puts $outfile "                <ddbFile>vlissingen.ddb</ddbFile>"
    puts $outfile "            2) Specification of dll/so to use:"
    puts $outfile "                <library>/opt/delft3d/lnx64/flow2d3d/bin/libflow2d3d.so</library>"
    puts $outfile "            3) Single precision:"
    puts $outfile "                <library>flow2d3d_sp</library>"
    puts $outfile "            4) Documentation:"
    puts $outfile "                <documentation>"
    puts $outfile "                    Basic tutorial testcase."
    puts $outfile "                </documentation>"
    puts $outfile "            5) More output to screen (silent, error, info, trace. default: error):"
    puts $outfile "                <verbosity>trace</verbosity>"
    puts $outfile "            6) Debugging by attaching to running processes (parallel run):"
    puts $outfile "                <waitFile>debug.txt</waitFile>"
    puts $outfile "            7) Force stack trace to be written (Linux only):"
    puts $outfile "                <crashOnAbort>true</crashOnAbort>"
    puts $outfile "        -->"
    puts $outfile "    </flow2D3D>"
    if {$dol} {
        puts $outfile "    <delftOnline>"
        puts $outfile "        <enabled>true</enabled>"
        puts $outfile "        <urlFile>$urlfile</urlFile>"
        puts $outfile "        <waitOnStart>$wait</waitOnStart>"
        puts $outfile "        <clientControl>true</clientControl>    <!-- client allowed to start, step, stop, terminate -->"
        puts $outfile "        <clientWrite>false</clientWrite>    <!-- client allowed to modify data -->"
        puts $outfile "        <!--"
        puts $outfile "            Options/alternatives:"
        puts $outfile "            1) Change port range:"
        puts $outfile "                <tcpPortRange start=\"51001\" end=\"51099\"/>"
        puts $outfile "            2) More output to screen (silent, error, info, trace. default: error):"
        puts $outfile "                <verbosity>trace</verbosity>"
        puts $outfile "            3) Force stack trace to be written (Linux only):"
        puts $outfile "                <crashOnAbort>true</crashOnAbort>"
        puts $outfile "        -->"
        puts $outfile "    </delftOnline>"
    }
    puts $outfile "</deltaresHydro>"
    close $outfile
}
