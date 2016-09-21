source create_config_xml.tcl

# mdf, no wait
set res [createConfigXML -mdf "f34.mdf" -out "config_d_hydro_1.xml"]
if {$res != ""} {
    puts $res
}

# ddb, with wait and sp
set runid "vlissingen  version2.2.3"
set version "3.4.5.6"
set res [createConfigXML -ddb "$runid.ddb" -sp -wait "true" -out "config -d_hydro2.3 new version $version.xml"]
if {$res != ""} {
    puts $res
}
set res [createConfigXML -mdf "$runid.mdf" -wait "false" -out "config -d_hydro2.3 newtoo version $version.xml" -dol 0]
if {$res != ""} {
    puts $res
}

puts "No error messages expected above.\n"
# The following calls should generate a proper error message and no output file

puts "Expected error message: no mdf or ddb:"
set res [createConfigXML -wait "true" -out "config_d_hydro_3.xml"]
if {$res != ""} {
    puts $res
}

puts "Expected error message: both mdf and ddb:"
set res [createConfigXML -mdf "f34.mdf" -ddb "vlissingen.ddb" -out "config_d_hydro_4.xml"]
if {$res != ""} {
    puts $res
}

puts "Expected error message: no out:"
set res [createConfigXML -mdf "f34.mdf" -wait "true"]
if {$res != ""} {
    puts $res
}

puts "Expected error message: wrong wait value:"
set res [createConfigXML -mdf "f34.mdf" -wait "1minute" -out "config_d_hydro_6.xml"]
if {$res != ""} {
    puts $res
}

puts "Expected error message: wrong argument:"
set res [createConfigXML -mdb "f34.mdf" -out "config_d_hydro_7.xml" -dol "yes"]
if {$res != ""} {
    puts $res
}

puts "Expected error message: mdf name starts with a \"-\":"
set res [createConfigXML -mdf -out "config_d_hydro_7.xml"]
if {$res != ""} {
    puts $res
}

