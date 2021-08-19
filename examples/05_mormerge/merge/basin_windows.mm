flowexedir       = ..\..\..\src\bin\x64\dflow2d3d\bin
flowargs         = config_d_hydro.xml
waveexedir       = ..\..\..\src\bin\x64\dwaves\bin
waveargs         = bas.mdw 1
swanbatdir       = ..\..\..\src\bin\x64\swan\scripts
mormergeexedir   = ..\..\..\src\bin\x64\dflow2d3d\bin
nodes            = local
# nodes            = 1
debug            = 0
# workdir          = e:\temp
condition:weight =   0deg : 3.0
condition:weight =  45deg : 1.0
# condition:weight =  90deg : 3.0
# condition:weight = 135deg : 1.0
# condition:weight = 180deg : 3.0
# condition:weight = 270deg : 3.0
