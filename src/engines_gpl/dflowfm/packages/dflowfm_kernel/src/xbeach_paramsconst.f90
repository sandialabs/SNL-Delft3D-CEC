module m_xbeach_paramsconst
  integer, parameter :: TURB_NONE                   =  0
  integer, parameter :: TURB_BORE_AVERAGED          =  1
  integer, parameter :: TURB_WAVE_AVERAGED          =  2

  integer, parameter :: WAVEFORM_RUESSINK_VANRIJN   =  0
  integer, parameter :: WAVEFORM_VANTHIEL           =  1

  integer, parameter :: TURBADV_NONE                =  0
  integer, parameter :: TURBADV_LAGRANGIAN          =  1
  integer, parameter :: TURBADV_EULERIAN            =  2
  
  integer, parameter :: INSTAT_STAT                 =  0
  integer, parameter :: INSTAT_BICHROM              =  1
  integer, parameter :: INSTAT_TS_1                 =  2
  integer, parameter :: INSTAT_TS_2                 =  3
  integer, parameter :: INSTAT_JONS                 =  4
  integer, parameter :: INSTAT_SWAN                 =  5
  integer, parameter :: INSTAT_VARDENS              =  6
  integer, parameter :: INSTAT_REUSE                =  7
  integer, parameter :: INSTAT_TS_NONH              =  8
  integer, parameter :: INSTAT_OFF                  =  9
  integer, parameter :: INSTAT_STAT_TABLE           =  10
  integer, parameter :: INSTAT_JONS_TABLE           =  11
 
end module m_xbeach_paramsconst
