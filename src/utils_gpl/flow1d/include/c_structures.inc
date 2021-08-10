  type, public, bind(C) :: weir
     character(kind=c_char) :: id(MAXSTRINGLEN)
     real(c_double) :: x
     real(c_double) :: y
     integer(c_int) :: branchid
     real(c_double) :: chainage
     integer(c_int) :: link_number
     real(c_double) :: crest_level
     real(c_double) :: crest_width
     real(c_double) :: discharge_coeff
     real(c_double) :: lat_dis_coeff
     integer(c_int) :: allowed_flow_dir
  end type weir
  type, public, bind(C) :: orifice
     character(kind=c_char) :: id(MAXSTRINGLEN)
     real(c_double) :: x
     real(c_double) :: y
     integer(c_int) :: branchid
     real(c_double) :: chainage
     integer(c_int) :: link_number
     real(c_double) :: crest_level
     real(c_double) :: crest_width
     real(c_double) :: open_level
     real(c_double) :: contraction_coeff
     real(c_double) :: lat_contr_coeff
     real(c_double) :: allowed_flow_dir
     real(c_double) :: limit_flow_pos
     real(c_double) :: limit_flow_neg
  end type orifice
  type, public, bind(C) :: pump
     character(kind=c_char) :: id(MAXSTRINGLEN)
     real(c_double) :: x
     real(c_double) :: y
     integer(c_int) :: branchid
     real(c_double) :: chainage
     integer(c_int) :: link_number
     real(c_double) :: start_level_suction_side
     real(c_double) :: stop_level_suction_side
     real(c_double) :: capacity
     real(c_double) :: reduction_factor
     integer(c_int) :: actual_stage
     logical(c_bool) :: is_active
  end type pump
  type, public, bind(C) :: culvert
     character(kind=c_char) :: id(MAXSTRINGLEN)
     real(c_double) :: x
     real(c_double) :: y
     integer(c_int) :: branchid
     real(c_double) :: chainage
     integer(c_int) :: link_number
     real(c_double) :: left_level
     real(c_double) :: right_level
     integer(c_int) :: allowed_flow_dir
     integer(c_int) :: crosssection_id
     real(c_double) :: length
     integer(c_int) :: valve_inuse
     real(c_double) :: valve_opening
  end type culvert
  type(weir), dimension(:), allocatable, target :: weirs
  type(orifice), dimension(:), allocatable, target :: orifices
  type(pump), dimension(:), allocatable, target :: pumps
  type(culvert), dimension(:), allocatable, target :: culverts
