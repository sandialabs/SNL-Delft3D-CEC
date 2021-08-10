

  subroutine set_weir_by_id(id, c_weir)
    ! convert structures to pumps (fortran to c conversion)
    use modelGlobalData
    use m_1d_structures
    use m_weir
    use MessageHandling
    character(len=*), intent(in) :: id

    type(weir), intent(in) :: c_weir

    integer :: istruct                ! counting...

    type(t_structure), pointer :: structure
    type(t_weir), pointer :: fortranweir


    do istruct=1,pnetwork%sts%Count
       structure => pnetwork%sts%struct(istruct)
       if (structure%id == id) then
          if (.not. structure%type .eq. ST_WEIR) then
             write(msgbuf,*) "Structure", id, "is not of type weir"
             call MESS(LEVEL_ERROR, msgbuf)
          end if
          fortranweir => structure%weir
          exit
       end if
    end do

    structure%id = char_array_to_string(c_weir%id)
    write(msgbuf,*) 'setting structure%xCoordinates(1) to', c_weir%x
    call mess(LEVEL_DEBUG, msgbuf)
    structure%xCoordinates(1) = c_weir%x
    write(msgbuf,*) 'setting structure%yCoordinates(1) to', c_weir%y
    call mess(LEVEL_DEBUG, msgbuf)
    structure%yCoordinates(1) = c_weir%y
    write(msgbuf,*) 'setting structure%ibran to', c_weir%branchid
    call mess(LEVEL_DEBUG, msgbuf)
    structure%ibran = c_weir%branchid
    write(msgbuf,*) 'setting structure%chainage to', c_weir%chainage
    call mess(LEVEL_DEBUG, msgbuf)
    structure%chainage = c_weir%chainage
    call mess(LEVEL_DEBUG, msgbuf)
    write(msgbuf,*) 'setting structure%linknumbers(1) to', c_weir%link_number
    call mess(LEVEL_DEBUG, msgbuf)
    structure%linknumbers(1) = c_weir%link_number
    write(msgbuf,*) 'setting fortranweir%crestlevel to', c_weir%crest_level
    call mess(LEVEL_DEBUG, msgbuf)
    fortranweir%crestlevel = c_weir%crest_level
    write(msgbuf,*) 'setting fortranweir%crestwidth to', c_weir%crest_width
    call mess(LEVEL_DEBUG, msgbuf)
    fortranweir%crestwidth = c_weir%crest_width
    write(msgbuf,*) 'setting fortranweir%dischargecoeff to', c_weir%discharge_coeff
    call mess(LEVEL_DEBUG, msgbuf)
    fortranweir%dischargecoeff = c_weir%discharge_coeff
    write(msgbuf,*) 'setting fortranweir%latdiscoeff to', c_weir%lat_dis_coeff
    call mess(LEVEL_DEBUG, msgbuf)
    fortranweir%latdiscoeff = c_weir%lat_dis_coeff
    write(msgbuf,*) 'setting fortranweir%allowedflowdir to', c_weir%allowed_flow_dir
    call mess(LEVEL_DEBUG, msgbuf)
    fortranweir%allowedflowdir = c_weir%allowed_flow_dir
  end subroutine set_weir_by_id
  subroutine set_orifice_by_id(id, c_orifice)
    ! convert structures to pumps (fortran to c conversion)
    use modelGlobalData
    use m_1d_structures
    use m_orifice
    use MessageHandling
    character(len=*), intent(in) :: id

    type(orifice), intent(in) :: c_orifice

    integer :: istruct            ! counting...

    type(t_structure), pointer :: structure
    type(t_orifice), pointer :: fortranorifice


    do istruct=1,pnetwork%sts%Count
       structure => pnetwork%sts%struct(istruct)
       if (structure%id == id) then
          if (.not. structure%type .eq. ST_ORIFICE) then
             write(msgbuf,*) "Structure", id, "is not of type orifice"
             call MESS(LEVEL_ERROR, msgbuf)
          end if
          fortranorifice => structure%orifice
          exit
       end if
    end do

    structure%id = char_array_to_string(c_orifice%id)
    write(msgbuf,*) 'setting structure%xCoordinates(1) to', c_orifice%x
    call mess(LEVEL_DEBUG, msgbuf)
    structure%xCoordinates(1) = c_orifice%x
    write(msgbuf,*) 'setting structure%yCoordinates(1) to', c_orifice%y
    call mess(LEVEL_DEBUG, msgbuf)
    structure%yCoordinates(1) = c_orifice%y
    write(msgbuf,*) 'setting structure%ibran to', c_orifice%branchid
    call mess(LEVEL_DEBUG, msgbuf)
    structure%ibran = c_orifice%branchid
    write(msgbuf,*) 'setting structure%chainage to', c_orifice%chainage
    call mess(LEVEL_DEBUG, msgbuf)
    structure%chainage = c_orifice%chainage
    call mess(LEVEL_DEBUG, msgbuf)
    write(msgbuf,*) 'setting structure%linknumbers(1) to', c_orifice%link_number
    call mess(LEVEL_DEBUG, msgbuf)
    structure%linknumbers(1) = c_orifice%link_number
    write(msgbuf,*) 'setting fortranorifice%crestlevel to', c_orifice%crest_level
    call mess(LEVEL_DEBUG, msgbuf)
    fortranorifice%crestlevel = c_orifice%crest_level
    write(msgbuf,*) 'setting fortranorifice%crestwidth to', c_orifice%crest_width
    call mess(LEVEL_DEBUG, msgbuf)
    fortranorifice%crestwidth = c_orifice%crest_width
    write(msgbuf,*) 'setting fortranorifice%openlevel to', c_orifice%open_level
    call mess(LEVEL_DEBUG, msgbuf)
    fortranorifice%openlevel = c_orifice%open_level
    write(msgbuf,*) 'setting fortranorifice%contrcoeff to', c_orifice%contraction_coeff
    call mess(LEVEL_DEBUG, msgbuf)
    fortranorifice%contrcoeff = c_orifice%contraction_coeff
    write(msgbuf,*) 'setting fortranorifice%latcontrcoeff to', c_orifice%lat_contr_coeff
    call mess(LEVEL_DEBUG, msgbuf)
    fortranorifice%latcontrcoeff = c_orifice%lat_contr_coeff
    write(msgbuf,*) 'setting fortranorifice%allowedflowdir to', c_orifice%allowed_flow_dir
    call mess(LEVEL_DEBUG, msgbuf)
    fortranorifice%allowedflowdir = c_orifice%allowed_flow_dir
    write(msgbuf,*) 'setting fortranorifice%limitflowpos to', c_orifice%limit_flow_pos
    call mess(LEVEL_DEBUG, msgbuf)
    fortranorifice%limitflowpos = c_orifice%limit_flow_pos
    write(msgbuf,*) 'setting fortranorifice%limitflowneg to', c_orifice%limit_flow_neg
    call mess(LEVEL_DEBUG, msgbuf)
    fortranorifice%limitflowneg = c_orifice%limit_flow_neg
  end subroutine set_orifice_by_id
  subroutine set_pump_by_id(id, c_pump)
    ! convert structures to pumps (fortran to c conversion)
    use modelGlobalData
    use m_1d_structures
    use m_pump
    use MessageHandling
    character(len=*), intent(in) :: id

    type(pump), intent(in) :: c_pump

    integer :: istruct             ! counting...

    type(t_structure), pointer :: structure
    type(t_pump), pointer :: fortranpump


    do istruct=1,pnetwork%sts%Count
       structure => pnetwork%sts%struct(istruct)
       if (structure%id == id) then
          if (.not. structure%type .eq. ST_PUMP) then
             write(msgbuf,*) "Structure", id, "is not of type pump"
             call MESS(LEVEL_ERROR, msgbuf)
          end if
          fortranpump => structure%pump
          exit
       end if
    end do

    structure%id = char_array_to_string(c_pump%id)
    write(msgbuf,*) 'setting structure%xCoordinates(1) to', c_pump%x
    call mess(LEVEL_DEBUG, msgbuf)
    structure%xCoordinates(1) = c_pump%x
    write(msgbuf,*) 'setting structure%yCoordinates(1) to', c_pump%y
    call mess(LEVEL_DEBUG, msgbuf)
    structure%yCoordinates(1) = c_pump%y
    write(msgbuf,*) 'setting structure%ibran to', c_pump%branchid
    call mess(LEVEL_DEBUG, msgbuf)
    structure%ibran = c_pump%branchid
    write(msgbuf,*) 'setting structure%chainage to', c_pump%chainage
    call mess(LEVEL_DEBUG, msgbuf)
    structure%chainage = c_pump%chainage
    call mess(LEVEL_DEBUG, msgbuf)
    write(msgbuf,*) 'setting structure%linknumbers(1) to', c_pump%link_number
    call mess(LEVEL_DEBUG, msgbuf)
    structure%linknumbers(1) = c_pump%link_number
    write(msgbuf,*) 'setting fortranpump%ss_onlevel(1) to', c_pump%start_level_suction_side
    call mess(LEVEL_DEBUG, msgbuf)
    fortranpump%ss_onlevel(1) = c_pump%start_level_suction_side
    write(msgbuf,*) 'setting fortranpump%ss_offlevel(1) to', c_pump%stop_level_suction_side
    call mess(LEVEL_DEBUG, msgbuf)
    fortranpump%ss_offlevel(1) = c_pump%stop_level_suction_side
    write(msgbuf,*) 'setting fortranpump%capacity(1) to', c_pump%capacity
    call mess(LEVEL_DEBUG, msgbuf)
    fortranpump%capacity(1) = c_pump%capacity
    write(msgbuf,*) 'setting fortranpump%reduction_factor to', c_pump%reduction_factor
    call mess(LEVEL_DEBUG, msgbuf)
    fortranpump%reduction_factor = c_pump%reduction_factor
    write(msgbuf,*) 'setting fortranpump%actual_stage to', c_pump%actual_stage
    call mess(LEVEL_DEBUG, msgbuf)
    fortranpump%actual_stage = c_pump%actual_stage
    write(msgbuf,*) 'setting fortranpump%is_active to', c_pump%is_active
    call mess(LEVEL_DEBUG, msgbuf)
    fortranpump%is_active = c_pump%is_active
  end subroutine set_pump_by_id
  subroutine set_culvert_by_id(id, c_culvert)
    ! convert structures to pumps (fortran to c conversion)
    use modelGlobalData
    use m_1d_structures
    use m_culvert
    use MessageHandling
    character(len=*), intent(in) :: id

    type(culvert), intent(in) :: c_culvert

    integer :: istruct               ! counting...

    type(t_structure), pointer :: structure
    type(t_culvert), pointer :: fortranculvert


    do istruct=1,pnetwork%sts%Count
       structure => pnetwork%sts%struct(istruct)
       if (structure%id == id) then
          if (.not. structure%type .eq. ST_CULVERT) then
             write(msgbuf,*) "Structure", id, "is not of type culvert"
             call MESS(LEVEL_ERROR, msgbuf)
          end if
          fortranculvert => structure%culvert
          exit
       end if
    end do

    structure%id = char_array_to_string(c_culvert%id)
    write(msgbuf,*) 'setting structure%xCoordinates(1) to', c_culvert%x
    call mess(LEVEL_DEBUG, msgbuf)
    structure%xCoordinates(1) = c_culvert%x
    write(msgbuf,*) 'setting structure%yCoordinates(1) to', c_culvert%y
    call mess(LEVEL_DEBUG, msgbuf)
    structure%yCoordinates(1) = c_culvert%y
    write(msgbuf,*) 'setting structure%ibran to', c_culvert%branchid
    call mess(LEVEL_DEBUG, msgbuf)
    structure%ibran = c_culvert%branchid
    write(msgbuf,*) 'setting structure%chainage to', c_culvert%chainage
    call mess(LEVEL_DEBUG, msgbuf)
    structure%chainage = c_culvert%chainage
    write(msgbuf,*) 'setting structure%linknumbers(1) to', c_culvert%link_number
    call mess(LEVEL_DEBUG, msgbuf)
    structure%linknumbers(1) = c_culvert%link_number
    write(msgbuf,*) 'setting fortranculvert%leftlevel to', c_culvert%left_level
    call mess(LEVEL_DEBUG, msgbuf)
    fortranculvert%leftlevel = c_culvert%left_level
    write(msgbuf,*) 'setting fortranculvert%rightlevel to', c_culvert%right_level
    call mess(LEVEL_DEBUG, msgbuf)
    fortranculvert%rightlevel = c_culvert%right_level
    write(msgbuf,*) 'setting fortranculvert%allowedflowdir to', c_culvert%allowed_flow_dir
    call mess(LEVEL_DEBUG, msgbuf)
    fortranculvert%allowedflowdir = c_culvert%allowed_flow_dir
    write(msgbuf,*) 'setting fortranculvert%crosssectionnr to', c_culvert%crosssection_id
    call mess(LEVEL_DEBUG, msgbuf)
    fortranculvert%crosssectionnr = c_culvert%crosssection_id
    write(msgbuf,*) 'setting fortranculvert%length to', c_culvert%length
    call mess(LEVEL_DEBUG, msgbuf)
    fortranculvert%length = c_culvert%length
    write(msgbuf,*) 'setting fortranculvert%valve_onoff to', c_culvert%valve_inuse
    call mess(LEVEL_DEBUG, msgbuf)
    fortranculvert%has_valve = c_culvert%valve_inuse
    write(msgbuf,*) 'setting fortranculvert%valveOpening to', c_culvert%valve_opening
    call mess(LEVEL_DEBUG, msgbuf)
    fortranculvert%valveOpening = c_culvert%valve_opening
  end subroutine set_culvert_by_id
