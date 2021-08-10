

  subroutine get_weir_by_id(id, c_weir)
    ! convert structures to pumps (fortran to c conversion)
    use modelGlobalData
    use m_1d_structures
    use m_weir

    character(len=*), intent(in) :: id
    type(weir), intent(out) :: c_weir

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


    c_weir%id = string_to_char_array(structure%id)
    c_weir%x = structure%xCoordinates(1)
    c_weir%y = structure%yCoordinates(1)
    c_weir%branchid = structure%ibran
    c_weir%chainage = structure%chainage
    c_weir%link_number = structure%linknumbers(1)
    c_weir%crest_level = fortranweir%crestlevel
    c_weir%crest_width = fortranweir%crestwidth
    c_weir%discharge_coeff = fortranweir%dischargecoeff
    c_weir%lat_dis_coeff = fortranweir%latdiscoeff
    c_weir%allowed_flow_dir = fortranweir%allowedflowdir

  end subroutine get_weir_by_id
  subroutine get_orifice_by_id(id, c_orifice)
    ! convert structures to pumps (fortran to c conversion)
    use modelGlobalData
    use m_1d_structures
    use m_orifice

    character(len=*), intent(in) :: id
    type(orifice), intent(out) :: c_orifice

    integer :: istruct               ! counting...

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


    c_orifice%id = string_to_char_array(structure%id)
    c_orifice%x = structure%xCoordinates(1)
    c_orifice%y = structure%yCoordinates(1)
    c_orifice%branchid = structure%ibran
    c_orifice%chainage = structure%chainage
    c_orifice%link_number = structure%linknumbers(1)
    c_orifice%crest_level = fortranorifice%crestlevel
    c_orifice%crest_width = fortranorifice%crestwidth
    c_orifice%open_level = fortranorifice%openlevel
    c_orifice%contraction_coeff = fortranorifice%contrcoeff
    c_orifice%lat_contr_coeff = fortranorifice%latcontrcoeff
    c_orifice%allowed_flow_dir = fortranorifice%allowedflowdir
    c_orifice%limit_flow_pos = fortranorifice%limitflowpos
    c_orifice%limit_flow_neg = fortranorifice%limitflowneg

  end subroutine get_orifice_by_id
  subroutine get_pump_by_id(id, c_pump)
    ! convert structures to pumps (fortran to c conversion)
    use modelGlobalData
    use m_1d_structures
    use m_pump

    character(len=*), intent(in) :: id
    type(pump), intent(out) :: c_pump

    integer :: istruct                 ! counting...

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


    c_pump%id = string_to_char_array(structure%id)
    c_pump%x = structure%xCoordinates(1)
    c_pump%y = structure%yCoordinates(1)
    c_pump%branchid = structure%ibran
    c_pump%chainage = structure%chainage
    c_pump%link_number = structure%linknumbers(1)
    c_pump%start_level_suction_side = fortranpump%ss_onlevel(1)
    c_pump%stop_level_suction_side = fortranpump%ss_offlevel(1)
    c_pump%capacity = fortranpump%capacity(1)
    c_pump%reduction_factor = fortranpump%reduction_factor
    c_pump%actual_stage = fortranpump%actual_stage
    c_pump%is_active = fortranpump%is_active

  end subroutine get_pump_by_id
  subroutine get_culvert_by_id(id, c_culvert)
    ! convert structures to pumps (fortran to c conversion)
    use modelGlobalData
    use m_1d_structures
    use m_culvert

    character(len=*), intent(in) :: id
    type(culvert), intent(out) :: c_culvert

    integer :: istruct                ! counting...

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


    c_culvert%id = string_to_char_array(structure%id)
    c_culvert%x = structure%xCoordinates(1)
    c_culvert%y = structure%yCoordinates(1)
    c_culvert%branchid = structure%ibran
    c_culvert%chainage = structure%chainage
    c_culvert%link_number = structure%linknumbers(1)
    c_culvert%left_level = fortranculvert%leftlevel
    c_culvert%right_level = fortranculvert%rightlevel
    c_culvert%allowed_flow_dir = fortranculvert%allowedflowdir
    c_culvert%crosssection_id = fortranculvert%crosssectionnr
    c_culvert%length = fortranculvert%length
    c_culvert%valve_inuse = fortranculvert%has_valve
    c_culvert%valve_opening = fortranculvert%valveOpening

  end subroutine get_culvert_by_id
