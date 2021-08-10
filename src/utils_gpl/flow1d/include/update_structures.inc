
  subroutine update_weirs(structures, weirs)
    ! convert structures to weirs (fortran to c conversion)
    use m_1d_structures
    use MessageHandling
    use m_weir

    implicit none

    type(t_structureSet), intent(in) :: structures
    type(weir), dimension(:), intent(inout), allocatable :: weirs

    integer :: istruct, iweir

    integer :: nweirs
    type(t_structure), pointer :: structure
    type(t_weir), pointer :: fortranweir
    type(weir) :: c_weir

    nweirs = 0

    write(msgbuf,*) 'Searching through', structures%Count, 'structures for weirs'
    call mess(LEVEL_DEBUG, msgbuf)

    do istruct=1,structures%Count
       structure => structures%struct(istruct)
       if ( structure%type .eq. ST_WEIR ) then
          fortranweir => structure%weir
          nweirs = nweirs + 1
       end if
    end do

    write(msgbuf,*) 'Checking if  weirs is allocated', allocated(weirs)
    call MESS(LEVEL_DEBUG, msgbuf)
    if ( allocated(weirs) ) then
       write(msgbuf,*) 'Deallocating weirs of size', shape(weirs)
       call mess(LEVEL_DEBUG, msgbuf)
       deallocate(weirs)
    end if

    write(msgbuf,*) 'Allocating  weirs to', nweirs
    call MESS(LEVEL_DEBUG, msgbuf)
    allocate(weirs(nweirs))
    write(msgbuf,*) 'Checking if  weirs is allocated', allocated(weirs)
    call MESS(LEVEL_DEBUG, msgbuf)


    iweir = 0
    do istruct=1,structures%Count
       structure => structures%struct(istruct)
       if ( structure%type .eq. ST_WEIR ) then
          fortranweir => structure%weir
          iweir = iweir + 1
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
          weirs(iweir) = c_weir
       end if
    end do

    write(msgbuf,*) 'Allocated weirs to shape', shape(weirs)
    call mess(LEVEL_DEBUG, msgbuf)

  end subroutine update_weirs
  subroutine update_orifices(structures, orifices)
    ! convert structures to orifices (fortran to c conversion)
    use m_1d_structures
    use MessageHandling
    use m_orifice

    implicit none

    type(t_structureSet), intent(in) :: structures
    type(orifice), dimension(:), intent(inout), allocatable :: orifices

    integer :: istruct, iorifice

    integer :: norifices
    type(t_structure), pointer :: structure
    type(t_orifice), pointer :: fortranorifice
    type(orifice) :: c_orifice

    norifices = 0

    write(msgbuf,*) 'Searching through', structures%Count, 'structures for orifices'
    call mess(LEVEL_DEBUG, msgbuf)

    do istruct=1,structures%Count
       structure => structures%struct(istruct)
       if ( structure%type .eq. ST_ORIFICE ) then
          fortranorifice => structure%orifice
          norifices = norifices + 1
       end if
    end do

    write(msgbuf,*) 'Checking if  orifices is allocated', allocated(orifices)
    call MESS(LEVEL_DEBUG, msgbuf)
    if ( allocated(orifices) ) then
       write(msgbuf,*) 'Deallocating orifices of size', shape(orifices)
       call mess(LEVEL_DEBUG, msgbuf)
       deallocate(orifices)
    end if

    write(msgbuf,*) 'Allocating  orifices to', norifices
    call MESS(LEVEL_DEBUG, msgbuf)
    allocate(orifices(norifices))
    write(msgbuf,*) 'Checking if  orifices is allocated', allocated(orifices)
    call MESS(LEVEL_DEBUG, msgbuf)


    iorifice = 0
    do istruct=1,structures%Count
       structure => structures%struct(istruct)
       if ( structure%type .eq. ST_ORIFICE ) then
          fortranorifice => structure%orifice
          iorifice = iorifice + 1
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
          orifices(iorifice) = c_orifice
       end if
    end do

    write(msgbuf,*) 'Allocated orifices to shape', shape(orifices)
    call mess(LEVEL_DEBUG, msgbuf)

  end subroutine update_orifices
  subroutine update_pumps(structures, pumps)
    ! convert structures to pumps (fortran to c conversion)
    use m_1d_structures
    use MessageHandling
    use m_pump

    implicit none

    type(t_structureSet), intent(in) :: structures
    type(pump), dimension(:), intent(inout), allocatable :: pumps

    integer :: istruct, ipump

    integer :: npumps
    type(t_structure), pointer :: structure
    type(t_pump), pointer :: fortranpump
    type(pump) :: c_pump

    npumps = 0

    write(msgbuf,*) 'Searching through', structures%Count, 'structures for pumps'
    call mess(LEVEL_DEBUG, msgbuf)

    do istruct=1,structures%Count
       structure => structures%struct(istruct)
       if ( structure%type .eq. ST_PUMP ) then
          fortranpump => structure%pump
          npumps = npumps + 1
       end if
    end do

    write(msgbuf,*) 'Checking if  pumps is allocated', allocated(pumps)
    call MESS(LEVEL_DEBUG, msgbuf)
    if ( allocated(pumps) ) then
       write(msgbuf,*) 'Deallocating pumps of size', shape(pumps)
       call mess(LEVEL_DEBUG, msgbuf)
       deallocate(pumps)
    end if

    write(msgbuf,*) 'Allocating  pumps to', npumps
    call MESS(LEVEL_DEBUG, msgbuf)
    allocate(pumps(npumps))
    write(msgbuf,*) 'Checking if  pumps is allocated', allocated(pumps)
    call MESS(LEVEL_DEBUG, msgbuf)


    ipump = 0
    do istruct=1,structures%Count
       structure => structures%struct(istruct)
       if ( structure%type .eq. ST_PUMP ) then
          fortranpump => structure%pump
          ipump = ipump + 1
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
          pumps(ipump) = c_pump
       end if
    end do

    write(msgbuf,*) 'Allocated pumps to shape', shape(pumps)
    call mess(LEVEL_DEBUG, msgbuf)

  end subroutine update_pumps
  subroutine update_culverts(structures, culverts)
    ! convert structures to culverts (fortran to c conversion)
    use m_1d_structures
    use MessageHandling
    use m_culvert

    implicit none

    type(t_structureSet), intent(in) :: structures
    type(culvert), dimension(:), intent(inout), allocatable :: culverts

    integer :: istruct, iculvert

    integer :: nculverts
    type(t_structure), pointer :: structure
    type(t_culvert), pointer :: fortranculvert
    type(culvert) :: c_culvert

    nculverts = 0

    write(msgbuf,*) 'Searching through', structures%Count, 'structures for culverts'
    call mess(LEVEL_DEBUG, msgbuf)

    do istruct=1,structures%Count
       structure => structures%struct(istruct)
       if ( structure%type .eq. ST_CULVERT ) then
          fortranculvert => structure%culvert
          nculverts = nculverts + 1
       end if
    end do

    write(msgbuf,*) 'Checking if  culverts is allocated', allocated(culverts)
    call MESS(LEVEL_DEBUG, msgbuf)
    if ( allocated(culverts) ) then
       write(msgbuf,*) 'Deallocating culverts of size', shape(culverts)
       call mess(LEVEL_DEBUG, msgbuf)
       deallocate(culverts)
    end if

    write(msgbuf,*) 'Allocating  culverts to', nculverts
    call MESS(LEVEL_DEBUG, msgbuf)
    allocate(culverts(nculverts))
    write(msgbuf,*) 'Checking if  culverts is allocated', allocated(culverts)
    call MESS(LEVEL_DEBUG, msgbuf)


    iculvert = 0
    do istruct=1,structures%Count
       structure => structures%struct(istruct)
       if ( structure%type .eq. ST_CULVERT ) then
          fortranculvert => structure%culvert
          iculvert = iculvert + 1
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
          culverts(iculvert) = c_culvert
       end if
    end do

    write(msgbuf,*) 'Allocated culverts to shape', shape(culverts)
    call mess(LEVEL_DEBUG, msgbuf)

  end subroutine update_culverts
