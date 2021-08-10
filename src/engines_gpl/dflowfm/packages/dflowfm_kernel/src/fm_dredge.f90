module m_rddredge
   use precision
   use handles
   use m_dredge_data

   implicit none

   public rddredge
   public initdredge
   public clrdredge
   
contains

   subroutine initdredge(dredgepar)
      use precision
      !
      implicit none
      !
      type(sv_dredge)   :: dredgepar
      integer           :: istat
!
!! executable statements -------------------------------------------------------
!
      nullify(dredgepar%link_percentage)
      nullify(dredgepar%link_distance)
      nullify(dredgepar%link_sum)
      nullify(dredgepar%dzdred)
      nullify(dredgepar%refplane)
      nullify(dredgepar%voldred)
      nullify(dredgepar%totvoldred)
      nullify(dredgepar%totvoldredfrac)
      nullify(dredgepar%globalareadred)
      nullify(dredgepar%voldune)
      nullify(dredgepar%percsupl)
      nullify(dredgepar%totvoldump)
      nullify(dredgepar%totvoldumpfrac)
      nullify(dredgepar%localareadump)
      nullify(dredgepar%globalareadump)
      nullify(dredgepar%globaldumpcap)
      nullify(dredgepar%voldump)
      !
      dredgepar%dredge_domainnr = 0
      dredgepar%dredge_ndomains = 0
      dredgepar%nadred          = 0
      dredgepar%nadump          = 0
      dredgepar%nasupl          = 0
      dredgepar%nalink          = 0
      dredgepar%ntimaccum       = 0.0_fp
      !
      nullify(dredgepar%link_def)
      nullify(dredgepar%ndredged)
      nullify(dredgepar%nploughed)
      !
      dredgepar%tsmortime       = .false.
      dredgepar%firstdredge     = .true.
      !
      nullify(dredgepar%dredge_areas)
      nullify(dredgepar%dump_areas)
      dredgepar%dredgefile      = ' '
      !
      nullify(dredgepar%dredge_prop)
      nullify(dredgepar%dump_prop)
      !
   end subroutine initdredge
   !
   !
   !
   subroutine clrdredge(dredgepar)

      use precision
      use table_handles, only: cleartable
      !
      implicit none
      !
      ! Local variables
      !
      integer           :: i, istat
      type(sv_dredge)   :: dredgepar
      !
      !! executable statements -------------------------------------------------------
      !
      if (associated(dredgepar%link_percentage)) deallocate (dredgepar%link_percentage, STAT = istat)
      if (associated(dredgepar%link_distance))   deallocate (dredgepar%link_distance  , STAT = istat)
      if (associated(dredgepar%link_sum))        deallocate (dredgepar%link_sum       , STAT = istat)
      if (associated(dredgepar%dzdred))          deallocate (dredgepar%dzdred         , STAT = istat)
      if (associated(dredgepar%refplane))        deallocate (dredgepar%refplane       , STAT = istat)
      if (associated(dredgepar%voldred))         deallocate (dredgepar%voldred        , STAT = istat)
      if (associated(dredgepar%totvoldred))      deallocate (dredgepar%totvoldred     , STAT = istat)
      if (associated(dredgepar%totvoldredfrac))      deallocate (dredgepar%totvoldred     , STAT = istat)
      if (associated(dredgepar%globalareadred))  deallocate (dredgepar%globalareadred , STAT = istat)
      if (associated(dredgepar%voldune))         deallocate (dredgepar%voldune        , STAT = istat)
      if (associated(dredgepar%percsupl))        deallocate (dredgepar%percsupl       , STAT = istat)
      if (associated(dredgepar%totvoldump))      deallocate (dredgepar%totvoldump     , STAT = istat)
      if (associated(dredgepar%totvoldumpfrac))      deallocate (dredgepar%totvoldump     , STAT = istat)
      if (associated(dredgepar%localareadump))   deallocate (dredgepar%localareadump  , STAT = istat)
      if (associated(dredgepar%globalareadump))  deallocate (dredgepar%globalareadump , STAT = istat)
      if (associated(dredgepar%globaldumpcap))   deallocate (dredgepar%globaldumpcap  , STAT = istat)
      if (associated(dredgepar%voldump))         deallocate (dredgepar%voldump        , STAT = istat)
      !
      if (associated(dredgepar%link_def))        deallocate (dredgepar%link_def       , STAT = istat)
      if (associated(dredgepar%ndredged))        deallocate (dredgepar%ndredged       , STAT = istat)
      if (associated(dredgepar%nploughed))       deallocate (dredgepar%nploughed      , STAT = istat)
      !
      if (associated(dredgepar%dredge_areas))    deallocate (dredgepar%dredge_areas   , STAT = istat)
      if (associated(dredgepar%dump_areas))      deallocate (dredgepar%dump_areas     , STAT = istat)
      !
      if (associated(dredgepar%dredge_prop)) then
         do i = 1, dredgepar%nadred
            if (associated(dredgepar%dredge_prop(i)%nm))             deallocate (dredgepar%dredge_prop(i)%nm                  , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%inm))            deallocate (dredgepar%dredge_prop(i)%inm                 , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%area))           deallocate (dredgepar%dredge_prop(i)%area                , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%hdune))          deallocate (dredgepar%dredge_prop(i)%hdune               , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%dz_dredge))      deallocate (dredgepar%dredge_prop(i)%dz_dredge           , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%dunetoplevel))   deallocate (dredgepar%dredge_prop(i)%dunetoplevel        , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%triggerlevel))   deallocate (dredgepar%dredge_prop(i)%triggerlevel        , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%bedlevel))       deallocate (dredgepar%dredge_prop(i)%bedlevel            , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%troughlevel))    deallocate (dredgepar%dredge_prop(i)%troughlevel         , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%sedimentdepth))  deallocate (dredgepar%dredge_prop(i)%sedimentdepth       , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%sortvar))        deallocate (dredgepar%dredge_prop(i)%sortvar             , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%triggered))      deallocate (dredgepar%dredge_prop(i)%triggered           , STAT = istat)
            if (associated(dredgepar%dredge_prop(i)%reflevel))       deallocate (dredgepar%dredge_prop(i)%reflevel            , STAT = istat)
         enddo
         deallocate (dredgepar%dredge_prop    , STAT = istat)
      endif
      !
      if (associated(dredgepar%dump_prop)) then
         do i = 1, dredgepar%nadump
            if (associated(dredgepar%dump_prop(i)%nm))               deallocate (dredgepar%dump_prop(i)%nm                    , STAT = istat)
            if (associated(dredgepar%dump_prop(i)%inm))              deallocate (dredgepar%dump_prop(i)%inm                   , STAT = istat)
            if (associated(dredgepar%dump_prop(i)%area))             deallocate (dredgepar%dump_prop(i)%area                  , STAT = istat)
            if (associated(dredgepar%dump_prop(i)%hdune))            deallocate (dredgepar%dump_prop(i)%hdune                 , STAT = istat)
            if (associated(dredgepar%dump_prop(i)%bedlevel))         deallocate (dredgepar%dump_prop(i)%bedlevel              , STAT = istat)
            if (associated(dredgepar%dump_prop(i)%dz_dump))          deallocate (dredgepar%dump_prop(i)%dz_dump               , STAT = istat)
            if (associated(dredgepar%dump_prop(i)%sortvar))          deallocate (dredgepar%dump_prop(i)%sortvar               , STAT = istat)
            if (associated(dredgepar%dump_prop(i)%reflevel))         deallocate (dredgepar%dump_prop(i)%reflevel              , STAT = istat)
         enddo
         deallocate (dredgepar%dump_prop      , STAT = istat)
      endif
      !
      call cleartable(dredgepar%tseriesfile)
      
   end subroutine clrdredge

   subroutine rddredge(dredgepar, md_dredgefile, error)

!!--declarations----------------------------------------------------------------
       use precision
       use mathconsts     
       use properties 
       use table_handles    
       use message_module
       use MessageHandling, only: LEVEL_INFO, mess
       use fm_polygon_module
       use m_alloc
       use m_flow
       use m_flowgeom
   	 use m_flowtimes
       use network_data
       use unstruc_files, only: mdia
       use m_sediment, only: stmpar
       !use m_dredge_data
       use m_bedform
       use interp
       use m_depfil_stm
       use m_partitioninfo
       !
       implicit none
       !
       ! The following list of pointer parameters is used to point inside the data structure
       !
       type (handletype)                , pointer :: tseriesfile
       real(fp)      , dimension(:,:)   , pointer :: link_percentage
       real(fp)      , dimension(:)     , pointer :: link_distance
       real(fp)      , dimension(:,:)   , pointer :: link_sum
       real(fp)      , dimension(:)     , pointer :: dzdred
       real(fp)      , dimension(:)     , pointer :: refplane
       real(fp)      , dimension(:,:)   , pointer :: voldred
       real(fp)      , dimension(:)     , pointer :: voldune
       real(fp)      , dimension(:)     , pointer :: totvoldred
       real(fp)      , dimension(:,:)   , pointer :: totvoldredfrac
       real(fp)      , dimension(:)     , pointer :: globalareadred
       real(fp)      , dimension(:,:)   , pointer :: voldump
       real(fp)      , dimension(:,:)   , pointer :: percsupl
       real(fp)      , dimension(:)     , pointer :: totvoldump
       real(fp)      , dimension(:,:)   , pointer :: totvoldumpfrac
       real(fp)      , dimension(:)     , pointer :: localareadump
       real(fp)      , dimension(:)     , pointer :: globalareadump
       real(fp)      , dimension(:)     , pointer :: globaldumpcap
       integer                          , pointer :: nadred
       integer                          , pointer :: nadump
       integer                          , pointer :: nasupl
       integer                          , pointer :: nalink
       integer                          , pointer :: lsedtot
       integer       , dimension(:,:)   , pointer :: link_def
       real(fp)       , dimension(:)    , pointer :: ndredged
       real(fp)       , dimension(:)    , pointer :: nploughed
       logical                          , pointer :: tsmortime
       character( 80), dimension(:)     , pointer :: dredge_areas
       character( 80), dimension(:)     , pointer :: dump_areas
       type (dredtype), dimension(:)    , pointer :: dredge_prop
       type (dumptype), dimension(:)    , pointer :: dump_prop
       character(20) , dimension(:)     , pointer :: namsed
       logical                          , pointer :: lfbedfrm
       logical                          , pointer :: cmpupd
   !
   ! Local variables
   !
       integer                                 :: cntdred
       integer                                 :: cntdump
       integer                                 :: cntssrc
       integer                                 :: cntsupl
       integer                                 :: cntlink
       integer                                 :: cntsedidx
       integer             , dimension(4)      :: def_active
       integer                                 :: def_chkloc
       integer                                 :: def_depthdef
       integer                                 :: def_dredgedistr
       integer                                 :: def_dumpdistr
       integer                                 :: def_dr2dudistr
       integer                                 :: def_drtrigger
       integer                                 :: i
       integer                                 :: ia
       integer                                 :: ic
       integer                                 :: inm
       integer                                 :: icdr
       integer                                 :: icdu
       integer                                 :: ilink             ! first link for dredge location
       integer                                 :: istat
       integer                                 :: iter
       integer                                 :: j
       integer                                 :: j2
       integer                                 :: lsed
       integer                                 :: n
       integer                                 :: nm
       integer                                 :: nnod
       integer                                 :: nmaxddb
       integer                                 :: nmcor
       integer                                 :: noutletlinks
       integer                                 :: npnt
       integer, allocatable, dimension(:)      :: imask
       integer, allocatable, dimension(:)      :: ipdr
       integer, pointer    , dimension(:)      :: ipdu
       integer, allocatable, dimension(:)      :: npdr
       integer, pointer    , dimension(:)      :: npdu
       integer             , dimension(4)      :: tmp_active
       integer                                 :: totnpdr           ! total number of points in dredge polygons
       integer                                 :: totnpdu           ! total number of points in dump   polygons
       real(fp)                                :: def_clearance
       real(fp)                                :: sumperc
       real(fp), allocatable, dimension(:)     :: xdr
       real(fp), allocatable, dimension(:)     :: xdu
       real(fp)             , dimension(2)     :: x1
       real(fp), allocatable, dimension(:)     :: ydr
       real(fp), allocatable, dimension(:)     :: ydu
       real(fp)             , dimension(2)     :: y1
       real(fp)                                :: loctemp
       real(fp)                                :: sedperc
       real(fp)                                :: def_dredge_depth
       real(fp)                                :: def_maxvolrate
       real(fp)                                :: def_mindumpdepth
       real(fp)                                :: def_alpha_dh
       real(fp)                                :: def_plough_effic
       real(fp)                                :: rmissval
       real(sp)                                :: versionnr
       real(sp)                                :: versionnrinput
       logical                                 :: ex
       logical, intent(out)                    :: error
       logical, external                       :: stringsequalinsens
       logical                                 :: succes
       logical                                 :: unique
       logical                                 :: def_dredgewhendry
       logical                                 :: def_dumpwhendry
       logical                                 :: def_if_morfac_0
       logical                                 :: def_obey_cmp
       logical                                 :: def_use_dunes
       logical                                 :: lastdumparea
       logical                                 :: sfound
       logical                                 :: triggerall
       character(11)                           :: fmttmp            ! Format file ('formatted  ') 
       character(80)                           :: name
       character(80)                           :: parname
       character(80)                           :: dredgetype
       character(20)                           :: sedname
       character(1024)                         :: message
       character(20)                           :: areatp
       character(256)                          :: filename
       character(256)                          :: polygonfile
       character(256)                          :: refplanefile
       character(80)                           :: stringval
       character(256)                          :: errmsg
       character(255), intent(in)              :: md_dredgefile
       type(tree_data), pointer                :: dad_ptr
       type(tree_data), pointer                :: dredge_area_ptr
       type(tree_data), pointer                :: dump_area_ptr
       type(tree_data), pointer                :: link_ptr
       type(tree_data), pointer                :: node_ptr
       type(tree_data), pointer                :: pol_ptr
       type(dredtype) , pointer                :: pdredge
       type(dumptype) , pointer                :: pdump
       type(sv_dredge), intent(inout), target  :: dredgepar
   !
   !! executable statements -------------------------------------------------------
   !
       tseriesfile       => dredgepar%tseriesfile
       nadred            => dredgepar%nadred
       nadump            => dredgepar%nadump
       nasupl            => dredgepar%nasupl
       nalink            => dredgepar%nalink
       tsmortime         => dredgepar%tsmortime
       namsed            => stmpar%sedpar%namsed
       lfbedfrm          => bfmpar%lfbedfrm
       cmpupd            => stmpar%morpar%cmpupd
       lsedtot           => stmpar%lsedtot
       !
       error = .false.
       !
       if (.not.cmpupd) then
          errmsg = '*** ERROR Dredging and dumping not supported when bed composition updating is disabled. ***'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       !
       rmissval       = -9.99E9_fp
       !
       versionnr      = 1.02_sp
       loctemp        = 0.0_fp
       !
       call tree_create  ( "Dredging input", dad_ptr )
       call tree_put_data( dad_ptr, transfer(trim(md_dredgefile),node_value), 'STRING' )
       !
       ! Put dad-file in input tree
       !
       call prop_file('ini', trim(md_dredgefile), dad_ptr, istat)
       if (istat /= 0) then
          select case (istat)
          case(1)
             call write_error(FILE_NOT_FOUND//trim(md_dredgefile), unit=mdia)
          case(3)
             call write_error(PREMATURE_EOF//trim(md_dredgefile), unit=mdia)
          case default
             call write_error(FILE_READ_ERROR//trim(md_dredgefile), unit=mdia)
          endselect
          error = .true.
          return
       endif
       !
       ! Put polygon file in input tree
       !
       write (mdia,*)
       write (mdia,'(a)') '*** Start of Dredging & dumping input'
       write (mdia,'(a)') 'General'
       !
       filename = ''
       call prop_get_string(dad_ptr, 'General', 'PolygonFile', filename)
       nullify(pol_ptr)
       if (filename /= ' ') then
          call tree_get_node_by_name( dad_ptr, 'General', node_ptr )
          if (associated(node_ptr)) then
             call tree_get_node_by_name( node_ptr, 'PolygonFile', pol_ptr )
          endif
       else
          call tree_get_node_by_name( dad_ptr, 'DredgeFileInformation', node_ptr )
          if (associated(node_ptr)) then
             call tree_get_node_by_name( node_ptr, 'PolygonFile', pol_ptr )
          endif
       endif
       if (associated(pol_ptr)) then
          call tree_get_data_string(pol_ptr, polygonfile, succes)
          call prop_file('tekal', polygonfile, pol_ptr, istat)
          if (istat /= 0) then
             select case (istat)
                case(1)
                   call write_error(FILE_NOT_FOUND//trim(polygonfile), unit=mdia)
                case(3)
                   call write_error(PREMATURE_EOF//trim(polygonfile), unit=mdia)
                case default
                   call write_error(FILE_READ_ERROR//trim(polygonfile), unit=mdia)
             endselect
             error = .true.
             return
          endif
        else
          errmsg = '*** ERROR Unable to find keyword "PolygonFile" in *.dad file. ***'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       !
       ! Check version number of dredge input file
       !
       versionnrinput = 0.00_sp
       call prop_get_real(dad_ptr, 'DredgeFileInformation', 'FileVersion', versionnrinput)
       !
       if (comparereal(versionnrinput, versionnr) == -1) then
          write (errmsg,'(a,f6.2,a)') '*** ERROR Dredge input file must have version number ',versionnr, ' or higher ***'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       !
       def_depthdef      = DEPTHDEF_REFPLANE
       def_dredgedistr   = DREDGEDISTR_PROPORTIONAL
       def_dumpdistr     = DUMPDISTR_UNIFORM
       def_dr2dudistr    = DR2DUDISTR_PERCENTAGE
       def_chkloc        = CHKLOC_CENTRE
       def_clearance     = 0.0_fp
       def_dredgewhendry = .false.
       def_dumpwhendry   = .false.
       def_if_morfac_0   = .true.
       def_obey_cmp      = .true.
       def_drtrigger     = DREDGETRIG_POINTBYPOINT
       def_dredge_depth  = 1.0e10_fp
       def_maxvolrate    = -999.0_fp
       def_mindumpdepth  = -999.0_fp
       def_use_dunes     = .false.
       def_alpha_dh      = 0.5_fp
       def_plough_effic  = 0.0_fp
       !
       ! Allocate array for refplane
       !
       allocate (dredgepar%refplane (ndx), stat = istat)
       if (istat /= 0) then
          errmsg = 'ERROR rddredge: memory alloc error'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       !
       ! update local pointers
       !
       refplane          => dredgepar%refplane
       !
       refplane    = 0.0_fp
       refplane(1) = rmissval
       !
       filename    = ' '
       call prop_get_string(dad_ptr, 'General', 'TimeSeries', filename)
       !
       ! Intel 7.0 crashes on an inquire statement when file = ' '
       !
       if (filename == ' ') filename = 'dummyname'
       inquire (file = trim(filename), exist = ex)
       if (ex) then
          call readtable(tseriesfile, trim(filename), julrefdat, errmsg)
          if (.not. (validtable(tseriesfile))) then
             call write_error(errmsg, unit=mdia)
             error = .true.
             return
          endif
       elseif (filename /= 'dummyname') then
          errmsg =  'ERROR rddredge: Missing time series file "'//trim(filename)//'"'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       !
       call prop_get(dad_ptr, 'General', 'DepthDef', def_depthdef)
       if (def_depthdef < 1 .or. def_depthdef > DEPTHDEF_MAX) then
          errmsg = 'ERROR rddredge: Invalid default depth definition'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       call prop_get_logical(dad_ptr, 'General', 'TS_MorTimeScale', tsmortime)
       call prop_get(dad_ptr, 'General', 'DredgeDepth', def_dredge_depth)
       call prop_get(dad_ptr, 'General', 'Clearance'  , def_clearance)
       call prop_get(dad_ptr, 'General', 'MaxVolRate' , def_maxvolrate)
       call prop_get_integer(dad_ptr, 'General', 'InPolygon', def_chkloc)
       if (def_chkloc < 1 .or. def_chkloc > 3) then
          errmsg = 'ERROR rddredge: Invalid default for in polygon check'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       call prop_get_integer(dad_ptr, 'General', 'DredgeDistr', def_dredgedistr)
       if (def_dredgedistr < 1 .or. def_dredgedistr > DREDGEDISTR_MAX) then
          errmsg = 'ERROR rddredge: Invalid default dredge distribution'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       call prop_get_logical(dad_ptr, 'General', 'UseDunes'        , def_use_dunes)
       !
       if (def_use_dunes .and. .not. lfbedfrm) then
          errmsg = 'ERROR rddredge: UseDunes - Dunes can only be used when modelled.'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       call prop_get(dad_ptr, 'General', 'AlphaDuneHeight', def_alpha_dh)
       call prop_get(dad_ptr, 'General', 'PloughEfficiency', def_plough_effic)
       call prop_get_integer(dad_ptr, 'General', 'DumpDistr', def_dumpdistr)
       if (def_dumpdistr < 1 .or. def_dumpdistr > DUMPDISTR_MAX) then
          errmsg = 'ERROR rddredge: Invalid default dump distribution'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       call prop_get_integer(dad_ptr, 'General', 'DistrOverDump', def_dr2dudistr)
       if (def_dr2dudistr < 1 .or. def_dr2dudistr > DR2DUDISTR_MAX) then
          errmsg = 'ERROR rddredge: Invalid default distribution over dump areas'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       !
       def_active    = 0
       def_active(1) = -999
       if (validtable(tseriesfile)) then
          call gettable(tseriesfile, 'General'    , 'active', def_active(1), &
                          & def_active(2), def_active(3), 0, errmsg)
          if (def_active(3) > 1) then
             write(message,'(i3,3a)') def_active(3), &
                                    & ' active parameters specified in file "', &
                                    & trim(getfilename(tseriesfile)), '" instead of 1.'
             call write_error(errmsg, unit=mdia)
             error = .true.
             return
          elseif (def_active(3) == 1) then
             call checktable(tseriesfile   , def_active(1) , &
                               & def_active(2) , def_active(3) , &
                               & CHKTAB_LOGICAL, errmsg)
             def_active(4) = 1
          endif
       endif
       !
       call prop_get        (dad_ptr, 'General', 'MinimumDumpDepth', def_mindumpdepth)
       call prop_get_logical(dad_ptr, 'General', 'DredgeWhenDry'   , def_dredgewhendry)
       call prop_get_logical(dad_ptr, 'General', 'DumpWhenDry'     , def_dumpwhendry)
       call prop_get_logical(dad_ptr, 'General', 'ObeyCmp'           , def_obey_cmp)
       triggerall = .false.
       call prop_get_logical(dad_ptr, 'General', 'TriggerAll'        , triggerall)
       if (triggerall) then
          def_drtrigger = DREDGETRIG_ALLBYONE
       else
          def_drtrigger = DREDGETRIG_POINTBYPOINT
       endif
       call prop_get        (dad_ptr, 'General', 'DredgeTrigger'     , def_drtrigger)
       call prop_get_logical(dad_ptr, 'General', 'DredgeWhileMorfac0', def_if_morfac_0)
       !
       sfound = .false.
       ex     = .false.
       if ( associated(dad_ptr%child_nodes) ) then
          do i = 1, size(dad_ptr%child_nodes)
             link_ptr => dad_ptr%child_nodes(i)%node_ptr
             if (tree_get_name( link_ptr ) == 'domain') then
                errmsg = 'ERROR rddredge: Specification of domain decomposition nodes not supported in FM. Please adapt *.dad file.'
                call write_error(errmsg, unit=mdia)
                error = .true.
                return
             end if
       !      !
       !      stringval = ''
       !      call prop_get_string(link_ptr, '*', 'Name', stringval)
       !      if (stringval /= gdp%runid) cycle
       !      !
       !      ! First assume that 'RefPlane' contains a filename
       !      ! If the file does not exist, assume that 'RefPlane' contains
       !      ! a uniform value (real)
       !      !
       !      refplanefile = ''
       !      call prop_get_string(link_ptr, '*', 'RefPlane', refplanefile)
       !      !
       !      ! Intel 7.0 crashes on an inquire statement when file = ' '
       !      !
       !      if (refplanefile == ' ') refplanefile = 'dummyname'
       !      inquire (file = refplanefile, exist = ex)
       !      if (ex) then
       !         sfound = .true.
       !      else
       !         refplanefile = ' '
       !         call prop_get(link_ptr, '*', 'RefPlane', refplane(1))
       !         sfound = comparereal(refplane(1),rmissval) /= 0
       !      endif
          enddo
       endif
       if (.not. sfound) then
          !
          ! No domain specific RefPlane input found, try General block.
          ! Check if 'RefPlane' contains a filename.
          !
          refplanefile = ''
          call prop_get_string(dad_ptr, 'General', 'RefPlane', refplanefile)
          !
          ! Intel 7.0 crashes on an inquire statement when file = ' '
          !
          if (refplanefile == ' ') refplanefile = 'dummyname'
          inquire (file = refplanefile, exist = ex)
       endif
       if (ex) then
          !
          ! Space varying data has been specified
          !
          ! DEBUG TO DO
          !errmsg = 'ERROR rddredge: spatially varying refplane specified. Currently not supported in FM.'
          !call write_error(errmsg, unit=mdia)
          !error = .true.
          !return
          !\DEBUG
          !
          write(mdia,'(2a)') '  Reference plane             : ', trim(refplanefile)
          !
          fmttmp = 'formatted'
          error  = .false.
          call depfil_stm_double(mdia    ,error     ,refplanefile,fmttmp  , &
                    & refplane  ,1         ,1         ,griddim)
          if (error) then
             errmsg = 'ERROR rddredge: Could not read refplane file.'
             call write_error(errmsg, unit=mdia)
             error = .true.
             return
          end if
       else
          if (.not. sfound) then
             !
             ! No domain specific RefPlane input found, and no filename in the General block.
             ! Now consider the case that the General block contains a uniform value.
             !
             refplanefile = ' '
             call prop_get(dad_ptr, 'General', 'RefPlane', refplane(1))
          endif
          if (comparereal(refplane(1),rmissval) == 0) then
             !
             ! RefPlane keyword not found (neither in domain specific block, nor in General block)
             ! Let's use a default RefPlane = 0
             !
             refplane(1) = 0.0_fp
          endif
          do nm = 2, ndx
             refplane(nm) = refplane(1)
          enddo
          call mess(LEVEL_INFO, '  Reference plane             : ', refplane(1))
       endif
       !
       ! Read dimensions from input file
       ! Add dredgid's and dumpid's to the polygons in the input_tree
       ! They are used during reading
       !
       nadred  = 0
       nadump  = 0
       nasupl  = 0
       nalink  = 0
       totnpdr = 0
       totnpdu = 0
       !
       if ( associated(dad_ptr%child_nodes) ) then
          !
          ! Unfortunately, almost the complete input tree must be scanned, just
          ! to get the dimensions.
          !
          do i = 1, size(dad_ptr%child_nodes)
             link_ptr => dad_ptr%child_nodes(i)%node_ptr
             dredgetype = tree_get_name( link_ptr )
             !
             ! Distinguish the cases:
             ! - 'dredge'
             ! - 'sandmining'
             !
             select case ( dredgetype )
             case ('dredge', 'sandmining')
                !
                ! Dredge or sandmining area specification found - name must be unique
                ! nadred incremented by register_polygon call
                !
                areatp  = 'dredge'
                unique  = .true.
                call prop_get_string(link_ptr, '*', 'name', name)
                call register_polygon(name   , pol_ptr, nadred, totnpdr, &
                                    & areatp , unique   )
                if ( associated(link_ptr%child_nodes) ) then
                   areatp = 'dump'
                   unique = .false.
                   do j = 1, size(link_ptr%child_nodes)
                      !
                      ! Does link_ptr contain one or more children with name 'Dump'?
                      ! nadump incremented by register_polygon call
                      !
                      node_ptr => link_ptr%child_nodes(j)%node_ptr
                      parname = tree_get_name( node_ptr )
                      if (parname == 'dump') then
                         call prop_get_string(node_ptr, '*', 'dump', name)
                         call register_polygon(name   , pol_ptr, nadump, totnpdu, &
                                             & areatp , unique)
                         nalink = nalink + 1
                      endif
                   enddo
                endif
             case ('nourishment')
                !
                ! Nourishment specification found
                !
                nasupl = nasupl + 1        
                if ( associated(link_ptr%child_nodes) ) then
                   areatp = 'dump'
                   unique = .false.
                   do j = 1, size(link_ptr%child_nodes)
                      !
                      ! Does link_ptr contain one or more children with name 'Dump'?
                      !
                      node_ptr => link_ptr%child_nodes(j)%node_ptr
                      parname = tree_get_name( node_ptr )
                      if (parname == 'dump') then
                         call prop_get_string(link_ptr, '*', 'dump', name)
                         call register_polygon(name   , pol_ptr, nadump, totnpdu, &
                                             & areatp , unique    )
                         nalink = nalink + 1
                      endif
                   enddo
                endif
             case default
                !
                ! Ignore anything else
                !
             end select
          enddo
       endif
       !
       ! Allocate arrays used during computation
       !
                     allocate (dredgepar%link_def        (nalink               ,2        ), stat = istat)
       if (istat==0) allocate (dredgepar%ndredged        (nadred+nasupl                  ), stat = istat)
       if (istat==0) allocate (dredgepar%nploughed       (nadred+nasupl                  ), stat = istat)
       !                             
       if (istat==0) allocate (dredgepar%link_percentage (nalink               ,lsedtot  ), stat = istat)
       if (istat==0) allocate (dredgepar%link_distance   (nalink                         ), stat = istat)
       if (istat==0) allocate (dredgepar%link_sum        (nalink               ,lsedtot  ), stat = istat)
       if (istat==0) allocate (dredgepar%dzdred          (1:ndx                          ), stat = istat)
       if (istat==0) allocate (dredgepar%voldred         (nadred+nasupl        ,lsedtot+1), stat = istat)
       if (istat==0) allocate (dredgepar%totvoldred      (nadred+nasupl                  ), stat = istat)
       if (istat==0) allocate (dredgepar%totvoldredfrac  (nadred+nasupl        ,lsedtot+1), stat = istat)
       if (istat==0) allocate (dredgepar%globalareadred  (nadred+nasupl                  ), stat = istat)
       if (istat==0) allocate (dredgepar%voldune         (1:ndx                          ), stat = istat)
       if (istat==0) allocate (dredgepar%percsupl        (nasupl               ,lsedtot  ), stat = istat)
       if (istat==0) allocate (dredgepar%totvoldump      (nadump                         ), stat = istat)
       if (istat==0) allocate (dredgepar%totvoldumpfrac  (nadump               ,lsedtot  ), stat = istat)
       if (istat==0) allocate (dredgepar%localareadump   (nadump                         ), stat = istat)
       if (istat==0) allocate (dredgepar%globalareadump  (nadump                         ), stat = istat)
       if (istat==0) allocate (dredgepar%globaldumpcap   (nadump                         ), stat = istat)
       if (istat==0) allocate (dredgepar%voldump         (nadump               ,lsedtot  ), stat = istat)
       !
       if (istat==0) allocate (dredgepar%dredge_areas    (nadred+nasupl                  ), stat = istat)
       if (istat==0) allocate (dredgepar%dump_areas      (nadump                         ), stat = istat)
       !                             
       if (istat==0) allocate (dredgepar%dredge_prop     (nadred+nasupl                  ), stat = istat)
       if (istat==0) allocate (dredgepar%dump_prop       (nadump                         ), stat = istat)
       if (istat/=0) then
          errmsg = 'ERROR RdDredge: memory alloc error'
          call write_error(errmsg, unit=mdia)
          error = .true.
          return
       endif
       !
       ! update local pointers
       !
       link_def          => dredgepar%link_def
       ndredged          => dredgepar%ndredged
       nploughed         => dredgepar%nploughed
       !                          
       link_percentage   => dredgepar%link_percentage
       link_distance     => dredgepar%link_distance
       link_sum          => dredgepar%link_sum
       dzdred            => dredgepar%dzdred
       voldred           => dredgepar%voldred
       totvoldred        => dredgepar%totvoldred
       totvoldredfrac    => dredgepar%totvoldredfrac
       globalareadred    => dredgepar%globalareadred
       voldune           => dredgepar%voldune
       percsupl          => dredgepar%percsupl
       totvoldump        => dredgepar%totvoldump
       totvoldumpfrac    => dredgepar%totvoldumpfrac
       localareadump     => dredgepar%localareadump
       globalareadump    => dredgepar%globalareadump
       globaldumpcap     => dredgepar%globaldumpcap
       voldump           => dredgepar%voldump
       !                          
       dredge_areas      => dredgepar%dredge_areas
       dump_areas        => dredgepar%dump_areas
       !                          
       dredge_prop       => dredgepar%dredge_prop
       dump_prop         => dredgepar%dump_prop
       !
       ! Allocate arrays used for detecting dredge, dump points
       !
                     allocate (imask(1:ndx), stat = istat)
       !
       if (istat==0) allocate (ipdr(nadred), stat = istat)
       if (istat==0) allocate (ipdu(nadump), stat = istat)
       if (istat==0) allocate (npdr(nadred), stat = istat)
       if (istat==0) allocate (npdu(nadump), stat = istat)
       !
       if (istat==0) allocate (xdr(totnpdr), stat = istat)
       if (istat==0) allocate (xdu(totnpdu), stat = istat)
       if (istat==0) allocate (ydr(totnpdr), stat = istat)
       if (istat==0) allocate (ydu(totnpdu), stat = istat)
       if (istat/=0) then
          errmsg = 'ERROR RdDredge: memory alloc error'
          call write_error(trim(errmsg), unit=mdia)
          error = .true.
          return
       endif
       !
       ! necessary initializations
       !
       link_def        = 0
       ndredged        = 0.0_fp
       nploughed       = 0.0_fp
       !
       link_percentage = 0.0_fp
       link_distance   = 0.0_fp
       link_sum        = 0.0_fp
       dzdred          = 0.0_fp
       voldred         = 0.0_fp
       totvoldred      = 0.0_fp
       totvoldredfrac  = 0.0_fp
       globalareadred  = 0.0_fp
       voldune         = 0.0_fp
       percsupl        = 0.0_fp
       totvoldump      = 0.0_fp
       totvoldumpfrac  = 0.0_fp
       localareadump   = 0.0_fp
       globalareadump  = 0.0_fp
       globaldumpcap   = 0.0_fp
       voldump         = 0.0_fp
       !
       dredge_areas    = ' '
       dump_areas      = ' '
       !
       ipdr            = -999
       ipdu            = -999
       npdr            = -999
       npdu            = -999
       !
       call mess(LEVEL_INFO, '  Number of dredging areas    : ', nadred)
       call mess(LEVEL_INFO, '  Number of dump areas        : ', nadump)
       call mess(LEVEL_INFO, '  Number of nourishment areas : ', nasupl)
       !
       ! Finally the input can be read
       !
       cntdred = 0
       cntdump = 0
       cntsupl = 0
       cntssrc = 0
       cntlink = 0
       icdr    = 1
       icdu    = 1
       !
       do iter = 1, 2
          if ( associated(dad_ptr%child_nodes) ) then
             do i = 1, size(dad_ptr%child_nodes)
                link_ptr => dad_ptr%child_nodes(i)%node_ptr
                dredgetype = tree_get_name( link_ptr )
                !
                ! Distinguish the cases:
                ! - 'dredge'
                ! - 'sandmining'
                ! to obtain the associated parameter values
                !
                ! sandmining cases should be processed first because in case of
                ! overlapping sandmining and dredging areas, continuous sandmining
                ! activities should prevail over conditional dredging.
                !
                if ((dredgetype == 'dredge') .or. (dredgetype == 'nourishment')) then
                   if (iter == 1) cycle
                else
                   if (iter /= 1) cycle
                endif
                !
                select case ( dredgetype )
                case ('dredge', 'sandmining')
                   !
                   ! Dredge or sandmining area specification found
                   !
                   cntdred = cntdred + 1
                   cntssrc = cntssrc + 1
                   pdredge => dredge_prop(cntssrc)
                   !
                   ! Initialize
                   !
                   pdredge%idx_type      = cntdred
                   pdredge%ichkloc       = def_chkloc
                   pdredge%paractive     = def_active
                   pdredge%depthdef      = def_depthdef
                   pdredge%dredge_depth  = def_dredge_depth
                   pdredge%maxvolrate    = def_maxvolrate
                   pdredge%clearance     = def_clearance
                   pdredge%stilldredging = .false.
                   pdredge%dredgewhendry = def_dredgewhendry
                   pdredge%dumplimited   = .false.
                   pdredge%in1domain     = .false.
                   pdredge%if_morfac_0   = def_if_morfac_0
                   pdredge%obey_cmp      = def_obey_cmp
                   pdredge%triggertype   = def_drtrigger
                   pdredge%dredgedistr   = def_dredgedistr
                   pdredge%dumpdistr     = def_dr2dudistr
                   pdredge%totalvolsupl  = 0.0_fp
                   pdredge%outletlink    = 0
                   pdredge%npnt          = 0
                   pdredge%use_dunes     = def_use_dunes
                   pdredge%alpha_dh      = def_alpha_dh
                   pdredge%plough_effic  = def_plough_effic
                   nullify(pdredge%nm)
                   nullify(pdredge%inm)
                   nullify(pdredge%area)
                   nullify(pdredge%hdune)
                   nullify(pdredge%dz_dredge)
                   nullify(pdredge%reflevel)
                   nullify(pdredge%dunetoplevel)
                   nullify(pdredge%triggerlevel)
                   nullify(pdredge%bedlevel)
                   nullify(pdredge%troughlevel)
                   nullify(pdredge%sedimentdepth)
                   nullify(pdredge%sortvar)
                   nullify(pdredge%triggered)
                   !
                   ! Set dredge area name
                   !
                   dredge_areas(cntssrc) = ''
                   call prop_get_string(link_ptr, '*', 'Name', dredge_areas(cntssrc))
                   pdredge%name = dredge_areas(cntssrc)
   
                   call mess(LEVEL_INFO,'Dredge definition number      : ', cntdred)
                   call mess(LEVEL_INFO,'Dredge area                   : ' // trim(dredge_areas(cntssrc)))
                   !
                   ! Read dredging parameters
                   !
                   tmp_active = 0
                   if (validtable(tseriesfile)) then
                      call gettable(tseriesfile  , pdredge%name , 'active'     , &
                                      & tmp_active(1), tmp_active(2), tmp_active(3), 0, errmsg)
                      if (tmp_active(3) == 0) then
                         tmp_active = def_active
                      elseif (tmp_active(3) > 1) then
                         write(errmsg,'(i3,3a)') tmp_active(3), &
                                                & ' active parameters specified in file "', &
                                                & trim(getfilename(tseriesfile)), '" instead of 1.'
                         call write_error(trim(errmsg), unit=mdia)
                         error = .true.
                         return
                      else
                         call checktable(tseriesfile   , tmp_active(1), &
                                           & tmp_active(2) , tmp_active(3), &
                                           & CHKTAB_LOGICAL, errmsg          )
                         tmp_active(4) = 1
                      endif
                      pdredge%paractive = tmp_active
                   endif
                   !
                   call prop_get(link_ptr, '*', 'DredgeDepth', pdredge%dredge_depth)
                   call prop_get(link_ptr, '*', 'Volume'     , pdredge%maxvolrate)
                   call prop_get(link_ptr, '*', 'MaxVolRate' , pdredge%maxvolrate)
                   !
                   if (comparereal(pdredge%dredge_depth,1.0e10_fp) == 0 .and. &
                     & comparereal(pdredge%maxvolrate  ,-999.0_fp) == 0) then
                      if (parname == 'sandmining') then
                         call write_error('Unable to read sand mining area "'// &
                                   & trim(dredge_areas(cntssrc))//'"', unit=mdia)
                      else
                         call write_error('Unable to read dredge depth of area "'// &
                                   & trim(dredge_areas(cntssrc))//'"', unit=mdia)
                      endif
                      error = .true.
                      return
                   elseif (comparereal(pdredge%dredge_depth,1.0e10_fp) == 0) then
                      !
                      ! Fixed rate dredging, previously known as sandmining
                      !
                      pdredge%itype       = DREDGETYPE_SANDMINING
                      pdredge%dredgedistr = 1
                      write(message, '(a,f10.2,a)') '  Dredging rate               : ',pdredge%maxvolrate,' m^3/y (including pores)'
                      call mess(LEVEL_INFO, message)
                   else
                      !
                      ! Dredging to specified depth (with optional maximum rate)
                      !
                      pdredge%itype = DREDGETYPE_DREDGING
                      write(message,'(a,f10.2,a)') '  Dredge depth                : ', pdredge%dredge_depth,' m'
                      call mess(LEVEL_INFO, message)
                      if (comparereal(pdredge%maxvolrate  ,-999.0_fp) /= 0) then
                         write(message, '(a,f10.2,a)') '  Maximum dredging rate       : ', pdredge%maxvolrate,' m^3/y (including pores)'
                         call mess(LEVEL_INFO, message)
                      end if
                   endif
   
                   if (.not. (comparereal(pdredge%maxvolrate,-999.0_fp) == 0)) then
                       pdredge%maxvolrate = pdredge%maxvolrate/yearsec_hp
                   endif
                   !
                   call prop_get(link_ptr, '*', 'DepthDef', pdredge%depthdef)
                   if (pdredge%depthdef < 1 .or. pdredge%depthdef > DEPTHDEF_MAX) then
                      errmsg = 'Invalid depth definition for dredge area "'// &
                                & trim(dredge_areas(cntssrc))//'"'
                      call write_error(trim(errmsg), unit=mdia)
                      error = .true.
                      return
                   endif
                   call prop_get_integer(link_ptr, '*', 'InPolygon', pdredge%ichkloc)
                   if (pdredge%ichkloc < 1 .or. pdredge%ichkloc > 3) then
                      errmsg = 'Invalid in polygon check for dredge area "'// &
                                & trim(dredge_areas(cntssrc))//'"'
                      call write_error(trim(errmsg), unit=mdia)
                      error = .true.
                      return
                   endif
                   call prop_get_integer(link_ptr, '*', 'DredgeDistr', pdredge%dredgedistr)
                   if (pdredge%dredgedistr < 1 .or. pdredge%dredgedistr > DREDGEDISTR_MAX) then
                      errmsg =  'Invalid dredge distribution for dredge area "'// &
                                & trim(dredge_areas(cntssrc))//'"'
                      call write_error(trim(errmsg), unit=mdia)
                      error = .true.
                      return
                   endif
                   if (pdredge%paractive(1) /= -999) then
                      call mess(LEVEL_INFO,'  Dredging active             : during intervals')
                   else
                      call mess(LEVEL_INFO,   '  Dredging active             : always')
                   endif
                   !
                   select case (pdredge%dredgedistr)
                   case (DREDGEDISTR_UNIFORM)
                      stringval = 'uniform'
                   case (DREDGEDISTR_HIGHEST)
                      stringval = 'highest'
                   case (DREDGEDISTR_PROPORTIONAL)
                      stringval = 'proportional'
                   case (DREDGEDISTR_HIGHFIRST)
                      stringval = 'high first'
                   case (DREDGEDISTR_LOWFIRST)
                      stringval = 'low first'
                   case (DREDGEDISTR_SHALLOWEST)
                      stringval = 'shallowest'
                   case (DREDGEDISTR_SHALLOWFIRST)
                      stringval = 'shallow first'
                   case (DREDGEDISTR_DEEPFIRST)
                      stringval = 'deep first'
                   end select
                   write(message, '(a, i0,a,a,a)') '  Dredge distribution         : ', pdredge%dredgedistr,' (',trim(stringval),')'
                   call mess(LEVEL_INFO, message)
                   !
                   select case (pdredge%depthdef)
                   case (DEPTHDEF_REFPLANE)
                      stringval = 'reference plane'
                   case (DEPTHDEF_WATERLVL)
                      stringval = 'water level'
                   case (DEPTHDEF_MAXREFWL)
                      stringval = 'maximum(reference plane,water level)'
                   case (DEPTHDEF_MINREFWL)
                      stringval = 'minimum(reference plane,water level)'
                   end select
                   write(message, '(a, i0,a,a,a)') '  Depth definition            : ',pdredge%depthdef,' (relative to '//trim(stringval)//')'
                   call mess(LEVEL_INFO, message)
                   !
                   call prop_get(link_ptr, '*', 'Clearance'    , pdredge%clearance)
                   call prop_get(link_ptr, '*', 'DredgeWhenDry', pdredge%dredgewhendry)
                   call prop_get(link_ptr, '*', 'DumpLimited'  , pdredge%dumplimited)
                   if (pdredge%maxvolrate < 0.0_fp) then
                      !
                      ! DredgeWhileMorfac0 can only be .true. if dredging is instantaneous.
                      !
                      call prop_get(link_ptr, '*', 'DredgeWhileMorfac0', pdredge%if_morfac_0)
                   else
                      pdredge%if_morfac_0 = .false.
                   endif
                   call prop_get(link_ptr, '*', 'ObeyCmp'    , pdredge%obey_cmp)
                   triggerall = .false.
                   call prop_get(link_ptr, '*', 'TriggerAll' , triggerall)
                   if (triggerall) then
                      ! TriggerAll = #YES# was explicitly specified
                      pdredge%triggertype = DREDGETRIG_ALLBYONE
                   else
                      ! triggerall may be false because it was specified or just because we set the default to false.
                      ! we need to distinguish, so let's change the default setting
                      triggerall = .true.
                      call prop_get(link_ptr, '*', 'TriggerAll' , triggerall)
                      if (.not.triggerall) then
                         ! now we know that TriggerAll = #NO# was explicitly specified
                         pdredge%triggertype = DREDGETRIG_POINTBYPOINT
                      endif
                   endif
                   call prop_get(link_ptr, '*', 'DredgeTrigger', pdredge%triggertype)
                   call prop_get(link_ptr, '*', 'UseDunes'     , pdredge%use_dunes)
                   if (pdredge%use_dunes .and. .not. lfbedfrm) then
                      errmsg = 'ERROR rddredge - UseDunes: Dunes can only be used when modelled.'
                      call write_error(trim(errmsg), unit=mdia)
                      error = .true.
                      return
                   endif
                   if (pdredge%use_dunes) then
                      call prop_get(link_ptr, '*', 'AlphaDuneHeight', pdredge%alpha_dh)
                      if ((pdredge%alpha_dh > 0.5_fp) .or. (pdredge%alpha_dh < 0.0_fp)) then
                         errmsg = 'ERROR rddgredge: AlphaDuneHeight should be a real number between 0.0 and 0.5'
                         call write_error(trim(errmsg), unit=mdia)
                         error = .true.
                         return
                      endif
                      call prop_get(link_ptr, '*', 'PloughEfficiency', pdredge%plough_effic)
                      if ((pdredge%plough_effic > 1.0_fp) .or. (pdredge%plough_effic < 0.0_fp)) then
                         errmsg = 'ERROR rddredge: PloughEfficiency should be a real number between 0.0 and 1.0'
                         call write_error(trim(errmsg), unit=mdia)
                         error = .true.
                         return
                      endif
                   endif
                   !
                   ! Read the coordinates of the corresponding polygon
                   !
                   call tree_get_node_by_name(pol_ptr, dredge_areas(cntssrc), dredge_area_ptr )
                   areatp = 'dredge'
                   call read_polygon_data(dredge_area_ptr, icdr, ipdr(cntdred), npdr(cntdred), &
                                        & xdr, ydr, areatp, cntdred)
                   !
                   ! Each dredge area may distribute to several dump areas
                   ! The sum of all distribution percentages must be 100.0 for each dredge area
                   ! The dumpid's, added while obtaining the dimensions, are used to uniquely
                   ! identify each dump area (they may occur more than once).
                   !
                   ilink     = 0
                   cntsedidx = 0
                   if ( associated(link_ptr%child_nodes) ) then
                      do j = 1, size(link_ptr%child_nodes)
                         !
                         ! Does link_ptr contain one or more children with name 'Dump'?
                         !
                         node_ptr => link_ptr%child_nodes(j)%node_ptr
                         parname = tree_get_name( node_ptr )
                         if (parname == 'dump') then
                            cntlink = cntlink + 1
                            if (ilink == 0) ilink = cntlink
                            !
                            ! Get dump name
                            !
                            call tree_get_data_string(node_ptr, parname, succes)
                            !
                            ! Get corresponding polygon
                            !
                            call tree_get_node_by_name( pol_ptr, parname, dump_area_ptr )
                            !
                            ! Get the polygon's dumpid
                            !
                            cntdump = 0
                            call prop_get_integer(dump_area_ptr, '*', 'dumpid', cntdump)
                            if (cntdump < 1) then
                               errmsg = 'ERROR rddredge: Invalid dump ID: '//trim(parname)
                               call write_error(trim(errmsg), unit=mdia)
                               error = .true.
                               return
                            endif
                            link_def(cntlink,1) = cntssrc
                            link_def(cntlink,2) = cntdump
                            !
                            if (dump_areas(cntdump) == parname) then
                               call mess(LEVEL_INFO, 'Dump area ', trim(dump_areas(cntdump)), ' has already been read.')
                               !
                               ! Do not read polygon points again
                               !
                               cycle
                            endif
                            !
                            dump_areas(cntdump) = parname
                            !
                            ! Read the coordinates of the corresponding polygon
                            !
                            call tree_get_node_by_name(pol_ptr, dump_areas(cntdump), dump_area_ptr )
                            areatp = 'dump'
                            call read_polygon_data(dump_area_ptr, icdu, ipdu(cntdump), npdu(cntdump), &
                                                 & xdu, ydu, areatp, cntdump)
                         elseif (parname == 'percentage') then
                            if (ilink == 0) then
                               errmsg = 'ERROR rddredge: Unexpected percentage encountered'
                               call write_error(trim(errmsg), unit=mdia)
                               error = .true.
                               return
                            endif
                            sedperc = 0.0_fp
                            call prop_get(node_ptr, '*', 'Percentage', sedperc)
                            if (cntsedidx == 0) then
                               do lsed = 1, lsedtot
                                  link_percentage(cntlink,lsed) = sedperc
                               enddo
                            else
                               link_percentage(cntlink,cntsedidx) = sedperc
                               cntsedidx = 0
                            endif
                         elseif ( parname == 'sediment') then
                            sedname = ''
                            call prop_get(node_ptr, '*', 'Sediment' , sedname)
                            sfound = .false.
                            do j2 = 1, lsedtot
                               if ( stringsequalinsens(namsed(j2), sedname) ) then
                                  cntsedidx = j2
                                  sfound    = .true.
                               endif
                            enddo
                            if (.not. sfound) then
                               errmsg = 'ERROR rddredge: Unknown sediment fraction "'//trim(sedname)//'"'
                               call write_error(trim(errmsg), unit=mdia)
                               error = .true.
                               return
                            endif
                         endif
                      enddo
                   endif
               case ('nourishment')
                   !
                   ! Nourishment specification found
                   !
                   cntssrc = cntssrc + 1
                   cntsupl = cntsupl + 1
                   pdredge => dredge_prop(cntssrc)
                   !
                   cntsedidx = 0
                   dredge_areas(cntssrc) = ''
                   call prop_get_string(link_ptr, '*', 'Name', dredge_areas(cntssrc))
                   if (dredge_areas(cntssrc) == ' ') then
                      write(stringval,'(a,i0)') 'nourishment ', cntsupl
                      dredge_areas(cntssrc) = stringval
                   endif
                   pdredge%name = dredge_areas(cntssrc)
                   !
                   ! Initialize
                   !
                   pdredge%idx_type      = cntsupl
                   pdredge%paractive     = def_active
                   pdredge%dredge_depth  = -999.0_fp
                   pdredge%clearance     = -999.0_fp
                   pdredge%stilldredging = .false.
                   pdredge%dredgewhendry = .false.
                   pdredge%dumplimited   = .false.
                   pdredge%in1domain     = .false.
                   pdredge%if_morfac_0   = .false.
                   pdredge%obey_cmp      = .true.
                   pdredge%triggertype   = DREDGETRIG_POINTBYPOINT
                   pdredge%depthdef      = DEPTHDEF_REFPLANE
                   pdredge%dredgedistr   = 0
                   pdredge%dumpdistr     = def_dr2dudistr
                   pdredge%outletlink    = 0
                   pdredge%npnt          = 0
                   pdredge%ichkloc       = 0
                   nullify(pdredge%nm)
                   nullify(pdredge%inm)
                   nullify(pdredge%area)
                   nullify(pdredge%hdune)
                   nullify(pdredge%dz_dredge)
                   nullify(pdredge%reflevel)
                   nullify(pdredge%dunetoplevel)
                   nullify(pdredge%triggerlevel)
                   nullify(pdredge%bedlevel)
                   nullify(pdredge%troughlevel)
                   nullify(pdredge%sedimentdepth)
                   nullify(pdredge%sortvar)
                   nullify(pdredge%triggered)
                   pdredge%maxvolrate   = def_maxvolrate
                   pdredge%totalvolsupl = -999.0_fp
                   !
                   tmp_active = 0
                   if (validtable(tseriesfile)) then
                      call gettable(tseriesfile  , pdredge%name , 'active'     , &
                                      & tmp_active(1), tmp_active(2), tmp_active(3), 0, errmsg)
                      if (tmp_active(3) == 0) then
                         tmp_active = def_active
                      elseif (tmp_active(3) > 1) then
                         write(message,'(i3,a,a,a)') tmp_active(3), &
                                                & ' active parameters specified in file "', &
                                                & trim(getfilename(tseriesfile)), '" instead of 1.'
                         call write_error(trim(message), unit=mdia)
                         error = .true.
                         return
                      else
                         call checktable(tseriesfile   , tmp_active(1), &
                                           & tmp_active(2) , tmp_active(3), &
                                           & CHKTAB_LOGICAL, errmsg          )
                         tmp_active(4) = 1
                      endif
                      pdredge%paractive = tmp_active
                   endif
                   !
                   call mess(LEVEL_INFO, 'Nourishment definition number : ', cntsupl)
                   call mess(LEVEL_INFO, '  Name                        : ', dredge_areas(cntssrc))
                   pdredge => dredge_prop(cntssrc)
                   call prop_get(link_ptr, '*', 'Volume' , pdredge%totalvolsupl)
                   call mess(LEVEL_INFO, '  Total nourishment volume    : ', pdredge%totalvolsupl)
                   call prop_get(link_ptr, '*', 'MaxVolRate' , pdredge%maxvolrate)
                   write(message,'(a,f10.2,a)') '  Nourishment rate            : ',pdredge%maxvolrate,' m^3/y (including pores)'
                   call mess(LEVEL_INFO, message)
                   if (pdredge%paractive(1) /= -999) then
                      call mess(LEVEL_INFO, '  Nourishment active          : during intervals')
                   else
                      call mess(LEVEL_INFO, '  Nourishment active          : always')
                   endif
                   !
                   pdredge%itype = DREDGETYPE_NOURISHMENT
                   if (.not. (comparereal(pdredge%maxvolrate  ,-999.0_fp) == 0)) then
                       pdredge%maxvolrate = pdredge%maxvolrate/yearsec_hp
                   endif
                   !
                   ilink = 0
                   if ( associated(link_ptr%child_nodes) ) then
                      do j = 1, size(link_ptr%child_nodes)
                         node_ptr => link_ptr%child_nodes(j)%node_ptr
                         parname = tree_get_name( node_ptr )
                         if ( parname == 'sediment') then
                            sedname = ''
                            call prop_get(node_ptr, '*', 'Sediment', sedname)
                            sfound = .false.
                            do j2 = 1, lsedtot
                               if ( stringsequalinsens(namsed(j2), sedname) ) then
                                  cntsedidx = j2
                                  sfound    = .true.
                               endif
                            enddo
                            if (.not. sfound) then
                               errmsg =  'ERROR rddredge: Unknown sediment fraction "'//trim(sedname)//'"'
                               call write_error(trim(errmsg), unit=mdia)
                               error = .true.
                               return
                            endif
                         elseif (parname == 'sedpercentage') then 
                            sedperc = 0.0_fp
                            call prop_get(node_ptr,'*','SedPercentage', sedperc)
                            if (cntsedidx == 0) then
                               errmsg =  'ERROR rddredge: SedPercentage without preceding Sediment keyword'
                               call write_error(trim(errmsg), unit=mdia)
                               error = .true.
                               return
                            else
                               j2 = cntsedidx
                               write(message, '(a,a,a,f2.2)' ) '  Percentage of ', trim(namsed(j2)), ': ', sedperc
                               call mess(LEVEL_INFO, message)
                               percsupl(cntsupl,j2) = sedperc
                               cntsedidx            = 0
                            endif
                         elseif ( parname == 'dump') then
                            cntlink = cntlink + 1
                            if (ilink == 0) ilink = cntlink
                            !
                            ! Get dump name
                            !
                            call tree_get_data_string(node_ptr, parname, succes)
                            !
                            ! Get corresponding polygon
                            !
                            call tree_get_node_by_name( pol_ptr, parname, dump_area_ptr )
                            !
                            ! Get the polygon's dumpid
                            !
                            cntdump = 0
                            call prop_get_integer(dump_area_ptr, '*', 'dumpid', cntdump)
                            if (cntdump < 1) then
                               errmsg =  'ERROR rddredge: Invalid dump ID'
                               call write_error(trim(errmsg), unit=mdia)
                               error = .true.
                               return    
                            endif
                            link_def(cntlink,1) = cntssrc
                            link_def(cntlink,2) = cntdump
                            !
                            if (dump_areas(cntdump) == parname) then
                               call mess(LEVEL_INFO, 'Dump area ', trim(dump_areas(cntdump)), ' has already been read.')
                               !
                               ! Do not read polygon points again
                               !
                               cycle
                            endif
                            !
                            dump_areas(cntdump) = parname
                            !
                            ! Read the coordinates of the corresponding polygon
                            !
                            call tree_get_node_by_name(pol_ptr, dump_areas(cntdump), dump_area_ptr )
                            areatp = 'dump'
                            call read_polygon_data(dump_area_ptr, icdu, ipdu(cntdump), npdu(cntdump), &
                                                 & xdu, ydu, areatp, cntdump)
                         elseif ( parname == 'percentage') then
                            if (ilink == 0) then
                               errmsg =  'ERROR rddredge: Unexpected percentage encountered'
                               call write_error(trim(errmsg), unit=mdia)
                               error = .true.
                               return
                            endif
                            call prop_get(node_ptr, '*', 'Percentage', link_percentage(cntlink,1))
                            do lsed = 2, lsedtot
                               link_percentage(cntlink,lsed) = link_percentage(cntlink,1)
                            enddo
                         endif 
                      enddo
                   endif
                   !
                   call mess(LEVEL_INFO, '  Sediment composition')
                   if (lsedtot > 1) then
                      sumperc = 0.0_fp
                      do lsed = 1, lsedtot
                         if (percsupl(cntsupl,lsed) > 0.0_fp) then
                            write(message, '(a,f2.2,2a)') '    ', percsupl(cntsupl,lsed),'% of sed. fraction  : ', trim(namsed(lsed))
                            call mess(LEVEL_INFO, message)
                         endif
                         sumperc = sumperc + percsupl(cntsupl,lsed)
                      enddo
                      if (comparereal(100.0_fp,sumperc) /= 0) then
                         errmsg = 'Sum of sediment fractions is not 100.0 for nourishment "'//dredge_areas(cntssrc)//'"'
                         call write_error(trim(errmsg), unit=mdia)
                         error = .true.
                         return
                      endif
                   else
                      percsupl(cntsupl,1) = 100.0_fp
                   endif
                case default
                   !
                   ! Ignore any other child (like 'dump')
                   !
                end select
                !
                ! Now verify the distribution of sediment from the dredging area,
                ! sandmining area, or nourishment to the dumping areas.
                !
                select case ( dredgetype )
                case ('dredge', 'sandmining', 'nourishment')
                   call prop_get(link_ptr, '*', 'DumpDistr'    , pdredge%dumpdistr) ! old keyword still supported
                   call prop_get(link_ptr, '*', 'DistrOverDump', pdredge%dumpdistr)
                   if (pdredge%dumpdistr<1 .or. pdredge%dumpdistr>DR2DUDISTR_MAX) then
                      errmsg =  'Invalid dump distribution for '//trim(dredgetype)//' area "'// &
                                & trim(dredge_areas(cntssrc))//'"'
                      call write_error(trim(errmsg), unit=mdia)
                      error = .true.
                      return
                   endif
                   !
                   if (ilink == 0) ilink = cntlink + 1
                   sumperc = 0.0_fp
                   do lsed = 1, lsedtot
                      do j = ilink, cntlink
                         sumperc = sumperc + link_percentage(j,lsed)
                      enddo
                   enddo
                   if (comparereal(sumperc,0.0_fp) == 0) then
                      if (ilink<=cntlink .and. pdredge%dumpdistr == DR2DUDISTR_PERCENTAGE) &
                        & pdredge%dumpdistr = DR2DUDISTR_SEQUENTIAL
                   else
                      if (pdredge%dumpdistr /= DR2DUDISTR_PERCENTAGE) then
                         errmsg =  'Specified percentages conflict with specified dump'// &
                                   & ' distribution for '//trim(dredgetype)//' area "'// &
                                   & trim(dredge_areas(cntssrc))//'"'
                         call write_error(trim(errmsg), unit=mdia)
                         error = .true.
                         return
                      endif
                   endif
                   !
                   select case (pdredge%dumpdistr)
                   case (DR2DUDISTR_PERCENTAGE)
                      !
                      ! Verify that percentages sum to 100%
                      !
                      stringval = 'percentage'
                      write(message,'(a,i0,3a)') '  Dump distribution           : ', pdredge%dumpdistr,' ('//trim(stringval)//')'
                      call mess(LEVEL_INFO, message)
                      do lsed = 1, lsedtot
                         if (lsedtot > 1) then
                            call mess(LEVEL_INFO, '  Sediment fraction           : ', trim(namsed(lsed)))
                         endif
                         sumperc = 0.0_fp
                         do j = ilink, cntlink
                            if (link_percentage(j,lsed) > 0.0_fp) then
                               write(message,'(a,f10.2,2a)') '    Dump ',link_percentage(j,lsed), '% at           : ', trim(dump_areas(link_def(j,2)))
                               call mess(LEVEL_INFO, message)
                            endif
                            sumperc = sumperc + link_percentage(j,lsed)
                         enddo
                         if (comparereal(100.0_fp,sumperc) /= 0 .and. pdredge%itype /= DREDGETYPE_SANDMINING) then
                            errmsg =  'Sum of dump % of '//trim(dredgetype)//' area "'// &
                                                & trim(dredge_areas(cntssrc))//'" is not equal to 100.0 '
                            call write_error(trim(errmsg), unit=mdia)
                            error = .true.
                            return
                         endif
                      enddo
                   case (DR2DUDISTR_SEQUENTIAL, DR2DUDISTR_PROPORTIONAL)
                      !
                      ! Verify that no percentages have been specified
                      !
                      if (pdredge%dumpdistr == DR2DUDISTR_SEQUENTIAL) then
                         stringval = 'sequential'
                      else !if (pdredge%dumpdistr == DR2DUDISTR_PROPORTIONAL) then
                         stringval = 'proportional'
                      endif
                      write(message,'(a,i0,3a)') '  Dump distribution           : ', pdredge%dumpdistr,' ('//trim(stringval)//')'
                      call mess(LEVEL_INFO, message)
                      !
                      do j = ilink, cntlink
                         call mess(LEVEL_INFO, '    Dump at                   : ', trim(dump_areas(link_def(j,2))))
                      enddo
                      !
                      ! To be checked futher down: unique relation and capacity limitation
                      !
                   end select
                end select
             enddo
          endif
       enddo
       !
       do i = 1, nadump
          pdump => dump_prop(i)
          !
          ! Initialize
          !
          pdump%depthdef     = def_depthdef
          pdump%mindumpdepth = def_mindumpdepth
          pdump%dumpcapaflag = comparereal(pdump%mindumpdepth,-999.0_fp) /= 0
          pdump%dumpdistr    = def_dumpdistr
          pdump%dumpwhendry  = def_dumpwhendry
          pdump%in1domain    = .false.
          pdump%ichkloc      = def_chkloc
          pdump%use_dunes    = def_use_dunes
          pdump%npnt         = 0
          nullify(pdump%nm)
          nullify(pdump%inm)
          nullify(pdump%reflevel)
          nullify(pdump%area)
          nullify(pdump%hdune)
          nullify(pdump%bedlevel)
          nullify(pdump%dz_dump)
          nullify(pdump%sortvar)
       enddo
       !
       if ( associated(dad_ptr%child_nodes) ) then
          do i = 1, size(dad_ptr%child_nodes)
             link_ptr => dad_ptr%child_nodes(i)%node_ptr
             dredgetype = tree_get_name( link_ptr )
             !
             ! Distinguish the cases:
             ! - 'dump'
             ! to obtain the associated parameter values
             !
             select case ( dredgetype )
             case ('dump')
                !
                ! Dump area specification found
                !
                ! Get dredge area name
                !
                parname = ''
                call prop_get_string(link_ptr, '*', 'Name', parname)
                do ia = 1, nadump
                   if (parname == dump_areas(ia)) exit
                enddo
                !
                if (ia > nadump) then
                   errmsg =  'Skipping data block for unknown dump area "'// &
                             & trim(parname)//'"'
                   call write_error(errmsg, unit=mdia)
                   cycle
                endif
                pdump => dump_prop(ia)
                !
                ! Read dumping parameters
                !
                call prop_get(link_ptr, '*', 'DepthDef', pdump%depthdef)
                if (pdump%depthdef<1 .or. pdump%depthdef>DEPTHDEF_MAX) then
                   errmsg =  'ERROR rddredge: Invalid depth definition for dump area "'// &
                             & trim(dump_areas(ia))//'"'
                   call write_error(trim(errmsg), unit=mdia)
                   error = .true.
                   return
                endif
                call prop_get_integer(link_ptr, '*', 'DumpDistr', pdump%dumpdistr)
                if (pdump%dumpdistr<1 .or. pdump%dumpdistr>DUMPDISTR_MAX) then
                   errmsg =  'ERROR rddredge: Invalid dump distribution for dump area "'// &
                             & trim(dump_areas(ia))//'"'
                   call write_error(trim(errmsg), unit=mdia)
                   error = .true.
                   return
                endif
                call prop_get_integer(link_ptr, '*', 'InPolygon', pdump%ichkloc)
                if (pdump%ichkloc<1 .or. pdump%ichkloc>3) then
                   errmsg = 'ERROR rddredge: Invalid in polygon check for dump area "'// &
                             & trim(dump_areas(ia))//'"'
                   call write_error(trim(errmsg), unit=mdia)
                   error = .true.
                   return
                endif
                !
                call prop_get_logical(link_ptr, '*', 'DumpWhenDry'  , pdump%dumpwhendry)
                call prop_get(link_ptr, '*', 'MinimumDumpDepth', pdump%mindumpdepth)
                pdump%dumpcapaflag = comparereal(pdump%mindumpdepth,-999.0_fp) /= 0
                !
                call prop_get(link_ptr, '*', 'UseDunes'     , pdump%use_dunes)
                if (pdump%use_dunes .and. .not. lfbedfrm) then
                   errmsg = 'ERROR rddredge: UseDunes: Dunes can only be used when modelled.'
                   call write_error(trim(errmsg), unit=mdia)
                   error = .true.
                   return
                endif
             case default
                !
                ! Ignore any other child (like 'dredge')
                !
             end select
          enddo
          !
          do i = 1, nadump
             pdump => dump_prop(i)
             call mess(LEVEL_INFO,      'Dump definition number        : ', i)
             call mess(LEVEL_INFO,         '  Dump area                   : ' // trim(dump_areas(i)))
             pdump%name = dump_areas(i)
             select case (pdump%dumpdistr)
             case (DUMPDISTR_UNIFORM)
                stringval = 'uniform'
             case (DUMPDISTR_LOWEST)
                stringval = 'lowest'
             case (DUMPDISTR_DEEPEST)
                stringval = 'deepest'
             case (DUMPDISTR_PROPORTIONAL)
                stringval = 'proportional'
             end select
             write(message,'(a,i0,3a)') '  Dump distribution           : ',pdump%dumpdistr,' ('//trim(stringval)//')'
             call mess(LEVEL_INFO,message)
             if (pdump%dumpcapaflag) then
                call mess(LEVEL_INFO, '  MinimumDumpDepth           : ',pdump%mindumpdepth)
             endif
             !
             select case (pdump%depthdef)
             case (DEPTHDEF_REFPLANE)
                stringval = 'reference plane'
             case (DEPTHDEF_WATERLVL)
                stringval = 'water level'
             case (DEPTHDEF_MAXREFWL)
                stringval = 'maximum(reference plane,water level)'
             case (DEPTHDEF_MINREFWL)
                stringval = 'minimum(reference plane,water level)'
             end select
             write(message,'(a,i0,3a)') '  Depth definition            : ',pdump%depthdef,' (relative to '//trim(stringval)//')'
             call mess(LEVEL_INFO, message)
          enddo
       endif
       !
       do i = 1, nadred+nasupl
          pdredge => dredge_prop(i)
          !
          if (.not. pdredge%dumplimited) cycle
          !
          ! This is a dredging area for which the dredging rate is limited by the
          ! dumping capacity. Now all links and dump areas are known, we can check
          ! whether the dump areas are uniquely associated with this dredging area.
          !
          do j = 1, nalink
             if (link_def(j,1) /= i) cycle
             !
             ic     = link_def(j,2)
             sfound = .false.
             do j2 = 1, nalink
                if (link_def(j2,2) /= ic .or. j2 == j) cycle
                sfound = .true.
             enddo
             !
             if (sfound) then
                write(message,'(5a)') 'Dump area "', trim(dump_areas(ic)), &
                                    & '" not uniquely associated with sediment source "', &
                                    & trim(dredge_areas(i)), '".'
                call write_error(trim(message), unit=mdia)
                error = .true.
                return
             endif
          enddo
       enddo
       !
       noutletlinks = 0
       do i = 1, nadred+nasupl
          pdredge => dredge_prop(i)
          !
          select case (pdredge%dumpdistr)
          case (DR2DUDISTR_PERCENTAGE)
             !
             ! Add an outlet if one of the dump areas can be full.
             !
             do j = 1, nalink
                if (link_def(j,1) /= i) cycle
                !
                ic = link_def(j,2)
                if (dump_prop(ic)%dumpcapaflag) then
                   noutletlinks       = noutletlinks + 1
                   pdredge%outletlink = nalink + noutletlinks
                   exit
                endif
             enddo
             !
             ! or if total percentage is less than 100%
             !
             if (pdredge%outletlink==0) then
                do lsed = 1, lsedtot
                   sumperc = 0.0_fp
                   do j = 1, nalink
                      if (link_def(j,1) /= i) cycle
                      sumperc = sumperc + link_percentage(j,lsed)
                   enddo
                   if (comparereal(100.0_fp,sumperc) /= 0 .and. pdredge%outletlink==0) then
                      noutletlinks       = noutletlinks + 1
                      pdredge%outletlink = nalink + noutletlinks
                      exit
                   endif
                enddo
             endif
          case (DR2DUDISTR_SEQUENTIAL)
             !
             ! Add an outlet if all dump areas can be full.
             !
             lastdumparea = .true.
             do j = nalink, 1, -1
                if (link_def(j,1) /= i) cycle
                !
                ic = link_def(j,2)
                if (lastdumparea) then
                   !
                   ! if the last dump area is unlimited, we don't need an outlet
                   !
                   if (dump_prop(ic)%dumpcapaflag) then
                      noutletlinks       = noutletlinks + 1
                      pdredge%outletlink = nalink + noutletlinks
                   endif
                   lastdumparea = .false.
                else
                   !
                   ! if an earlier dump area is unlimited, give an error
                   !
                   if (.not.dump_prop(ic)%dumpcapaflag) then
                      write(message,'(5a)') 'Dump area "', trim(dump_areas(ic)), &
                                          & '" is not last in sequence of dump areas for "', &
                                          & trim(dredge_areas(i)), '" but has unlimited dumping capacity.'
                      call write_error(trim(message),unit=mdia)
                      error=.true.
                      return
                   endif
                endif
             enddo
          case (DR2DUDISTR_PROPORTIONAL)
             !
             ! All dump areas should be limited, verify this and add one outletlink.
             !
             do j = nalink, 1, -1
                if (link_def(j,1) /= i) cycle
                !
                ic = link_def(j,2)
                if (.not. dump_prop(ic)%dumpcapaflag) then
                   write(message,'(5a)') 'Dump area "', trim(dump_areas(ic)), &
                                       & '" in list of dump areas for "', trim(dredge_areas(i)), &
                                       & '" has unlimited dumping capacity.'
                   call write_error(trim(message),unit=mdia)
                   error=.true.
                   return
                endif
             enddo
             noutletlinks       = noutletlinks + 1
             pdredge%outletlink = nalink + noutletlinks
          end select
       enddo
       !
       do i = 1, nadump
          pdump => dump_prop(i)
          !
          if (pdump%dumpdistr == DUMPDISTR_PROPORTIONAL .and. .not.pdump%dumpcapaflag) then
             write(message,'(3a)') 'Dump distribution proportional for area "', &
                                  & trim(dump_areas(i)), '" requires specification of MinimumDumpDepth.'
             call write_error(trim(message),unit=mdia)
             error=.true.
             return
          endif
       enddo
       !
       call mess(LEVEL_INFO, '*** End of Dredging & dumping input')
       !
       if (noutletlinks > 0) then
          !
          istat = 0
          if (istat == 0) call reallocP(dredgepar%link_percentage, (/nalink+noutletlinks,lsedtot/), fill = 100.0_fp, stat = istat)
          if (istat == 0) call reallocP(dredgepar%link_distance, nalink+noutletlinks, fill = 0.0_fp, stat = istat)
          if (istat == 0) call reallocP(dredgepar%link_sum, (/nalink+noutletlinks,lsedtot/), fill = 0.0_fp, stat = istat)
          if (istat == 0) call reallocP(dredgepar%voldump, (/nadump+1,lsedtot/), fill = 0.0_fp, stat = istat)
          if (istat == 0) call reallocP(dredgepar%totvoldump, nadump+1, fill = 0.0_fp, stat = istat)
          if (istat == 0) call reallocP(dredgepar%localareadump, nadump+1, fill = 0.0_fp, stat = istat)
          if (istat == 0) call reallocP(dredgepar%globalareadump, nadump+1, fill = 0.0_fp, stat = istat)
          if (istat == 0) call reallocP(dredgepar%globaldumpcap, nadump+1, fill = 0.0_fp, stat = istat)
          if (istat == 0) call reallocP(dredgepar%link_def, (/nalink+noutletlinks,2/), fill = 0, stat = istat)
          if (istat == 0) call reallocP(dredgepar%dump_areas, nadump+1, fill = ' ', stat = istat)
          if (istat == 0) then
             allocate(dredgepar%dump_prop(nadump+1), stat=istat)
             if (istat == 0) then
                dredgepar%dump_prop(1:nadump) = dump_prop(1:nadump)
                deallocate(dump_prop, stat=istat)
             endif
          endif
          !
          if (istat /= 0) then
             call write_error('RdDredge: memory realloc error', unit=mdia)
             error = .true.
             return
          endif
          !
          if (istat == 0) call reallocP(ipdu, nadump+1, stat = istat, fill=0)
          if (istat == 0) call reallocP(npdu, nadump+1, stat = istat, fill=0)
          !
          ! update local pointers
          !
          link_percentage   => dredgepar%link_percentage
          link_distance     => dredgepar%link_distance
          link_sum          => dredgepar%link_sum
          voldump           => dredgepar%voldump
          totvoldump        => dredgepar%totvoldump
          localareadump     => dredgepar%localareadump
          globalareadump    => dredgepar%globalareadump
          globaldumpcap     => dredgepar%globaldumpcap
          link_def          => dredgepar%link_def
          dump_areas        => dredgepar%dump_areas
          dredge_prop       => dredgepar%dredge_prop
          dump_prop         => dredgepar%dump_prop
          !
          nalink = nalink + noutletlinks
          nadump = nadump + 1
          !
          do i = 1, nadred+nasupl
             pdredge => dredge_prop(i)
             !
             if (pdredge%outletlink>0) then
                link_def(pdredge%outletlink,1) = i
                link_def(pdredge%outletlink,2) = nadump
             endif
          enddo
          !
          dump_areas(nadump) = 'REMOVED FROM MODEL'
          ! dump_prop
          pdump => dump_prop(nadump)
          !
          ! Initialize
          !
          pdump%name         = dump_areas(nadump)
          pdump%mindumpdepth = -999.0_fp
          pdump%dumpcapaflag = .false.
          pdump%dumpdistr    = DUMPDISTR_UNIFORM
          pdump%dumpwhendry  = .false.
          ! pdump%npnt         = 0 will be set by loop over dump polygons below
          pdump%use_dunes    = .false.
          pdump%depthdef     = DEPTHDEF_REFPLANE
          pdump%ichkloc      = CHKLOC_CENTRE
          nullify(pdump%nm)
          nullify(pdump%inm)
          nullify(pdump%reflevel)
          nullify(pdump%bedlevel)
          nullify(pdump%dz_dump)
          nullify(pdump%sortvar)
          !
       endif
       !
       ! assign points to dredging and dumping areas,
       ! compute areas of grid cells and total areas dumping locations
       !
       do i = 1, nadred+nasupl
          if (dredge_prop(i)%itype == DREDGETYPE_NOURISHMENT) cycle
          cntdred        = dredge_prop(i)%idx_type
          !
          imask(:)       = 0
          npnt           = 0
          ia             = ipdr(cntdred)
          do nm = 1, ndxi                            ! only internal points
             if (jampi == 1) then 
                 if (idomain(nm) /= my_rank) cycle
             endif
             if (ba(nm) > 0.0_fp) then 
                istat = 0
                select case (dredge_prop(i)%ichkloc)
                case (CHKLOC_ALLCORNER)
                   ! check all netnodes
                   nnod = size(nd(nm)%nod)
                   do n = 1, nnod
                      if (istat >= 0) call ipon(xdr(ia), ydr(ia), npdr(cntdred), xk(nd(nm)%nod(n)), yk(nd(nm)%nod(n)), istat)
                   end do
                case (CHKLOC_CENTRE)
                   call ipon(xdr(ia), ydr(ia), npdr(cntdred), xz(nm), yz(nm), istat)
                case (CHKLOC_ANYCORNER)
                   ! check any corner, istat==0 at begin
                   call ipon(xdr(ia), ydr(ia), npdr(cntdred), xk(nd(nm)%nod(1)), yk(nd(nm)%nod(1)), istat)
                   do n = 2, size(nd(nm)%nod)
                      if (istat < 0) call ipon(xdr(ia), ydr(ia), npdr(cntdred), xk(nd(nm)%nod(n)), yk(nd(nm)%nod(n)), istat)
                   end do 
                end select
                if (istat >= 0) then
                   imask(nm) = 1
                   npnt      = npnt + 1
                endif
             endif
          enddo
          !
          dredge_prop(i)%npnt = npnt
                          allocate (dredge_prop(i)%nm(npnt)           , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%inm(npnt)          , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%area(npnt)         , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%hdune(npnt)        , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%dz_dredge(npnt)    , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%reflevel(npnt)     , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%dunetoplevel(npnt) , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%triggerlevel(npnt) , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%bedlevel(npnt)     , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%troughlevel(npnt)  , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%sedimentdepth(npnt), stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%sortvar(npnt)      , stat = istat)
          if (istat == 0) allocate (dredge_prop(i)%triggered(npnt)    , stat = istat)
          if (istat /= 0) then
             errmsg =  'ERROR RdDredge: memory alloc error'
             call write_error(errmsg, unit=mdia)
             error = .true.
             return
          endif
          !
          npnt = 0
          do nm = 1, ndx
             if (imask(nm) > 0) then
                npnt                      = npnt + 1
                dredge_prop(i)%nm(npnt)   = nm
                dredge_prop(i)%area(npnt) = ba(nm)
                globalareadred(i)         = globalareadred(i) + ba(nm)
             endif
          enddo
          do ic = 1,npnt
             dredge_prop(i)%inm(ic) = ic
          enddo
          dredge_prop(i)%hdune         = -1.0E11_fp
          dredge_prop(i)%dz_dredge     = -1.0E11_fp
          dredge_prop(i)%reflevel      = -1.0E11_fp
          dredge_prop(i)%dunetoplevel  = -1.0E11_fp
          dredge_prop(i)%triggerlevel  = -1.0E11_fp
          dredge_prop(i)%bedlevel      = -1.0E11_fp
          dredge_prop(i)%troughlevel   = -1.0E11_fp
          dredge_prop(i)%sedimentdepth = -1.0E11_fp
          dredge_prop(i)%sortvar       = -1.0E11_fp
          dredge_prop(i)%triggered     = .false.
          !
          ! Calculate average x,y coordinate of dredge location
          ! for distance between dredge and dump locations
          !
          if (npnt > 0) then
             loctemp = 0.0_fp
             do ic = 1, npnt
               inm     = dredge_prop(i)%nm(ic)
               loctemp = loctemp + xz(inm)    
             enddo
             dredge_prop(i)%dredgeloc(1) = loctemp/npnt
             loctemp                     = 0.0_fp
             do ic = 1, npnt
               inm     = dredge_prop(i)%nm(ic)
               loctemp = loctemp + yz(inm)    
             enddo
             dredge_prop(i)%dredgeloc(2) = loctemp / npnt
          else 
             dredge_prop(i)%dredgeloc(1) = -999.0_fp
             dredge_prop(i)%dredgeloc(2) = -999.0_fp
          endif
       enddo
       !
       do i = 1, nadump
          imask(:)       = 0
          npnt           = 0
          if (npdu(i) /= 0) then
             ia = ipdu(i)
             do nm = 1, ndxi
                if (jampi == 1) then 
                    if (idomain(nm) /= my_rank) cycle
                endif                 
                if (ba(nm) > 0.0_fp) then 
                   istat = 0
                   select case (dump_prop(i)%ichkloc)
                   case (CHKLOC_ALLCORNER)
                      ! check all netnodes
                      nnod = size(nd(nm)%nod)
                      do n = 1, nnod
                         if (istat >= 0) call ipon( xdu(ia), ydu(ia), npdu(i), xk(nd(nm)%nod(n)), yk(nd(nm)%nod(n)), istat)
                      end do
                   case (CHKLOC_CENTRE)
                      call ipon( xdu(ia), ydu(ia), npdu(i), xz(nm), yz(nm), istat)
                   case (CHKLOC_ANYCORNER)
                      ! check any corner, istat==0 at begin
                      call ipon( xdu(ia), ydu(ia), npdu(i), xk(nd(nm)%nod(1)), yk(nd(nm)%nod(1)), istat)
                      do n = 2, size(nd(nm)%nod)
                         if (istat < 0) call ipon (xdu(ia), ydu(ia), npdu(i), xk(nd(nm)%nod(n)), yk(nd(nm)%nod(n)), istat)
                      end do 
                   end select
                   if (istat >= 0) then
                      imask(nm) = 1
                      npnt      = npnt + 1
                   endif
                endif
             enddo
          endif
          !
          dump_prop(i)%npnt = npnt
                          allocate (dump_prop(i)%nm(npnt)      , stat = istat)
          if (istat == 0) allocate (dump_prop(i)%inm(npnt)     , stat = istat)
          if (istat == 0) allocate (dump_prop(i)%reflevel(npnt), stat = istat)
          if (istat == 0) allocate (dump_prop(i)%area(npnt)    , stat = istat)
          if (istat == 0) allocate (dump_prop(i)%hdune(npnt)   , stat = istat)
          if (istat == 0) allocate (dump_prop(i)%bedlevel(npnt), stat = istat)
          if (istat == 0) allocate (dump_prop(i)%dz_dump(npnt) , stat = istat)
          if (istat == 0) allocate (dump_prop(i)%sortvar(npnt) , stat = istat)
          if (istat /= 0) then
             errmsg =  'RdDredge: memory alloc error'
             call write_error(errmsg, unit=mdia)
             error = .true.
             return
          endif
          do ic = 1, npnt
             dump_prop(i)%inm(ic) = ic
          enddo
          dump_prop(i)%hdune    = 1.0E11_fp
          dump_prop(i)%reflevel = -1.0E11_fp
          dump_prop(i)%bedlevel = -1.0E11_fp
          dump_prop(i)%dz_dump  = -1.0E11_fp
          dump_prop(i)%sortvar  = -1.0E11_fp
          !
          npnt = 0
          do nm = 1, ndxi
             if (imask(nm) > 0) then
                npnt                    = npnt + 1
                localareadump(i)        = localareadump(i) + ba(nm)
                dump_prop(i)%nm(npnt)   = nm
                dump_prop(i)%area(npnt) = ba(nm)
             endif
          enddo
          !
          ! Calculate average x,y coordinate of dump location
          ! for distance between dredge and dump locations
          !
          if (npnt > 0) then
             loctemp = 0.0_fp
             do ic = 1, npnt
                inm     = dump_prop(i)%nm(ic)
                loctemp = loctemp + xz(inm)    
             enddo
             dump_prop(i)%dumploc(1) = loctemp / npnt
             loctemp = 0.0_fp
             do ic = 1, npnt
                inm     = dump_prop(i)%nm(ic)
                loctemp = loctemp + yz(inm)    
             enddo
             dump_prop(i)%dumploc(2) = loctemp / npnt
          else 
             dump_prop(i)%dumploc(1) = -999.0_fp
             dump_prop(i)%dumploc(2) = -999.0_fp
          endif
       enddo
       !
       do ic = 1, nalink
          x1 = dredge_prop(link_def(ic,1))%dredgeloc
          y1 =   dump_prop(link_def(ic,2))%dumploc
          if  (      comparereal(  dump_prop(link_def(ic,2))%dumploc(1)  , -999.0_fp)              == 0   &
              & .or. comparereal(  dump_prop(link_def(ic,2))%dumploc(2)  , -999.0_fp)              == 0   &
              & .or. comparereal(dredge_prop(link_def(ic,1))%dredgeloc(1), -999.0_fp)              == 0   &
              & .or. comparereal(dredge_prop(link_def(ic,1))%dredgeloc(2), -999.0_fp)              == 0   &
                .or. dredge_prop(link_def(ic,1))%itype                  == DREDGETYPE_NOURISHMENT      ) then
               link_distance(ic) = 0.0_fp
           else    
               link_distance(ic) = sqrt(abs((y1(1)-x1(1))**2.0_fp)+abs((y1(2)-x1(2))**2.0_fp ))
           endif
       enddo
       !
       ! Deallocate arrays used for detecting dredge, dump points
       !
       deallocate (ipdr , stat = istat)
       deallocate (ipdu , stat = istat)
       deallocate (npdr , stat = istat)
       deallocate (npdu , stat = istat)
       !
       deallocate (xdr  , stat = istat)
       deallocate (xdu  , stat = istat)
       deallocate (ydr  , stat = istat)
       deallocate (ydu  , stat = istat)
       !
       deallocate (imask, stat = istat)
       !
   end subroutine rddredge
end module m_rddredge


module m_dredge

   public fm_dredge

contains

subroutine fm_dredge(error)
    use precision     
    use properties 
    use table_handles    
    use message_module
    use fm_polygon_module
    use bedcomposition_module
    use m_sediment, only: mtd, stmpar, sedtra
    use m_alloc
    use m_sferic
    use m_physcoef              ! or mathconst from deltares_common...
    use m_flow, only: s1
    use m_flowtimes
    use m_flowgeom, only: bl, ndx, ndxi
    use m_dad
    use m_sediment, only: stmpar
    use unstruc_files, only: mdia
    use m_bedform
    use m_partitioninfo, only: ndomains, my_rank, numranks   ! #TODOsedmorparallel    to check numranks or ndomains
    use m_fm_morstatistics

!#TODOsedmorparallel   
!* ghost cells in in polygon dredge ... if non ghost
    
    use messageHandling
    !
    implicit none
    !
    logical                        , intent(out) :: error
    !
    ! The following list of pointer parameters is used to point inside the data structures
    !
    type (handletype)              , pointer :: tseriesfile
    real(fp)      , dimension(:,:) , pointer :: link_percentage
    real(fp)      , dimension(:,:) , pointer :: link_sum
    real(fp)      , dimension(:)   , pointer :: dzdred
    real(fp)      , dimension(:)   , pointer :: refplane
    real(fp)      , dimension(:,:) , pointer :: voldred
    real(fp)      , dimension(:)   , pointer :: totvoldred
    real(fp)      , dimension(:,:) , pointer :: totvoldredfrac
    real(fp)      , dimension(:)   , pointer :: globalareadred
    real(fp)      , dimension(:,:) , pointer :: voldump
    real(fp)      , dimension(:,:) , pointer :: percsupl
    real(fp)      , dimension(:)   , pointer :: totvoldump
    real(fp)      , dimension(:,:) , pointer :: totvoldumpfrac
    real(fp)      , dimension(:)   , pointer :: localareadump
    real(fp)      , dimension(:)   , pointer :: globalareadump
    real(fp)      , dimension(:)   , pointer :: globaldumpcap
    real(fp)      , dimension(:)   , pointer :: duneheight
    integer                        , pointer :: dredge_domainnr
    integer                        , pointer :: dredge_ndomains
    integer                        , pointer :: nadred
    integer                        , pointer :: nadump
    integer                        , pointer :: nasupl
    integer                        , pointer :: nalink
    real(fp)                        , pointer :: ntimaccum
    integer       , dimension(:,:) , pointer :: link_def
    real(fp)       , dimension(:)   , pointer :: ndredged
    real(fp)       , dimension(:)   , pointer :: nploughed
    logical                        , pointer :: tsmortime
    logical                        , pointer :: firstdredge
    type (dredtype), dimension(:)  , pointer :: dredge_prop
    type (dumptype), dimension(:)  , pointer :: dump_prop
    real(fp)                       , pointer :: morfac
    real(fp)                       , pointer :: thresh
    real(fp)                       , pointer :: bed
    real(fp)                       , pointer :: tmor
    real(hp)                       , pointer :: morft
    real(fp)      , dimension(:,:) , pointer :: dbodsd
    logical                        , pointer :: cmpupd
    integer       , dimension(:)   , pointer :: kfsed
    integer                        , pointer :: lsedtot
    real(fp)      , dimension(:)   , pointer :: cdryb
!
! Local variables
!
    integer                         :: i
    integer                         :: i2
    integer                         :: ia
    integer                         :: ib
    integer                         :: ib2
    integer                         :: id
    integer                         :: il
    integer                         :: imax
    integer                         :: imaxdunes
    integer                         :: imin
    integer                         :: imindunes
    integer                         :: inm
    integer                         :: irock
    integer                         :: istat
    integer                         :: j
    integer                         :: jnm
    integer                         :: lsed
    integer                         :: nm
    integer                         :: np
    integer                         :: in_ndomains
    integer                         :: globalnpnt
    integer                         :: localoffset
    integer                         :: ierr
    integer ,dimension(4)           :: paract
    real(fp)                        :: areatim
    real(fp)                        :: availvolume ! volume available for dredging
    real(fp)                        :: avg_alphadune
    real(fp)                        :: avg_depth
    real(fp)                        :: avg_trigdepth
    real(fp)                        :: clr
    real(fp)                        :: ddp
    real(fp)                        :: div2h
    real(fp)                        :: dmax
    real(fp)                        :: dpadd
    real(fp)                        :: dredge_area
    real(fp)                        :: dump_area
    real(fp)                        :: dz
    real(fp)                        :: dzdump
    real(fp)                        :: dzl        ! depth change due to one sediment fraction
    real(fp)                        :: extravolume
    real(fp)                        :: factor
    real(fp)                        :: fracdumped
    real(fp)                        :: fracoutlet
    real(fp)                        :: lin_dz
    real(fp)                        :: ltimhr
    real(fp)                        :: maxdumpvol ! (maximum) volume to be dumped in current time step
    real(fp)                        :: maxvol     ! maximum volume to be dredged in current time step
    real(fp)                        :: morhr
    real(fp)                        :: plough_fac ! fraction of dune height that remains after ploughing
    real(fp)                        :: qua_dz
    real(fp)                        :: requiredvolume
    real(fp)                        :: voltim     ! local volume variable, various meanings
    real(fp), dimension(1)          :: values
    real(fp)                        :: voldredged
    real(fp)                        :: voldumped
    real(fp)                        :: voltot
    real(fp)                        :: z_dredge
    real(fp)                        :: z_dump
    real(fp)                        :: zmin
    real(fp)                        :: zmax
    real(fp), dimension(:), pointer :: numpoints
    real(fp), dimension(:), pointer :: dz_dredge
    real(fp), dimension(:), pointer :: area
    real(fp), dimension(:), pointer :: hdune
    real(fp), dimension(:), pointer :: reflevel
    real(fp), dimension(:), pointer :: dunetoplevel
    real(fp), dimension(:), pointer :: triggerlevel
    real(fp), dimension(:), pointer :: bedlevel
    real(fp), dimension(:), pointer :: dz_dump
    real(fp), dimension(:), pointer :: troughlevel
    real(fp), dimension(:), pointer :: sedimentdepth
    real(fp), dimension(:), pointer :: dz_dummy
    logical                         :: dredged
    logical                         :: local_cap
    logical                         :: ploughed
    logical , dimension(:), pointer :: triggered
    character(80)                   :: msgstr
    character(256)                  :: errorstring
    type(dredtype),         pointer :: pdredge
    type(dumptype),         pointer :: pdump
!
!! executable statements -------------------------------------------------------
!
    duneheight          => bfmpar%duneheight
    tseriesfile         => dadpar%tseriesfile
    link_percentage     => dadpar%link_percentage
    link_sum            => dadpar%link_sum
    dzdred              => dadpar%dzdred
    refplane            => dadpar%refplane
    voldred             => dadpar%voldred
    totvoldred          => dadpar%totvoldred
    totvoldredfrac      => dadpar%totvoldredfrac
    globalareadred      => dadpar%globalareadred
    voldump             => dadpar%voldump
    percsupl            => dadpar%percsupl
    totvoldump          => dadpar%totvoldump
    totvoldumpfrac      => dadpar%totvoldumpfrac
    localareadump       => dadpar%localareadump
    globalareadump      => dadpar%globalareadump
    globaldumpcap       => dadpar%globaldumpcap
    dredge_domainnr     => dadpar%dredge_domainnr
    dredge_ndomains     => dadpar%dredge_ndomains
    nadred              => dadpar%nadred
    nadump              => dadpar%nadump
    nasupl              => dadpar%nasupl
    nalink              => dadpar%nalink
    ntimaccum           => dadpar%ntimaccum
    link_def            => dadpar%link_def
    tsmortime           => dadpar%tsmortime
    firstdredge         => dadpar%firstdredge
    dredge_prop         => dadpar%dredge_prop
    dump_prop           => dadpar%dump_prop
    ndredged            => dadpar%ndredged
    nploughed           => dadpar%nploughed
    kfsed               => sedtra%kfsed
    dbodsd              => sedtra%dbodsd
    lsedtot             => stmpar%lsedtot
    cdryb               => stmpar%sedpar%cdryb
    morfac              => stmpar%morpar%morfac
    thresh              => stmpar%morpar%thresh
    bed                 => stmpar%morpar%bed
    tmor                => stmpar%morpar%tmor
    morft               => stmpar%morpar%morft
    cmpupd              => stmpar%morpar%cmpupd
    !
    morhr = real(morft*24.0_hp,fp)
    if (firstdredge) then
       globalareadump = localareadump
       if (ndomains > 1) then
          !
          ! Start communication with other domains
          ! Determine number of domains that use dredging
          ! Determine "rank" of current domain (use 1-based number instead of
          ! the 0-based number returned by C routine)
          !
          dredge_domainnr = my_rank + 1 
          dredge_ndomains = numranks
          !
          ! For all dredge and dump areas count the global number of points
          ! Due to the way dredgecommunicate is implemented we need to
          ! communicate via floating point array.
          !
          allocate(numpoints(dredge_ndomains), stat = istat)
          !
          ! For each dredge area count the global number of points
          !
          do ia = 1, nadred+nasupl
             pdredge => dredge_prop(ia)
             !
             numpoints = 0.0_fp
             numpoints(dredge_domainnr) = real(pdredge%npnt,fp)
             !
             call fm_dredgecommunicate(numpoints, dredge_ndomains, error, msgstr)
             if (error) goto 999
             !
             in_ndomains = 0
             globalnpnt = 0
             localoffset = 0
             do id = 1,  dredge_ndomains
                np = nint(numpoints(id))
                if (np>0) then
                   in_ndomains = in_ndomains + 1
                   globalnpnt = globalnpnt + np
                   if (id<dredge_domainnr) localoffset = localoffset + np
                endif
             enddo
             if (in_ndomains <= 1) then
                pdredge%in1domain = .true.
             else
                pdredge%npnt = globalnpnt
                !
                ! Reallocate and shift
                !
                istat = 0
                call reallocP(pdredge%area         ,globalnpnt,fill=0.0_fp,shift=localoffset,stat=istat)
                call fm_dredgecommunicate(pdredge%area, pdredge%npnt, error, msgstr)
                if (error) goto 999
                !
                call reallocP(pdredge%hdune        ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%dz_dredge    ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%reflevel     ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%dunetoplevel ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%triggerlevel ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%bedlevel     ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%troughlevel  ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%sedimentdepth,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%sortvar      ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%inm          ,globalnpnt       ,shift=localoffset,stat=istat)
                ! nm(i)=0 for points outside this domain is used in this subroutine
                call reallocP(pdredge%nm           ,globalnpnt,fill=0,shift=localoffset,stat=istat)
                call reallocP(pdredge%triggered    ,globalnpnt       ,shift=localoffset,stat=istat)
                !
                if (istat/=0) then
                   call mess(LEVEL_ERROR, 'fm_dredge:: Dredge: memory realloc error')
                endif
                !
                globalareadred(ia) = 0.0_fp
                do i = 1,globalnpnt
                   pdredge%inm(i) = i
                   globalareadred(ia) = globalareadred(ia) + pdredge%area(i)
                enddo
             endif
          enddo
          !
          ! For each dump area count the global number of points
          !
          do ib = 1, nadump
             pdump => dump_prop(ib)
             !
             numpoints = 0.0_fp
             numpoints(dredge_domainnr) = real(pdump%npnt,fp)
             !
             call fm_dredgecommunicate(numpoints, dredge_ndomains, error, msgstr)
             if (error) goto 999
             !
             in_ndomains = 0
             globalnpnt = 0
             localoffset = 0
             do id = 1,  dredge_ndomains
                np = nint(numpoints(id))
                if (np>0) then
                   in_ndomains = in_ndomains + 1
                   globalnpnt = globalnpnt + np
                   if (id<dredge_domainnr) localoffset = localoffset + np
                endif
             enddo
             if (in_ndomains <= 1) then
                pdump%in1domain = .true.
             else
                pdump%npnt = globalnpnt
                !
                ! Reallocate and shift
                !
                istat = 0
                call reallocP(pdump%area    ,globalnpnt,fill=0.0_fp,shift=localoffset,stat=istat)
                call fm_dredgecommunicate(pdump%area, pdump%npnt, error, msgstr)
                if (error) goto 999
                !
                call reallocP(pdump%hdune   ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%reflevel,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%bedlevel,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%dz_dump ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%sortvar ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%inm     ,globalnpnt       ,shift=localoffset,stat=istat)
                ! nm(i)=0 for points outside this domain is used in this subroutine
                call reallocP(pdump%nm      ,globalnpnt,fill=0,shift=localoffset,stat=istat)
                !
                if (istat/=0) then
                   call mess(LEVEL_ERROR, 'fm_dredge:: Dredge: memory realloc error')
                endif
                !
                do i = 1,globalnpnt
                   pdump%inm(i) = i
                enddo
             endif
          enddo
          !
          deallocate(numpoints, stat = istat)
          !
          ! Communicate dump areas with other domains
          !
          call fm_dredgecommunicate(globalareadump, nadump, error, msgstr)
          if (error) goto 999
       else
          !
          ! Only one domain, so no exchange needed for any dredge or dump area
          !
          dredge_domainnr = 1
          do ia = 1, nadred+nasupl
             dredge_prop(ia)%in1domain = .true.
          enddo
          do ib = 1, nadump
             dump_prop(ib)%in1domain = .true.
          enddo
       endif
       
       firstdredge = .false.
    endif
    !
    ! JRE continue dredging
    ntimaccum = ntimaccum+dts
    !
    ! DREDGING areas include SANDMINING areas.
    !
    ! Verify for each dredge and nourishment area whether dredging
    ! respectively nourishment should occur at the current time step.
    !
    if (tsmortime) then
       ltimhr = morhr
    else
       ltimhr = time1/3600d0
    endif
    !
    do ia = 1,nadred+nasupl
       pdredge => dredge_prop(ia)
       !
       ! The default setting for dredging/nourishment is false
       ! unless there is no interval at all, then it is true.
       !
       if (pdredge%paractive(1) == -999) then
          pdredge%active = .true.
       else
          paract = pdredge%paractive
          call gettabledata(tseriesfile, paract(1) , paract(2), &
                   & paract(3), paract(4)  , values    , ltimhr   , &
                   & julrefdat   , errorstring)
          pdredge%active = values(1)>0.0_fp
       endif
    enddo
    !
    ! For each dump area determine the maximum dump capacity
    !
    do ib = 1, nadump
       pdump => dump_prop(ib)
       area => pdump%area
       reflevel => pdump%reflevel
       !
       ! Set the reference level and compute dump capacity and area.
       !
       voltim = 0.0_fp
       do i = 1,pdump%npnt
          nm = pdump%nm(i)
          if (nm==0) then
             reflevel(i) = 0.0_fp
             cycle
          endif
          !
          select case (pdump%depthdef)
          case (DEPTHDEF_REFPLANE)
             reflevel(i) = refplane(nm)
          case (DEPTHDEF_WATERLVL)
             reflevel(i) = s1(nm)
          case (DEPTHDEF_MAXREFWL)
             reflevel(i) = max(s1(nm),refplane(nm))
          case (DEPTHDEF_MINREFWL)
             reflevel(i) = min(s1(nm),refplane(nm))
          end select
          if (kfsed(nm)==1 .or. pdredge%dredgewhendry) then
             voltim = voltim + max( (reflevel(i) - pdump%mindumpdepth) - bl(nm), 0.0_fp)*area(i) ! to check
          endif
       enddo
       !
       ! If capacity limited use dump capacity to distribute sediment over the
       ! domains, otherwise use the area.
       !
       if (pdump%dumpcapaflag) then
          globaldumpcap(ib) = voltim
       else
          globaldumpcap(ib) = 0.0_fp
       endif
    enddo
    !
    if (ndomains > 1) then
       !
       ! Communicate dump capacity with other domains
       !
       call fm_dredgecommunicate(globaldumpcap, nadump, error, msgstr)
       if (error) goto 999
    endif
    !
    ! For each dredging area carry out the dredging.
    !
    do ia = 1,nadred+nasupl
       pdredge => dredge_prop(ia)
       voldred(ia,:) = 0.0_fp
       !
       ! If not in a dredging interval then go to next dredge/nourishment area
       !
       if (.not. pdredge%active) cycle
       if (pdredge%npnt==0 .and. pdredge%itype/=DREDGETYPE_NOURISHMENT) cycle
       !
       ! Maximum dredging volume depends on morphological time step.
       ! Although during the initial period morfac is arbitrary,
       ! it should effectively be set to 0.
       !
       if ((comparereal(morfac,0.0_fp) == 0) .or. (time1 < tstart_user + tmor * tfac)) then
          !
          ! Rate limited dredging will be zero during simulation phases
          ! with morfac=0. User may have allowed for (unlimited)
          ! instaneous dredging during such periods.
          !
          if (pdredge%if_morfac_0) then
             maxvol = -999.0_fp
          else
             maxvol = 0.0_fp
          endif
       elseif (comparereal(pdredge%maxvolrate  ,-999.0_fp) /= 0) then
          !
          ! Rate limited dredging.
          !
          maxvol = pdredge%maxvolrate*dts*morfac
       else
          !
          ! Dredging speed unconstrained.
          !
          maxvol = -999.0_fp
       endif
       !
       if (pdredge%dumplimited) then
          maxdumpvol = 0.0_fp
          do il = 1, nalink
             if (link_def(il,1) == ia) then
                maxdumpvol = maxdumpvol + globaldumpcap(link_def(il,2))
             endif
          enddo
          !
          if (comparereal(maxvol,-999.0_fp) == 0) then
             maxvol = maxdumpvol
          else
             maxvol = min(maxvol, maxdumpvol)
          endif
       endif
       !
       if (pdredge%itype == DREDGETYPE_NOURISHMENT) then
          if (dredge_domainnr /= 1) cycle
          !
          if (comparereal(maxvol, -999.0_fp) == 0) then
             maxvol = pdredge%totalvolsupl
          endif
          if (comparereal(pdredge%totalvolsupl, -999.0_fp) /= 0) then
             maxvol = min(pdredge%totalvolsupl,maxvol)
             pdredge%totalvolsupl = pdredge%totalvolsupl-maxvol
          endif
          do lsed = 1, lsedtot
             voldred(ia,lsed) = 0.01_fp*percsupl(pdredge%idx_type,lsed)*maxvol
          enddo
          cycle
       endif
       !
       ! Dredging down to certain depth or level, or dredging at specified rate.
       !
       hdune => pdredge%hdune
       hdune = 0.0_fp
       reflevel => pdredge%reflevel
       reflevel = 0.0_fp
       bedlevel => pdredge%bedlevel
       bedlevel = 0.0_fp
       sedimentdepth => pdredge%sedimentdepth
       sedimentdepth = 0.0_fp
       !
       do i = 1, pdredge%npnt
          nm = pdredge%nm(i)
          if (nm==0) cycle
          !
          bedlevel(i) = bl(nm)
          !
          if (pdredge%use_dunes) hdune(i) = duneheight(nm)
          !
          select case (pdredge%depthdef)
          case (DEPTHDEF_REFPLANE)
             reflevel(i) = refplane(nm)
          case (DEPTHDEF_WATERLVL)
             reflevel(i) = s1(nm)
          case (DEPTHDEF_MAXREFWL)
             reflevel(i) = max(s1(nm),refplane(nm))
          case (DEPTHDEF_MINREFWL)
             reflevel(i) = min(s1(nm),refplane(nm))
          end select
          !
          ! The sediment depth is stored always as a separate thickness instead
          ! of the bottom of the sediment column as a "rock level" for reasons
          ! of accuracy.
          !
          if (kfsed(nm)==1 .or. pdredge%dredgewhendry) then
             if (pdredge%obey_cmp) then
                call getsedthick(stmpar%morlyr, nm, sedimentdepth(i))    ! in bedcompomodule
             else
                sedimentdepth(i) = 1.0E11_fp
             endif
          else
             sedimentdepth(i) = 0.0_fp
          endif
       enddo
       !
       if (.not.pdredge%in1domain) then
          !
          ! communicate dredge data among domains
          !
          call fm_dredgecommunicate(reflevel, pdredge%npnt, error, msgstr)
          if (error) goto 999
          call fm_dredgecommunicate(bedlevel, pdredge%npnt, error, msgstr)
          if (error) goto 999
          call fm_dredgecommunicate(hdune, pdredge%npnt, error, msgstr)
          if (error) goto 999
          call fm_dredgecommunicate(sedimentdepth, pdredge%npnt, error, msgstr)
          if (error) goto 999
       endif
       !
       availvolume = 0.0_fp
       area => pdredge%area
       dz_dredge => pdredge%dz_dredge
       dunetoplevel => pdredge%dunetoplevel
       triggerlevel => pdredge%triggerlevel
       troughlevel => pdredge%troughlevel
       triggered => pdredge%triggered
       triggered = .false.
       do i = 1, pdredge%npnt
          !
          ! Derive the other characteristic levels from the bed level.
          !
          dunetoplevel(i) = bedlevel(i) + hdune(i)/2
          triggerlevel(i) = bedlevel(i) + pdredge%alpha_dh*hdune(i)
          troughlevel(i) = bedlevel(i) - hdune(i)/2
       enddo
       !
       ddp = pdredge%dredge_depth
       clr = pdredge%clearance
       if (pdredge%stilldredging) then
          !
          ! If dredging was not completed last time, lower threshold depth
          ! now by clearance level. NOTE: This implementation works only in
          ! combination with trigger_all.
          !
          ddp = ddp + clr
          clr = 0.0_fp
          pdredge%stilldredging = .false.
       endif
       !
       dredged = .false.
       ploughed = .false.
       !
       !-----------------------------------------------------------------------
       ! Trigger dredging and ploughing
       !
       plough_fac = 1.0_fp - pdredge%plough_effic
       select case (pdredge%triggertype)
       case (DREDGETRIG_POINTBYPOINT,DREDGETRIG_ALLBYONE)
          !
          ! In case of one point triggers all: check whether the bed level at
          ! any one point is above the critical level for triggering dredging.
          ! Allow only points with sediment to trigger dredging.
          !
          if (pdredge%triggertype==DREDGETRIG_ALLBYONE) then
             do i = 1, pdredge%npnt
                !
                ! The check on the bed levels will be whether
                ! triggerlevel = z_bed (+duneheight) > z_dredge = z_ref - dredgedepth
                !
                z_dredge = reflevel(i) - ddp
                if (z_dredge<triggerlevel(i) .and. sedimentdepth(i)>0.0_fp) then
                   !
                   ! If dredging is triggered, then dredge all points
                   ! above the critical level minus clearance depth.
                   !
                   ddp = ddp + clr
                   clr = 0.0_fp
                   exit
                endif
             enddo
          endif
          !
          ! Determine how much we would dredge at every location if the dredge
          ! rate is not limited by a maximum dredge rate and compute the
          ! resulting total volume.
          !
          do i = 1, pdredge%npnt
             !
             ! Trigger dredging based on depth without clearance
             ! (unless clearance has been added above due to
             ! trigger all or continuation of previous time step).
             !
             z_dredge = reflevel(i) - ddp
             if (triggerlevel(i)>z_dredge .and. sedimentdepth(i)>0.0_fp) then
                !
                if (plough_fac<1.0_fp) then
                   if (bedlevel(i)+plough_fac*pdredge%alpha_dh*hdune(i) < z_dredge-clr) then
                      !
                      ! if just ploughing the dunes is sufficient to satisfy
                      ! the critical depth plus clearance, then just plough
                      ! the dunes.
                      !
                      ploughed = .true.
                      hdune(i) = hdune(i)*plough_fac
                      dz_dredge(i) = 0.0_fp
                      cycle
                   endif
                endif
                !
                ! If dredging is triggered, lower dredging level by
                ! clearance.
                !
                triggered(i) = .true.
                z_dredge = z_dredge - clr
                if (z_dredge<=troughlevel(i)) then
                   !
                   ! Don't dredge more than is available unless
                   ! indicated otherwise.
                   !
                   dz_dredge(i) = min(bedlevel(i)-z_dredge, sedimentdepth(i))
                else
                   !
                   ! dune range:
                   ! dredgeable volume = 1/2 * dz * [(dz/H_dune) * L_dune]
                   ! effective height  = volume / L_dune = dz^2/(2*H_dune)
                   !
                   dz_dredge(i) = (dunetoplevel(i) - z_dredge)**2/(2*hdune(i))
                endif
                !
                ! Don't dredge negative amounts of sediment.
                !
                dz_dredge(i) = max(dz_dredge(i),0.0_fp)
                availvolume = availvolume + dz_dredge(i)*area(i)
             else 
                dz_dredge(i) = 0.0_fp
             endif
             !
          enddo
          requiredvolume = availvolume
       case (DREDGETRIG_ALLBYAVG)
          !
          ! In case of average triggers all: check whether the average bed
          ! level is above the critical level for triggering dredging.
          !
          avg_depth     = 0.0_fp
          avg_trigdepth = 0.0_fp
          dredge_area   = 0.0_fp
          do i = 1, pdredge%npnt
             avg_depth     = avg_depth     + (reflevel(i)-bedlevel(i))*area(i)
             avg_trigdepth = avg_trigdepth + (reflevel(i)-triggerlevel(i))*area(i)
             dredge_area   = dredge_area   + area(i)
             !
             ! maximum depth to dredge is the amount of sediment available
             ! all points with sediment are triggered
             !
             dz_dredge(i) = sedimentdepth(i)
             availvolume = availvolume + dz_dredge(i)*area(i)
             if (sedimentdepth(i)>0) then
                triggered(i) = .true.
             endif
          enddo
          avg_depth     = avg_depth/dredge_area
          avg_trigdepth = avg_trigdepth/dredge_area
          !
          if (avg_trigdepth<ddp) then
             !
             ! If dredging is triggered, lower dredging level by
             ! clearance.
             !
             avg_alphadune = avg_trigdepth - avg_depth
             if (avg_depth-plough_fac*avg_alphadune < ddp-clr) then
                !
                ! if just ploughing the dunes is sufficient to satisfy
                ! the critical depth plus clearance, then just plough
                ! the dunes.
                !
                ploughed = .true.
                do i = 1, pdredge%npnt
                   hdune(i) = hdune(i)*plough_fac
                enddo
                requiredvolume = 0.0_fp
             else
                requiredvolume = (ddp - avg_trigdepth + clr)*dredge_area
             endif
          else 
             requiredvolume = 0.0_fp
          endif
       end select
       !
       if (ploughed) then
           nploughed(ia) = nploughed(ia)+1
       endif
       if (requiredvolume > 0.0_fp .and. (maxvol < 0.0_fp .or. maxvol > 0.0_fp)) then
           ndredged(ia) = ndredged(ia)+dts
       else
           cycle
       endif
       !
       !-----------------------------------------------------------------------
       ! Perform dredging
       !
       if (comparereal(maxvol,0.0_fp) == 0) then
          !
          ! No dredging capacity, reset all dredging amounts to zero.
          !
          dz_dredge = 0.0_fp
       elseif ((maxvol > 0.0_fp .and. requiredvolume > maxvol) .or. &
             & (requiredvolume > 0.0_fp .and. requiredvolume < availvolume)) then
          !
          ! a) we need to dredge more than we can dredge per time step, or
          ! b) dredging has been triggered by an average level and we still
          !    have to figure out where to dredge.
          !
          ! In case a) limit the amount of dredging to what we can dredge and
          ! set a flag to indicate to continue dredging at the next time step
          ! The latter is only relevant in case of dredging and not in case of
          ! sandmining.
          !
          if (maxvol > 0.0_fp .and. requiredvolume > maxvol) then
             requiredvolume = maxvol
             pdredge%stilldredging = pdredge%itype==DREDGETYPE_DREDGING
          endif
          !
          ! Reduce total dredging volume and dredging amounts
          ! per point at current time step
          !
          select case (dredge_prop(ia)%dredgedistr)
          case (DREDGEDISTR_UNIFORM)
             !
             ! dredge sediment uniformly:
             !  * sort nm points based on increasing dz_dredge:
             !    thinnest sediment layer will become 1,
             !    thickest sediment layer will become pdredge%npnt
             !
             call sortindices(pdredge%inm,pdredge%npnt, &
                & dz_dredge, 1, pdredge%npnt,.true.)
             !
             !  * increase thickness gradually
             !
             dredge_area = globalareadred(ia)
             do i = 1, pdredge%npnt
                inm = pdredge%inm(i)
                !
                ! check whether dredge thickness can be found
                ! that matches the required dredging volume.
                !
                dmax = dz_dredge(inm)
                extravolume = dredge_area * dmax
                if (extravolume<requiredvolume) then
                   requiredvolume = requiredvolume - dmax*area(inm)
                   dredge_area = dredge_area - area(inm)
                else
                   dmax = requiredvolume / max(dredge_area, 0.01_fp)  ! safety
                   do j = i,pdredge%npnt
                      jnm = pdredge%inm(j)
                      dz_dredge(jnm) = dmax
                   enddo
                   exit
                endif
             enddo
          case (DREDGEDISTR_HIGHEST,DREDGEDISTR_SHALLOWEST) 
             !
             ! dredge slices off the top of the bed
             !  * sort nm points based on decreasing dunetoplevel
             !    (for simulations without dune effect, this is equal
             !    to the bed level):
             !    the highest point (max dunetoplevel) will become 1,
             !    the deepest point (min dunetoplevel) will become npnt.
             !
             ! Make sure that points that are not triggered for dredging
             ! do not actively participate in the dredging.
             !
             if (dredge_prop(ia)%dredgedistr==DREDGEDISTR_SHALLOWEST) then
                do i=1,pdredge%npnt
                   bedlevel(i)     = bedlevel(i)     - reflevel(i)
                   dunetoplevel(i) = dunetoplevel(i) - reflevel(i)
                   triggerlevel(i) = triggerlevel(i) - reflevel(i)
                   troughlevel(i)  = troughlevel(i)  - reflevel(i)
                enddo
             endif
             !
             do i=1,pdredge%npnt
                if (.not.triggered(i)) dunetoplevel(i) = -1.0E11_fp
             enddo
             !
             call sortindices(pdredge%inm,pdredge%npnt, &  
                & dunetoplevel, 1, pdredge%npnt, .false.)
             !
             !  * determine the approximate height by checking the top
             !    levels
             !
             imin = 1              ! volume above = 0.0
             imax = pdredge%npnt+1 ! dummy point: volume = availvolume>maxvol
             do while (imax>imin+1)
                i = (imin+imax)/2
                inm = pdredge%inm(i)
                !
                ! check whether there is sufficient sediment above the top
                ! of the sediment column indicated by index i.
                !
                z_dredge = dunetoplevel(inm)
                voltim = 0.0_fp
                do j = 1, i
                   jnm = pdredge%inm(j)
                   !
                   if (z_dredge<=troughlevel(jnm)) then
                      dz = min(bedlevel(jnm)-z_dredge, sedimentdepth(jnm))
                   else
                      dz = (dunetoplevel(jnm) - z_dredge)**2/(2*hdune(jnm))
                   endif
                   !
                   voltim = voltim + max(dz,0.0_fp)*area(jnm)
                enddo
                if (voltim>requiredvolume) then
                   imax = i
                else
                   imin = i
                endif
             enddo
             !
             zmax = dunetoplevel(pdredge%inm(imin))
             if (imin<pdredge%npnt) then
                zmin = dunetoplevel(pdredge%inm(imin+1))
             else
                zmin = -1.0E11_fp
             endif
             imaxdunes = imin
             do i = imin+1,pdredge%npnt
                inm = pdredge%inm(i)
                dz_dredge(inm) = 0.0_fp
             enddo
             !
             ! dredge level is known to lie between zmin and zmax
             ! points imaxdunes+1:npnt have dunetoplevel below/equal zmin
             ! points 1:imaxdunes have dunetoplevel above/equal zmax
             !
             ! now determine imindunes such that the points
             ! imindunes:imaxdunes have troughlevel below/equal zmin
             ! and dunetoplevel above/equal zmax
             !
             if (pdredge%use_dunes) then
                !
                !  * sort the first imaxdunes points based on decreasing troughlevel:
                !    the highest point (max troughlevel) will become 1,
                !    the deepest point (min troughlevel) will become imaxdunes.
                !
                call sortindices(pdredge%inm,imaxdunes, &  
                   & troughlevel, 1, pdredge%npnt, .false.)
                !
                !  * determine the approximate height by checking the trough
                !    levels
                !
                ! default imindunes = 1 in case all points 1:imaxdunes have
                !
                imindunes = 1
                do i = imaxdunes,1,-1
                   inm = pdredge%inm(i)
                   z_dredge = troughlevel(inm)
                   if (z_dredge>=zmax) then
                      !
                      ! troughlevel above zmax. Thus imindunes has been found.
                      !
                      imindunes = i+1
                      exit
                   elseif (z_dredge>zmin) then
                      !
                      ! troughlevel below zmax and above zmin. Use this level
                      ! to narrow the zmin-zmax range.
                      !
                      voltim = 0.0_fp
                      do j = 1, imaxdunes
                         jnm = pdredge%inm(j)
                         !
                         if (z_dredge<=troughlevel(jnm)) then
                            dz = min(bedlevel(jnm)-z_dredge, sedimentdepth(jnm))
                         else
                            dz = (dunetoplevel(jnm) - z_dredge)**2/(2*hdune(jnm))
                         endif
                         !
                         voltim = voltim + max(dz,0.0_fp)*area(jnm)
                      enddo
                      if (voltim>requiredvolume) then
                         !
                         ! zmin level can be raised.
                         !
                         zmin = z_dredge
                      else
                         !
                         ! after update the troughlevel is above zmax, so,
                         ! imindunes has been found.
                         !
                         zmax = z_dredge
                         imindunes = i+1
                         exit
                      endif
                      !
                   endif
                enddo
             else
                imindunes = imaxdunes+1
             endif
             !
             ! dredge level is known to lie between zmin and zmax
             ! points imaxdunes+1:npnt have dunetoplevel below/equal zmin
             ! points 1:imaxdunes have dunetoplevel above/equal zmax
             ! points imindunes:imaxdunes have troughlevel below zmin
             ! points 1:imindunes-1 have troughlevel above zmax
             !
             ! now determine irock such that the points
             ! 1:irock have bedlevel-sedimentdepth above/equal zmax
             !
             !  * sort the first imindunes-1 points based on decreasing bedlevel-sedimentdepth:
             !    the highest point (max bedlevel-sedimentdepth) will become 1,
             !    the deepest point (min bedlevel-sedimentdepth) will become imindunes-1.
             !
             pdredge%sortvar = bedlevel-sedimentdepth
             call sortindices(pdredge%inm,imindunes-1, &  
                & pdredge%sortvar, 1, pdredge%npnt, .false.)
             !
             !  * determine the approximate height by checking the trough
             !    levels
             !
             ! default imindunes = 1 in case all points 1:imaxdunes have
             !
             irock = 0
             do i = imindunes-1,1,-1
                inm = pdredge%inm(i)
                z_dredge = bedlevel(inm)-sedimentdepth(inm)
                if (z_dredge>=zmax) then
                   !
                   ! bedlevel-sedimentdepth above zmax. Thus irock has been found.
                   !
                   irock = i
                   exit
                elseif (z_dredge>zmin) then
                   !
                   ! bedlevel-sedimentdepth below zmax and above zmin. Use this level
                   ! to narrow the zmin-zmax range.
                   !
                   voltim = 0.0_fp
                   do j = 1, imaxdunes
                      jnm = pdredge%inm(j)
                      !
                      if (z_dredge<=troughlevel(jnm)) then
                         dz = min(bedlevel(jnm)-z_dredge, sedimentdepth(jnm))
                      else
                         dz = (dunetoplevel(jnm) - z_dredge)**2/(2*hdune(jnm))
                      endif
                      !
                      voltim = voltim + max(dz,0.0_fp)*area(jnm)
                   enddo
                   if (voltim>requiredvolume) then
                      !
                      ! zmin level can be raised.
                      !
                      zmin = z_dredge
                   else
                      !
                      ! after update the bedlevel-sedimentdepth is above zmax, so,
                      ! irock has been found.
                      !
                      zmax = z_dredge
                      irock = i
                      exit
                   endif
                   !
                endif
             enddo
             !
             ! dredge level is known to lie between zmin and zmax
             ! points imaxdunes+1:npnt have dunetoplevel below/equal zmin
             ! points 1:imaxdunes have dunetoplevel above/equal zmax
             ! points imindunes:imaxdunes have troughlevel below zmin
             ! points 1:imindunes-1 have troughlevel above zmax
             ! points 1:irock have bedlevel-sedimentdepth above/equal zmax
             !
             !  * determine the exact height of the critical dredge depth
             !    Critical dredge depth lies between zmin and zmax.
             !
             ! points 1:irock can be dredged completely.
             !
             voltim = 0.0_fp
             lin_dz = 0.0_fp
             qua_dz = 0.0_fp
             do i = 1,irock
                inm = pdredge%inm(i)
                !
                voltim = voltim + dz_dredge(inm)*area(inm)
                !
                if (pdredge%use_dunes) then
                   hdune(inm) = 0.0_fp
                endif
             enddo
             !
             ! at points irock+1:imindunes-1 the dunes are dredged
             ! completely and possibly a bit more.
             !
             do i = irock+1,imindunes-1
                inm = pdredge%inm(i)
                !
                dz_dredge(inm) = bedlevel(inm)-zmax
                !
                voltim = voltim + dz_dredge(inm)*area(inm)
                lin_dz = lin_dz + area(inm)
             enddo
             !
             ! at points imindunes:imaxdunes only part of the dunes
             ! will be dredged.
             !
             do i = imindunes,imaxdunes
                inm = pdredge%inm(i)
                !
                dz = dunetoplevel(inm) - zmax
                div2h = 1.0_fp /(2*hdune(inm))
                !
                dz_dredge(inm) = dz**2*div2h
                !
                voltim = voltim + dz_dredge(inm)*area(inm)
                lin_dz = lin_dz + dz*div2h*area(inm)
                qua_dz = qua_dz + area(inm)*div2h
             enddo
             !
             ! solve equation requiredvolume = voltim + dz*lin_dz + dz**2*qua_dz
             !
             if (comparereal(qua_dz,0.0_fp) == 0) then
                !
                ! a = 0
                ! b = lin_dz
                ! c = voltim-requiredvolume
                !
                ! dz = -c/b
                !
                dz = (requiredvolume-voltim)/lin_dz
             else
                !
                ! a = qua_dz
                ! b = lin_dz
                ! c = voltim-requiredvolume
                !
                ! dz = [-b +/- sqrt(b**2-4*a*c)]/(2*a)
                ! dz = -b/(2*a) + sqrt{[b/(2*a)]**2-c/a}
                !
                lin_dz = -lin_dz/(2*qua_dz)
                dz = lin_dz + sqrt(lin_dz**2+(requiredvolume-voltim)/qua_dz)
             endif
             !
             z_dredge = max(zmax-dz,zmin)
             !
             do i = irock+1,imindunes-1
                inm = pdredge%inm(i)
                !
                dz_dredge(inm) = bedlevel(inm)-z_dredge
                !
                if (pdredge%use_dunes) then
                   hdune(inm) = 0.0_fp
                endif
             enddo
             !
             do i = imindunes,imaxdunes
                inm = pdredge%inm(i)
                !
                dz = dunetoplevel(inm) - z_dredge
                div2h = 1.0_fp /(2*hdune(inm))
                !
                dz_dredge(inm) = dz**2*div2h
                !
                if (pdredge%use_dunes) then
                   hdune(inm) = 0.0_fp
                endif
             enddo
          case (DREDGEDISTR_PROPORTIONAL)
             !
             ! dredge sediment proportionally to amount of sediment available
             ! for dredging
             !
             factor = requiredvolume / availvolume
             do i = 1, pdredge%npnt
                dz_dredge(i) = dz_dredge(i) * factor
             enddo
          case (DREDGEDISTR_HIGHFIRST,DREDGEDISTR_SHALLOWFIRST)
             !
             ! dredge highest points first
             !  * sort nm points based on decreasing dunetoplevel (for
             !    simulations without dune effect, this is equal to the
             !    bed level):
             !    the highest point (max dunetoplevel) will become 1,
             !    the deepest point (min dunetoplevel) will become npnt.
             !
             ! Make sure that points that are not triggered for dredging
             ! do not actively participate in the dredging.
             !
             if (dredge_prop(ia)%dredgedistr==DREDGEDISTR_SHALLOWFIRST) then
                do i=1,pdredge%npnt
                   bedlevel(i)     = bedlevel(i)     - reflevel(i)
                   dunetoplevel(i) = dunetoplevel(i) - reflevel(i)
                   triggerlevel(i) = triggerlevel(i) - reflevel(i)
                   troughlevel(i)  = troughlevel(i)  - reflevel(i)
                enddo
             endif
             !
             do i=1,pdredge%npnt
                if (.not.triggered(i)) dunetoplevel(i) = -1.0E11_fp
             enddo
             !
             call sortindices(pdredge%inm,pdredge%npnt, &  
                & dunetoplevel, 1, pdredge%npnt, .false.)
             !
             !  dredge until you obtain the required volume
             !
             voltim = 0.0_fp
             do i = 1,pdredge%npnt
                inm = pdredge%inm(i)
                !
                if (voltim+dz_dredge(inm)*area(inm)<requiredvolume) then
                   voltim = voltim + dz_dredge(inm)*area(inm)
                   hdune(inm) = 0.0_fp
                else
                   dz_dredge(inm) = (requiredvolume-voltim)/area(inm)
                   hdune(inm) = 0.0_fp
                   !
                   do j=i+1,pdredge%npnt
                      jnm = pdredge%inm(j)
                      !
                      dz_dredge(jnm) = 0.0_fp
                      hdune(jnm) = 0.0_fp
                   enddo
                   exit
                endif
             enddo
          case (DREDGEDISTR_LOWFIRST,DREDGEDISTR_DEEPFIRST)
             !
             ! dredge lowest points first
             !  * sort nm points based on increasing dunetoplevel (for
             !    simulations without dune effect, this is equal to the
             !    bed level):
             !    the deepest point (min dunetoplevel) will become 1,
             !    the highest point (max dunetoplevel) will become npnt.
             !
             ! Make sure that points that are not triggered for dredging
             ! do not actively participate in the dredging.
             !
             if (dredge_prop(ia)%dredgedistr==DREDGEDISTR_DEEPFIRST) then
                do i=1,pdredge%npnt
                   bedlevel(i)     = bedlevel(i)     - reflevel(i)
                   dunetoplevel(i) = dunetoplevel(i) - reflevel(i)
                   triggerlevel(i) = triggerlevel(i) - reflevel(i)
                   troughlevel(i)  = troughlevel(i)  - reflevel(i)
                enddo
             endif
             !
             do i=1,pdredge%npnt
                if (.not.triggered(i)) dunetoplevel(i) = 1.0E11_fp
             enddo
             !
             call sortindices(pdredge%inm,pdredge%npnt, &  
                & dunetoplevel, 1, pdredge%npnt, .true.)
             !
             !  dredge until you obtain the required volume
             !
             voltim = 0.0_fp
             do i = 1,pdredge%npnt
                inm = pdredge%inm(i)
                !
                if (voltim+dz_dredge(inm)*area(inm)<requiredvolume) then
                   voltim = voltim + dz_dredge(inm)*area(inm)
                   hdune(inm) = 0.0_fp
                else
                   dz_dredge(inm) = (requiredvolume-voltim)/area(inm)
                   hdune(inm) = 0.0_fp
                   !
                   do j=i+1,pdredge%npnt
                      jnm = pdredge%inm(j)
                      !
                      dz_dredge(jnm) = 0.0_fp
                      hdune(jnm) = 0.0_fp
                   enddo
                   exit
                endif
             enddo
          end select
       else
          !
          ! Dredging not limited by maximum volume, so we will dredge the
          ! dz_dredge amount already computed.
          !
          if (pdredge%use_dunes) then 
             do i = 1, pdredge%npnt
                nm = pdredge%nm(i)
                !
                if (dz_dredge(i)>0.0_fp) then
                   hdune(i) = 0.0_fp
                endif
             enddo
          endif
       endif
       !
       do i = 1,pdredge%npnt
          nm = pdredge%nm(i)
          if (nm==0) cycle
          !
          dzdred(nm) = dz_dredge(i)
          if (pdredge%use_dunes) duneheight(nm) = hdune(i)
       enddo
       !
       ! Update sediment administration for sandmining/dredging only
       ! dbodsd is filled (kg/m^2 sediment removed in a cell)
       !
       if (cmpupd) then
          if (gettoplyr(stmpar%morlyr, dzdred, dbodsd, mtd%messages) /= 0) then
             call writemessages(mtd%messages, mdia)
             error = .true.
             return
          endif
       else
          dbodsd = 0.0_fp
       endif
       !
       ! Use dbodsd to calculate voldred, and update dps
       !
       do i = 1, pdredge%npnt
          nm = pdredge%nm(i)
          if (nm==0) cycle
          !
          dz = 0.0_fp
          do lsed = 1, lsedtot
             dzl               = dbodsd(lsed, nm) / cdryb(lsed)
             voldred(ia,lsed)  = voldred(ia,lsed) + dzl * area(i)
             dz                = dz + dzl
          enddo
          if (pdredge%obey_cmp) then
             bl(nm)               = bl(nm) - dz
          else
             bl(nm)               = bl(nm) - dz_dredge(i)
             voldred(ia,lsedtot+1) = voldred(ia,lsedtot+1) + (dz_dredge(i)-dz) * area(i)
          endif
          dzdred(nm)        = 0.0_fp
       enddo
    enddo
    !
    if (ndomains > 1) then
       !
       ! Communicate dredged volumes with other domains
       !
       call fm_dredgecommunicate(voldred, (nadred+nasupl)*(lsedtot+1), error, msgstr)
       if (error) goto 999
    endif
    !
    !--------------------------------------------------------------------------
    ! Distribute sediments over dump areas
    !
    voldump(1:nadump,1:lsedtot) = 0.0_fp
    do ia = 1, nadred+nasupl
       pdredge => dredge_prop(ia)
       !
       ! Add dredged volumes to the total dredged volumes (cumulative!)
       !
       voldredged = 0.0_fp
       do lsed = 1, lsedtot
          voldredged = voldredged + voldred(ia,lsed)
       enddo
       totvoldred(ia) = totvoldred(ia) + voldredged + voldred(ia,lsedtot+1)
       totvoldredfrac(ia,:) = totvoldredfrac(ia,:) + voldred(ia,:)
       if (voldredged<=0.0_fp) cycle
       !
       select case (pdredge%dumpdistr)
       case (DR2DUDISTR_PERCENTAGE)
          !
          ! Distribute based on user-specified percentages
          !
          do i = 1,nalink
             if (link_def(i,1) /= ia) cycle
             ib = link_def(i,2)
             i2 = pdredge%outletlink
             if (i2>0) then
                ib2 = link_def(i2,2)
             else
                ib2 = 0
             endif
             !
             voldumped = 0.0_fp
             do lsed = 1,lsedtot
                voltim = 0.01_fp*link_percentage(i,lsed)*voldred(ia,lsed)
                voldumped = voldumped + voltim
             enddo
             !
             if (voldumped>globaldumpcap(ib) .and. dump_prop(ib)%dumpcapaflag) then
                fracdumped = globaldumpcap(ib)/voldumped
                globaldumpcap(ib) = 0.0_fp
             else
                fracdumped = 1.0_fp
                if (dump_prop(ib)%dumpcapaflag) then
                   globaldumpcap(ib) = globaldumpcap(ib)-voldumped
                endif
             endif
             fracoutlet = 1.0_fp - fracdumped
             !
             do lsed = 1,lsedtot
                voltim = 0.01_fp*link_percentage(i,lsed)*voldred(ia,lsed)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim*fracdumped
                voldump(ib, lsed) = voldump(ib, lsed) + voltim*fracdumped
                !
                if (ib2>0) then
                   link_sum(i2, lsed) = link_sum(i2, lsed) + voltim*fracoutlet
                   voldump(ib2, lsed) = voldump(ib2, lsed) + voltim*fracoutlet
                endif
             enddo
          enddo
       case (DR2DUDISTR_SEQUENTIAL)
          !
          ! Distribute according user-specified order up to maximum
          ! capacity
          !
          voldumped = 0.0_fp
          do i = 1,nalink
             if (link_def(i,1) /= ia) cycle
             ib = link_def(i,2)
             maxvol = voldredged-voldumped
             if (dump_prop(ib)%dumpcapaflag) then
                maxvol = min(maxvol,globaldumpcap(ib))
             endif
             !
             do lsed = 1,lsedtot
                voltim = maxvol*(voldred(ia,lsed)/voldredged)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim
                voldump(ib, lsed) = voldump(ib, lsed) + voltim
             enddo
             if (dump_prop(ib)%dumpcapaflag) then
                globaldumpcap(ib) = globaldumpcap(ib)-maxvol
             endif
             !
             voldumped = voldumped + maxvol
             if (comparereal(voldredged,voldumped) == 0) exit
          enddo
          !
          ! Maximum capacity reached; any sediment remaining?
          !
          if (voldredged>voldumped) then
             maxvol = voldredged - voldumped
             i = pdredge%outletlink
             ib = link_def(i,2)
             do lsed = 1,lsedtot
                voltim = maxvol*(voldred(ia,lsed)/voldredged)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim
                voldump(ib, lsed) = voldump(ib, lsed) + voltim
             enddo
          endif
       case (DR2DUDISTR_PROPORTIONAL)
          maxvol = 0.0_fp
          do i = 1,nalink
             if (link_def(i,1) /= ia) cycle
             if (i==pdredge%outletlink) cycle
             ib = link_def(i,2)
             maxvol = maxvol + globaldumpcap(ib)
          enddo
          !
          ! Distribute proportionally based on dumping capacity
          ! Don't dump more than capacity available
          !
          voldumped = min(voldredged,maxvol)
          if (voldumped>0.0_fp) then
             do i = 1,nalink
                if (link_def(i,1) /= ia) cycle
                if (i==pdredge%outletlink) cycle
                ib = link_def(i,2)
                !
                voltot = (globaldumpcap(ib)/maxvol)*voldumped
                do lsed = 1,lsedtot
                   voltim = voltot*(voldred(ia,lsed)/voldredged)
                   link_sum(i, lsed) = link_sum(i, lsed) + voltim
                   voldump(ib, lsed) = voldump(ib, lsed) + voltim
                enddo
                globaldumpcap(ib) = globaldumpcap(ib)-voltot
             enddo
          endif
          !
          ! Maximum capacity reached; any sediment remaining?
          !
          if (voldredged>voldumped) then
             maxvol = voldredged - voldumped
             i = pdredge%outletlink
             ib = link_def(i,2)
             do lsed = 1,lsedtot
                voltim = maxvol*(voldred(ia,lsed)/voldredged)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim
                voldump(ib, lsed) = voldump(ib, lsed) + voltim
             enddo
          endif
       end select
    enddo
!
    !--------------------------------------------------------------------------
    ! And finally: Dumping
    !
    dbodsd(1:lsedtot, 1:ndx) = 0.0_fp
    do ib = 1, nadump
       pdump => dump_prop(ib)
       !
       ! Add dumped volumes to the total dumped volumes (cumulative!)
       !
       voldumped = 0.0_fp
       do lsed = 1, lsedtot
          voldumped = voldumped + voldump(ib, lsed)
       enddo
       totvoldump(ib)       = totvoldump(ib) + voldumped
       totvoldumpfrac(ib,:) = totvoldumpfrac(ib,:) + voldump(ib,:)
       !
       ! Skip dump areas where nothing has to be dumped
       !
       if (comparereal(voldumped,0.0_fp) == 0) cycle
       !
       bedlevel => pdump%bedlevel
       bedlevel = 0.0_fp
       hdune => pdump%hdune
       hdune = 0.0_fp
       dz_dump => pdump%dz_dump
       dz_dump = 0.0_fp
       reflevel => pdump%reflevel
       local_cap = .false.
       do i = 1, pdump%npnt
          nm = pdump%nm(i)
          if (nm==0) cycle
          !
          bedlevel(i) = real(bl(nm),fp)
          if (pdump%use_dunes) hdune(i) = duneheight(nm)
          !
          if (kfsed(nm)==1 .or. pdump%dumpwhendry) then
             if (pdump%dumpcapaflag) then
                dz_dump(i) = max( (reflevel(i) - pdump%mindumpdepth) - bedlevel(i), 0.0_fp)
                local_cap = local_cap .or. dz_dump(i)>0.0_fp
             else
                dz_dump(i) = 1.0E11_fp   ! JRE to check
                local_cap = .true.
             endif
          else
             dz_dump(i) = 0.0_fp
          endif
       enddo
       !
       area => pdump%area
       if (.not.pdump%in1domain) then
          !
          ! communicate dredge data among domains
          !
          call fm_dredgecommunicate(reflevel, pdump%npnt, error, msgstr)
          if (error) goto 999
          call fm_dredgecommunicate(bedlevel, pdump%npnt, error, msgstr)
          if (error) goto 999
          call fm_dredgecommunicate(dz_dump, pdump%npnt, error, msgstr)
          if (error) goto 999
       endif
       !
       ! Go through dumping procedure only if some dump capacity is available
       ! locally
       !
       if (.not.local_cap) cycle
       !
       select case (pdump%dumpdistr)
       case (DUMPDISTR_UNIFORM)
          !
          ! dump sediment uniformly:
          !  * sort nm points based on increasing dump capacity dz_dump:
          !    least capacity will become 1, maximum capacity wil become
          !    pdump%npnt.
          !
          call sortindices(pdump%inm,pdump%npnt, &
             & dz_dump, 1, pdump%npnt,.true.)
          !
          ! loop over points and increase dzdump gradually
          !
          requiredvolume = voldumped
          dump_area = globalareadump(ib)
          do i = 1, pdump%npnt
             inm = pdump%inm(i)
             !
             extravolume = dz_dump(inm)*dump_area
             if (extravolume<requiredvolume) then
                !
                ! if insufficient capacity at current point, fill it up
                ! and continue with next point
                !
                dump_area = dump_area - area(inm)
                requiredvolume = requiredvolume - dz_dump(inm)*area(inm)
             else
                dzdump = dz_dump(inm)*requiredvolume/extravolume
                !
                ! if sufficient capacity, fill all remaining points and
                ! exit loop
                !
                do j = i, pdump%npnt
                   jnm = pdump%inm(j)
                   !
                   dz_dump(jnm) = dzdump
                enddo
                exit
             endif
          enddo
       case (DUMPDISTR_LOWEST,DUMPDISTR_DEEPEST)
          !
          ! lowest or deepest locations first:
          !  * sort nm points based on increasing bedlevel:
          !    deepest point (min bedlevel) will become 1,
          !    shallowest point (max bedlevel) will become pdump%npnt.
          !
          if (pdump%dumpdistr == DUMPDISTR_DEEPEST) then
             do i=1,pdump%npnt
                bedlevel(i) = bedlevel(i) - reflevel(i)
             enddo
          endif
          !
          call sortindices(pdump%inm,pdump%npnt, &
             & bedlevel, 1, pdump%npnt,.true.)
          !
          !  * search bed level below which sufficient dumping capacity is
          !    available
          !
          requiredvolume = voldumped
          do i = 2,pdump%npnt
             inm = pdump%inm(i)
             z_dump = bedlevel(inm)
             !
             voltim = 0.0_fp
             do j = 1, i-1
                jnm = pdump%inm(j)
                !
                voltim = voltim + max(min(dz_dump(jnm),z_dump-bedlevel(jnm)),0.0_fp)*area(jnm)
             enddo
             !
             if (voltim>=requiredvolume) exit
          enddo
          imax = i-1
          if (imax==pdump%npnt) then
             zmax = 1.0E11_fp
          else
             zmax = z_dump
          endif
          zmin = bedlevel(pdump%inm(imax))
          !
          ! dump level is known to lie between zmin and zmax
          ! points imax+1:npnt have bedlevel above zmax
          ! points 1:imax have bedlevel below zmin
          !
          !  * sort the first imax points based on increasing level of bed
          !    level plus maximum dump thickness
          !
          pdump%sortvar = bedlevel+dz_dump
          call sortindices(pdump%inm,imax, &
             & pdump%sortvar, 1, pdump%npnt,.true.)
          !
          do i = 1,imax
             inm = pdump%inm(i)
             z_dump = pdump%sortvar(inm)
             !
             if (z_dump<zmin) cycle
             if (z_dump>zmax) exit
             !
             voltim = 0.0_fp
             do j = 1, imax
                jnm = pdump%inm(j)
                !
                voltim = voltim + max(min(dz_dump(jnm),z_dump-bedlevel(jnm)),0.0_fp)*area(jnm)
             enddo
             !
             if (voltim>=requiredvolume) then
                zmax = z_dump
                exit
             else
                zmin = z_dump
             endif
          enddo
          imin = i
          !
          ! dump level is known to lie between zmin and zmax
          ! points imax+1:npnt have bedlevel above zmax
          ! points 1:imax have bedlevel below zmin
          ! points 1:imin-1 have capacity limit below zmin
          ! points imin:imax have capacity limit above zmax
          !
          !  * determine exact height of dump level which lies between
          !    zmin and zmax
          !
          voltim = 0.0_fp
          areatim = 0.0_fp
          z_dump = zmin
          do i = 1,imax
             inm = pdump%inm(i)
             !
             voltim = voltim + max(min(dz_dump(inm),z_dump-bedlevel(inm)),0.0_fp)*area(inm)
             if (i>=imin) areatim = areatim + area(inm)
          enddo
          dz = (requiredvolume - voltim)/areatim
          z_dump = zmin + dz
          !
          do i = 1, pdump%npnt
             inm = pdump%inm(i)
             !
             ! determine the thickness of the local deposit
             ! determine the associated volume
             !
             dz_dump(inm) = max(min(dz_dump(inm),z_dump-bedlevel(inm)),0.0_fp)
          enddo
       case (DUMPDISTR_PROPORTIONAL)
          !
          ! proportional to maximum dump depth
          ! determine ratio of dumped volume and capacity
          !
          maxvol = 0.0_fp
          do i = 1, pdump%npnt
             maxvol = maxvol + dz_dump(i)*area(i)
          enddo
          factor = voldumped / maxvol
          do i = 1, pdump%npnt
             dz_dump(i) = dz_dump(i)*factor
          enddo
       end select
       !
       ! Now dump the sediments locally
       !
       do i = 1, pdump%npnt
          nm = pdump%nm(i)
          if (nm==0) cycle
          !
          dz = dz_dump(i)
          do lsed = 1, lsedtot
             dpadd            = dz * (voldump(ib, lsed) / voldumped)
             dbodsd(lsed, nm) = dbodsd(lsed, nm) + dpadd * cdryb(lsed)
          enddo
          !
          bl(nm) = bl(nm) + real(dz_dump(i), prec)
          if (pdump%use_dunes) duneheight(nm) = hdune(i)
       enddo
    enddo
    !
    ! Update sediment administration for dumping only
    ! dbodsd is filled (kg/m^2 sediment added to a cell)
    !
    if (cmpupd) then 
       allocate(dz_dummy(1:ndx), stat=istat)   ! no actual bed update, unlike updmorlyr in fm_erosed.f90
       call morstats(dbodsd)
       if (updmorlyr(stmpar%morlyr, dbodsd, dz_dummy, mtd%messages) /= 0) then
          call writemessages(mtd%messages, mdia)
          error = .true.
          return
       else
          call writemessages(mtd%messages, mdia)
       endif
       deallocate(dz_dummy, stat=istat)
    endif
    return

999 call write_error(msgstr, unit=mdia)
    error = .true.
    return
end subroutine fm_dredge
   
end module m_dredge

subroutine sortindices(nm     ,npnt   ,val    ,nmlb   ,nmub   ,increasing)
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer                                              , intent(in)   :: npnt
    integer   , dimension(1:npnt)                        , intent(inout):: nm
    integer                                              , intent(in)   :: nmlb
    integer                                              , intent(in)   :: nmub
    real(fp)  , dimension(nmlb:nmub)                     , intent(in)   :: val
    logical                                              , intent(in)   :: increasing
!
! Local variables
!
    integer   :: i
    integer   :: ibi
    integer   :: imin
    integer   :: imax
    integer   :: j
    integer   :: nmi
    real(fp)  :: vi
    real(fp)  :: sign
!
!! executable statements -------------------------------------------------------
!
    sign = -1.0_fp
    if (increasing) sign = 1.0_fp
    !
    do i = 2, npnt
       nmi = nm(i)
       vi  = sign*val(nmi)
       !
       ! vi bigger than the currently biggest value
       !
       if (vi>=sign*val(nm(i-1))) cycle
       !
       ! vi smaller than the currently smallest value
       !
       if (vi<sign*val(nm(1))) then
          do j = i,2,-1
             nm(j) = nm(j-1)
          enddo
          nm(1) = nmi
          cycle
       endif
       !
       ! vi somewhere in between
       !
       imin = 1
       imax = i-1
       do while (imax>imin+1)
          ibi = (imax+imin)/2
          if (vi>=sign*val(nm(ibi))) then
             imin = ibi
          else
             imax = ibi
          endif
       enddo
       !
       ! vi between imin and imax
       !
       do j = i,imax+1,-1
          nm(j) = nm(j-1)
       enddo
       nm(imax) = nmi
    enddo
    !
end subroutine sortindices

subroutine fm_dredgecommunicate(a, n, error, msgstr)
    use precision
    use m_partitioninfo
    implicit none
    !
    integer               , intent(in)    :: n      ! length of real array
    real(fp), dimension(n), intent(inout) :: a      ! real array to be accumulated
    logical               , intent(out)   :: error  ! error flag
    character(*)          , intent(out)   :: msgstr ! string to pass message
    !
    error = .false. 
    msgstr = ''
    call reduce_sum(n, a)
    
end subroutine fm_dredgecommunicate
