c
c     Program to decompose a PROCES.ASC file into tables
c     28-2-2000: Start upgrade om PDF's in te checken
c     04-8-2000: Finish
c     13-01-2011: Jos van Gils, update to import asci files produced by the DUPROL parser
c     27-12-2012: Modify for Open Source, table P5 and R1/R2/R9 are no longer relevant
c
      include 'data.inc'
      character*1  c1
      character*10 c10, c10b, c10a
      character*20 c20
      character*30 grp
      character*50 c50
      character*10 initialConfgId
      character*50 initialConfgName
      character*80 pdffil, procesnaam
      character*255 ArgumentString
      real         value
      integer      jndex , naanta, iaanta, iproc , i     , ihulp ,
     j             noffse, ihulp2, ihulp3, ihulp4, nprocl,
     j             noffsf, iitem
      integer      delete, replac, insert, abort , none
      parameter   (delete = 1, replac = 2, insert = 3, abort  = 0,
     j             none   = 4)
      logical      newtab, duprol, newfrm
      integer      io_mes, io_asc, io_inp
      data         io_mes /11/
      data         io_asc /14/
      data         io_inp /15/
      data         grp /'DummyGroup                          '/
      data         initialConfgId /'DummyConfg'/
      
c     Command line arguments

      newtab = .false.
      newfrm = .false.
      duprol = .false.
      pdffil = 'proces.asc'
      do i=1,9999
            call getarg (i,ArgumentString)
            if (ArgumentString.eq.'') exit
            if (index(ArgumentString,'-pdf').gt.0) then
            pdffil = trim(ArgumentString(5:))
            endif
            if (index(ArgumentString,'-duprol').gt.0) duprol = .true.
            if (index(ArgumentString,'-newtab').gt.0) newtab = .true.
            if (index(ArgumentString,'-newfrm').gt.0) newfrm = .true.
      enddo

      open ( io_mes , file = 'waqpb_import.log' )
	if (duprol) then
          write (io_mes,'(''DUPROL import'')')
          newtab = .true.
          write (io_mes,'('' (obligatory to create new tables'')')
          newfrm = .true.
          write (io_mes,'('' (obligatory to use new format'')')
      else
	  if (newtab) then
          write (io_mes,'(''Creating new tables'')')
	  else
          write (io_mes,'(''Updating existing tables'')')
	  endif
	  if (newfrm) then
          write (io_mes,'(''Using NEW format'')')
	  else
          write (io_mes,'(''Using OLD format'')')
	  endif
      endif
      initialConfgName = initialConfgId

      nitem = 0
      nfort = 0
      nproc = 0
      ninpu = 0
      noutp = 0
      noutf = 0
      nstoc = 0 
      ndisp = 0
      nvelo = 0

      if (.not.newtab) then
          write (*,'('' Loading database......'')')
          write (*,*)
          write (io_mes,'(''Loading database......'')')
c         Read the existing tables
          call readdb ( io_inp , io_mes )
c         Store R1 in relational way
          ncnpr = 0
          do 10 iproc = 1,nproc
          do 10 iconf = 1,nconf
              if ( conpro(iconf,iproc) ) then
                  ncnpr = ncnpr+1
                  r1_pid(ncnpr) = procid(iproc)
                  r1_cid(ncnpr) = confid(iconf)
              endif
   10     continue
c         Remove primary   table  P4
c         Remove secondary tables R4 till R8
          nproc = 0
          ninpu = 0
          noutp = 0
          noutf = 0
          nstoc = 0
          ndisp = 0
          nvelo = 0
      endif

      write (*,'('' Decomposing '',a,''......'')') pdffil
      write (*,*)
      write (io_mes,'('' Decomposing '',a,''......'')') pdffil
      open ( io_asc , file = pdffil , status='old', err=999)
      read ( io_asc , * ) nprocl
      call iniind

    1 continue

c----------------------------------------------------------------------c
c     We decompose the Proces.asc
c----------------------------------------------------------------------c

      do 100 iproc = 1,nprocl

c         proces name and description
          read ( io_asc , '(a10,20x,a50)' ) c10,c50
          write ( io_mes , '(''Process '',a10)' ) c10
          write (*,'(''Process: '',a10)') c10
	    if (duprol) grp = c10

c         fortran code
          read ( io_asc , '(a10)' ) c10a

c         transport code
          read ( io_asc , * ) jndex

          if ( nproc+1 .gt. nprocm ) stop 'DIMENSION NPROCM'
          nproc = nproc + 1
          procid(nproc) = c10
          procnm(nproc) = c50
          procfo(nproc) = c10a
          procco(nproc) = jndex

          call upd_p3 ( c10a , newtab , io_mes )

c         input items on segment level

          read ( io_asc , '(i10)' ) naanta
          ihulp = naanta
          do 15 iaanta = 1,naanta
              if ( newfrm ) then
                  read ( io_asc , 3001) c10,value,c1,c50,c20
              else
                  read ( io_asc , 2001) c10,value,c1,c50
	            c20 = ' '
              endif
              call upd_p2 ( c10, c50, value, 1, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .false. )

              ninpu = ninpu + 1
              if ( ninpu .gt. ninpum ) then
                  write (*,*) ninpu
                  stop 'DIMENSION NINPUM'
              endif
              inpupr(ninpu) = procid(nproc)
              inpuit(ninpu) = itemid(iitem)
              inpunm(ninpu) = iaanta
              inpudo(ninpu) = c1

              if ( abs(value+999.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'N'
              elseif ( abs(value+888.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'G'
              elseif ( abs(value+101.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'B'
              elseif ( abs(value+11.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'M'
              elseif ( abs(value+1.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'O'
              else
                  inpude(ninpu) = 'Y'
              endif
c             Switch to decide segment/exchange!
              inpusx(ninpu) = 1

   15     continue

c         input items on exchange level

          read ( io_asc , '(i10)' ) naanta
          do 20 iaanta = 1,naanta
              if ( newfrm ) then
                  read ( io_asc , 3001) c10,value,c1,c50,c20
              else
                  read ( io_asc , 2001) c10,value,c1,c50
	            c20 = ' '
              endif
              call upd_p2 ( c10, c50, value, 2, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .false. )

              ninpu = ninpu + 1
              if ( ninpu .gt. ninpum ) then
                  write (*,*) ninpu
                  stop 'DIMENSION NINPUM'
              endif
              inpupr(ninpu) = procid(nproc)
              inpuit(ninpu) = itemid(iitem)
              inpunm(ninpu) = iaanta + ihulp
              inpudo(ninpu) = c1
              if ( abs(value+999.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'N'
              elseif ( abs(value+888.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'G'
              elseif ( abs(value+101.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'B'
              elseif ( abs(value+11.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'M'
              elseif ( abs(value+1.) .lt. 1e-10 ) then
                  inpude(ninpu) = 'O'
              else
                  inpude(ninpu) = 'Y'
              endif
c             Switch to decide segment/exchange!
              inpusx(ninpu) = 0
   20     continue

c         output items on segment level

          read ( io_asc , '(i10)' ) naanta
          ihulp = naanta
          do 30 iaanta = 1,naanta
              if ( newfrm ) then
                  read ( io_asc , 3002) c10,c1,c50,c20
              else
                  read ( io_asc , 2002) c10,c1,c50
	            c20 = ' '
              endif
              value = -999.
              call upd_p2 ( c10, c50, value, 1, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .false. )

              noutp = noutp + 1
              if ( noutp .gt. noutpm ) stop 'DIMENSION NOUTPM'
              outppr(noutp) = procid(nproc)
              outpit(noutp) = itemid(iitem)
              outpnm(noutp) = iaanta
              outpdo(noutp) = c1
c             Switch to decide segment/exchange!
              outpsx(noutp) = 1
   30     continue

c         output items on exchange level

          read ( io_asc , '(i10)' ) naanta
          do 40 iaanta = 1,naanta
              if ( newfrm ) then
                  read ( io_asc , 3002) c10,c1,c50,c20
              else
                  read ( io_asc , 2002) c10,c1,c50
	            c20 = ' '
              endif
              value = -999.
              call upd_p2 ( c10, c50, value, 2, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .false. )

              noutp = noutp + 1
              if ( noutp .gt. noutpm ) stop 'DIMENSION NOUTPM'
              outppr(noutp) = procid(nproc)
              outpit(noutp) = itemid(iitem)
              outpnm(noutp) = iaanta + ihulp
              outpdo(noutp) = c1
c             Switch to decide segment/exchange!
              outpsx(noutp) = 0
   40     continue

c         fluxes

          noffsf = noutf
          read ( io_asc , '(i10)' ) naanta
          do 50 iaanta = 1,naanta
              if ( newfrm ) then
                  read ( io_asc , 3002) c10,c1,c50,c20
              else
                  read ( io_asc , 2002) c10,c1,c50
	            c20 = ' '
              endif
              value = -999.
              call upd_p2 ( c10, c50, value, 1, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .false. )

              noutf = noutf + 1
              if ( noutf .gt. noutfm ) stop 'DIMENSION NOUTFM'
              outfpr(noutf) = procid(nproc)
              outffl(noutf) = c10
              outfnm(noutf) = iaanta
              outfdo(noutf) = c1
   50     continue

c         stochi lines

          noffse = nstoc
          read ( io_asc , '(i10)' ) naanta
          do 60 iaanta = 1,naanta
              read ( io_asc , '(a10,2x,a10,2x,f10.0)' ) c10,c10b,value

c             check presence of current flux in fluxes under current process
              call zoek (c10b,(noutf-noffsf),outffl(noffsf+1),10,ihulp)
              if ( ihulp .le. 0 ) then
                  write (*,*) ' Illegal flux in stochi line!!'
                  write (*,*) c10b
                  stop ' Fatal error'
              endif
              nstoc = nstoc + 1
              if ( nstoc .gt. nstocm ) stop 'DIMENSION NSTOCM'
              stocfl(nstoc) = c10b
              stocsu(nstoc) = c10
              stocsc(nstoc) = value

              value = -999.
              c50 = ' '
	        c20 = ' '
              call upd_p2 ( c10, c50, value, 0, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .false. )
   60     continue

c         stochi lines D

          noffse = ndisp
          read ( io_asc , '(i10)' ) naanta
          do 80 iaanta = 1,naanta
              read ( io_asc , '(a10,2x,a10,2x,f10.0)' ) c10,c10b,value

              ndisp = ndisp + 1
              if ( ndisp .gt. ndispm ) stop 'DIMENSION NDISPM'
              dispit(ndisp) = c10b
              dispsu(ndisp) = c10
              dispsc(ndisp) = value

              value = -999.
              c50 = ' '
              c20 = ' '
              call upd_p2 ( c10, c50, value, 0, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .false. )
   80     continue

c         stochi lines V

          noffse = nvelo
          read ( io_asc , '(i10)' ) naanta
          do 70 iaanta = 1,naanta
              read ( io_asc , '(a10,2x,a10,2x,f10.0)' ) c10,c10b,value

              nvelo = nvelo + 1
              if ( nvelo .gt. nvelom ) stop 'DIMENSION NVELOM'
              veloit(nvelo) = c10b
              velosu(nvelo) = c10
              velosc(nvelo) = value

              value = -999.
              c50 = ' '
              c20 = ' '
              call upd_p2 ( c10, c50, value, 0, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .false. )
   70     continue
          read ( io_asc , '(a10)' ) c10
          if ( c10(1:3) .ne. 'END' ) STOP 'error'
  100 continue

c         DUPROL format has two additional blocks, specifying active and inactive substances

      if ( duprol ) then
          read ( io_asc , '(i10)' ) naanta
          do iaanta = 1,naanta
              read ( io_asc , 3002) c10,c1,c50,c20
              value = -999.
              call upd_p2 ( c10, c50, value, 0, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .false. )
          enddo
          read ( io_asc , '(i10)' ) naanta
          do iaanta = 1,naanta
              read ( io_asc , 3002) c10,c1,c50,c20
              value = -999.
              call upd_p2 ( c10, c50, value, 0, newtab, grp, io_mes ,
     j                      iitem, c20, newfrm, .true. )
          enddo
      endif
      close (io_asc)

c     Sort and check R6-R7-R8

      call sortst ( stocfl, stocsu, stocsc, nstoc )
      call chksto ( stocfl, stocsu, stocsc, nstoc , itemid, nitem )
      call sortst ( dispit, dispsu, dispsc, ndisp )
      call chksto ( dispit, dispsu, dispsc, ndisp , itemid, nitem )
      call sortst ( veloit, velosu, velosc, nvelo )
      call chksto ( veloit, velosu, velosc, nvelo , itemid, nitem )

c     Create/update tables R1, R2
      call cratab (grp,newtab,initialConfgId,initialConfgName)

c     Clear tables
      call cldept

c----------------------------------------------------------------------c
c     Adhoc correction on default values
c     BLOOMAlgii must be -101
c     Only -dis and -par quantities may have default value -11
c----------------------------------------------------------------------c

      if ( newtab ) then

      do 120 i = 1,nitem
          call zoek (itemid(i)(1:8),1,'bloomalg',8,ihulp)
          if ( ihulp .gt. 0 ) then
              if ( itemid(i)(9:10) .eq. '01' ) then
                  itemde(i) = -999.
              else
                  itemde(i) = -101.
              endif
          endif
          if ( abs(itemde(i)+11.0).lt.1e-10 ) then
              ihulp  = index (itemid(i),'-dis')
              ihulp2 = index (itemid(i),'-par')
              ihulp3 = index (itemid(i),'-Dis')
              ihulp4 = index (itemid(i),'-Par')
              if ( ihulp  .le. 0 .and. ihulp2 .le. 0 .and.
     j             ihulp3 .le. 0 .and. ihulp4 .le. 0 ) then
                  itemde(i) = -999.
              endif
          endif
          call zoek (itemid(i)(1:5),1,'depth',5,ihulp)
          if ( ihulp .eq. 1 ) itemde(i) = -999.
          call zoek (itemid(i)(1:4),1,'delt',4,ihulp)
          if ( ihulp .eq. 1 ) itemde(i) = -999.
          call zoek (itemid(i)(1:10),1,'totaldepth',10,ihulp)
          if ( ihulp .eq. 1 ) itemde(i) = -999.

          if (duprol) then

c         Remove defaults for DUFLOW hydro parameters, so that conversion process will be activated
              call zoek (itemid(i)(1:5),1,'Z    ',5,ihulp)
              if ( ihulp .eq. 1 ) itemde(i) = -999.
              call zoek (itemid(i)(1:5),1,'Q    ',5,ihulp)
              if ( ihulp .eq. 1 ) itemde(i) = -999.
              call zoek (itemid(i)(1:5),1,'As   ',5,ihulp)
              if ( ihulp .eq. 1 ) itemde(i) = -999.
              call zoek (itemid(i)(1:5),1,'dt   ',5,ihulp)
              if ( ihulp .eq. 1 ) itemde(i) = -999.
              call zoek (itemid(i)(1:5),1,'dx   ',5,ihulp)
              if ( ihulp .eq. 1 ) itemde(i) = -999.
              call zoek (itemid(i)(1:5),1,'V    ',5,ihulp)

              if ( ihulp .eq. 1 ) itemde(i) = -999.
              call zoek (itemid(i)(1:5),1,'Wf   ',5,ihulp)
              if ( ihulp .eq. 1 ) itemde(i) = -999.
              call zoek (itemid(i)(1:5),1,'Wd   ',5,ihulp)
              if ( ihulp .eq. 1 ) itemde(i) = -999.
              call zoek (itemid(i)(1:5),1,'Chezy',5,ihulp)
              if ( ihulp .eq. 1 ) itemde(i) = -999.
              call zoek (itemid(i)(1:5),1,'ITIME',5,ihulp)
              if ( ihulp .eq. 1 ) itemde(i) = -999.
          endif
  120 continue

      endif

c----------------------------------------------------------------------c
c     Dump tables
c----------------------------------------------------------------------c

      call writdb ( 15 )

c----------------------------------------------------------------------c
c     Write prefined set for SOBEK (DUPROL mode ONLY)
c----------------------------------------------------------------------c

      if (duprol) then
          i = index (pdffil,'.')
	    if (i.le.1) then
            procesnaam = pdffil
          else
            procesnaam = pdffil(1:i-1)
          endif
          pdffil = trim(procesnaam)//'.0'
	    open (15,file=pdffil)
	    write (15,1000) 
      	write (15,'(''this file is not available for a DUFLOW model '')')
          close (15)
          pdffil = trim(procesnaam)//'.des'
	    open (15,file=pdffil)
	    write (15,'(''DUFLOW model '',a)') trim(procesnaam)
	    close (15)
          pdffil = trim(procesnaam)//'.sub'
	    open (15,file=pdffil)
	    call wrisub(15)
	    close (15)
      endif

 1000 format ('configuration ''DUPROL'' serial 2011010101') 
 1010 format ('2011010101') 

      close (io_mes)

      stop 'Normal end'
  999 stop 'PROCES.ASC does not exist!!!!!!!!!'
 2001 format (a10,f18.0,a1,1x,a50)
 2002 format (a10,18x,  a1,1x,a50)
 3001 format (a10,f18.0,a1,1x,a50,5x,a20)
 3002 format (a10,18x,  a1,1x,a50,5x,a20)
      end

      subroutine iniind()
      include 'data.inc'

      integer    iitem, ifort , iproc , iinpu , ioutp , ioutf , istoc ,
     j           ivelo, idisp

c     Initialise indexes arrays

      do 10 iitem=1,nitemm
   10 item_i(iitem) = iitem
      do 20 ifort=1,nfortm
   20 fort_i(ifort) = ifort
      do 30 iproc=1,nprocm
   30 proc_i(iproc) = iproc
      do 40 iinpu=1,ninpum
   40 inpu_i(iinpu) = iinpu
      do 50 ioutp=1,noutpm
   50 outp_i(ioutp) = ioutp
      do 60 ioutf=1,noutfm
   60 outf_i(ioutf) = ioutf
      do 70 istoc=1,nstocm
   70 stoc_i(istoc) = istoc
      do 80 ivelo=1,nvelom
   80 velo_i(ivelo) = ivelo
      do 90 idisp=1,ndispm
   90 disp_i(idisp) = idisp
      return
      end

      subroutine cratab (grp,newtab,initialConfgId,initialConfgName)

      character*30 grp
      character*10 initialConfgId
      character*50 initialConfgName
      include 'data.inc'
      integer iitem , iproc, icnpr, iconf
      logical newtab

      if ( newtab ) then

c         NEW TABLES

c         Dummy versions of tables P1 and P5
          nsgrp = 1
          sgrpid(1) = grp
          sgrpnm(1) = grp
          nconf = 1
          confid(1) = initialConfgId
          confnm(1) = initialConfgName

c         Table R1
c         include all processes in Dummy configuration

          do 5 iproc = 1,nproc
              conpro(1,iproc) = .true.
    5     continue

c         Table R2
c         add all substances to Dummy configuration

          ncnsb = 0
          do 10 iitem = 1,nitem
              if ( itemgr(iitem) .eq. grp ) then
c                 This must be a substance
                  ncnsb = ncnsb + 1
                  r2_cid(ncnsb) = confid(1)
                  r2_sid(ncnsb) = itemid(iitem)
              endif
   10     continue

      else

c         UPDATE TABLES

c         Recreate Table R1

          do 15 iproc = 1,nproc
          do 15 iconf = 1,nconf
   15     conpro(iconf,iproc) = .false.
          do 20 icnpr = 1,ncnpr
              call zoek (r1_pid(icnpr),nproc,procid,10,iproc)
              call zoek (r1_cid(icnpr),nconf,confid,10,iconf)
              if (iconf.le.0) stop 'BUG CRATAB'
              if (iproc.gt.0) conpro(iconf,iproc) = .true.
   20     continue

c         Table R2
c         add all new substances to Dummy configuration
c         NO EFFORT DONE TO CLEAR OLD ENTRIES

          do 40 iitem = 1,nitem
              if ( itemgr(iitem) .eq. grp ) then
c                 This must be a NEW substance
                  if ( ncnsb + 1 .gt. ncnsbm ) stop 'DIMENSION NCNSBM'
                  ncnsb = ncnsb + 1
                  r2_cid(ncnsb) = 'DummyConfg'
                  r2_sid(ncnsb) = itemid(iitem)
              endif
   40     continue

      endif
      return
      end

