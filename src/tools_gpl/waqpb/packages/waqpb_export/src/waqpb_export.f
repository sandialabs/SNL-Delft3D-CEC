c
c     Program to compose a PROCES.ASC file from tables
c
c     This program consists of the following parts:
c     - Reading of tables holding PROCLIB data structure
c     - Loop over the processes:
c       - Empty PDF structure
c       - Construct and write PDF structure
c       - Dump structure to PROCES.ASC

c     Include data structures for tables and PDF-file
      include 'data.inc'
      include 'pdf.inc'
      integer      jndex , iproc , iinpu , iitem , ioutp , idisp ,
     j             ioutf , isubs , naanta, ioffse, ioffs2, ivelo ,
     j             istoc , iconf , naant2, serial,
     j             ierror, icnsb , imodv , i
      logical      itmswi(nitemm)
      logical      newfrm
      character*10 c10   
      character*20 c20
      character*50 adduni
      character*255 ArgumentString
      real         actdef, versio
      integer      lu_inp, lu_mes, status
      data         lu_inp /14/
      data         lu_mes /11/
      
c     Defaults for command line arguments

      versio = 5.00
      serial = 20130101
      newfrm = .false.

      do i=1,9999
            call getarg (i,ArgumentString)
            if (ArgumentString.eq.'') exit
            if (index(ArgumentString,'-version').gt.0) then
                c20 = ArgumentString(9:28)
                read (c20,'(f20.0)',iostat=status) versio
            endif
            if (index(ArgumentString,'-serial').gt.0) then
                c20 = ArgumentString(8:27)
                read (c20,'(i20)',iostat=status) serial
            endif
            if (index(ArgumentString,'-newfrm').gt.0) newfrm = .true.
      enddo


      itmswi = .false.
      open ( lu_mes , file = 'waqpb_export.log' )
      if (newfrm) then
        write (lu_mes,'(''Using NEW format'')')
	else
        write (lu_mes,'(''Using OLD format'')')
	endif

      write (*,'('' Reading data......'')')

c----------------------------------------------------------------------c
c     READ DATABASE
c----------------------------------------------------------------------c

      call readdb ( lu_inp, lu_mes )

c     Check validity of table R9

      do 10 imodv = 1,nmodv
          call zoek (modvci(imodv),nconf,confid,10,iconf)
          if ( iconf .le. 0 )
     j        write ( lu_mes , '(''Unknown config in TABLE5: '',a10,1x,
     j                            a10)') modvci(imodv),modvit(imodv)
          call zoek (modvit(imodv),nitem,itemid,10,iitem)
          if ( iitem .le. 0 )
     j        write ( lu_mes , '(''Unknown item in TABLE5: '',a10,1x,
     j                            a10)') modvci(imodv),modvit(imodv)
   10 continue

c     Create auxiliary table of substances

      nsubs = 0
      do icnsb = 1,ncnsb
          c10 = r2_sid(icnsb)

c         Lookup substance in item array
          call zoek (c10,nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) then
              write (*,*) ' ITEM: ',c10
              STOP 'Unknown substance in R2 table'
          endif

c         Add to substances array
          call zoek (c10,nsubs,subsid,10,isubs)
          if ( isubs .le. 0 ) then
              if ( nsubs+1 .gt. nsubsm ) STOP 'Dimension NSUBSM'
              nsubs = nsubs+1
              subsid(nsubs) = c10
          endif
      enddo

c     Dump TRM tables

c      write (*,'('' Writing TRM tables......'')')
c      call writrm
      write (*,'('' Writing TRM tables for LaTeX......'')')
      call writex
      
c----------------------------------------------------------------------c
c     SET VERSION, SERIAL AND WRITE NEFIS FILE
c----------------------------------------------------------------------c

      write (11,'(''Writing NEFIS process definition file'')')
      call makind()
      call pdfnef(11    , serial, versio, ierror)
      if ( ierror .ne. 0 ) then
         write (11,'(''ERROR writing NEFIS file'')')
         write (*,'(''ERROR writing NEFIS file, see report file'')')
      endif

c----------------------------------------------------------------------c
c     LOOP OVER PROCESSES
c----------------------------------------------------------------------c

      write (*,'('' Making PROCES.ASC......'')')
      write (*,*)
      open ( 15 , file = 'procesm.asc' )
      write ( 15 , '(i10,50x,f8.2,2x,i10)' ) nproc,versio,serial

      do 800 iproc=1,nproc

          write (*,'(''+Process: '',a10)') procid(iproc)

c----------------------------------------------------------------------c
c         CONSTRUCT PROCESS
c----------------------------------------------------------------------c

c         Clear PDF structure

          ins = 0
          ine = 0
          ous = 0
          oue = 0
          flu = 0
          sto = 0
          dis = 0
          vel = 0

c         Fill PDF structure

C         INPUT ITEMS ON SEGMENT LEVEL/EXCHANGE LEVEL

c         scan input items table for FIRST occurence of proces
          call zoek ( procid(iproc), ninpu, inpupr, 10, ioffse )
          naanta = 0
          if ( ioffse .gt. 0 ) then

c             loop over all INPU rows related to this process

  410         continue
              naanta = naanta + 1

c             Process current row

c             Lookup item in items table
              iinpu = ioffse + naanta-1
              call zoek ( inpuit(iinpu), nitem, itemid, 10, iitem)
              if ( iitem .le. 0 ) stop 'unknown ITEM'

c             Documented items are marked for COEFEDIT.DAT
              if ( inpudo(iinpu) .eq. 'x' ) itmswi(iitem) = .true.

c             Find item properties and store in PDF structure
              if ( inpude(iinpu) .eq. 'Y' ) then
                  actdef = itemde(iitem)
              elseif ( inpude(iinpu) .eq. 'G' ) then
                  actdef = -888.
              elseif ( inpude(iinpu) .eq. 'B' ) then
                  actdef = -101.
              elseif ( inpude(iinpu) .eq. 'M' ) then
                  actdef = -11.
              elseif ( inpude(iinpu) .eq. 'O' ) then
                  actdef = -1.
              else
                  actdef = -999.
              endif
              if ( inpusx(iinpu) .eq. 1 ) then
                  ins = ins + 1
                  if ( ins .gt. insmax ) stop 'DIMENSION insmax'
                  ins_id(ins) = itemid(iitem)
                  if (newfrm) then
                  ins_nm(ins) = itemnm(iitem)
                  ins_un(ins) = itemun(iitem)
                  else
                  ins_nm(ins) = adduni(itemnm(iitem),itemun(iitem))
                  endif
                  ins_va(ins) = actdef
                  ins_do(ins) = inpudo(iinpu)
              else
                  ine = ine + 1
                  if ( ine .gt. inemax ) stop 'DIMENSION inemax'
                  ine_id(ine) = itemid(iitem)
                  if (newfrm) then
                  ine_nm(ine) = itemnm(iitem)
                  ine_un(ine) = itemun(iitem)
                  else
                  ine_nm(ine) = adduni(itemnm(iitem),itemun(iitem))
                  endif
                  ine_va(ine) = actdef
                  ine_do(ine) = inpudo(iinpu)
              endif

c             Back for next row in table INPU,
c             if it still matches current proces

              if ( (iinpu+1) .le. ninpu ) then
                  call zoek ( procid(iproc), 1, inpupr(iinpu+1),
     j                  10, jndex )
                  if ( jndex .gt. 0 ) goto 410
              endif
          endif

C         OUTPUT ITEMS ON SEGMENT LEVEL/EXCHANGE LEVEL

c         scan output items table for FIRST occurence of proces
          call zoek ( procid(iproc), noutp, outppr, 10, ioffse )
          naanta = 0
          if ( ioffse .gt. 0 ) then

c             loop over all OUTP rows related to this process

  440         continue
              naanta = naanta + 1

c             Process current row

c             Lookup item in items table
              ioutp = ioffse + naanta-1
              call zoek ( outpit(ioutp), nitem, itemid, 10, iitem)
              if ( iitem .le. 0 ) stop 'unknown ITEM'

c             Find item properties and store in PDF structure
              if ( outpsx(ioutp) .eq. 1 ) then
                  ous = ous + 1
                  if ( ous .gt. ousmax ) stop 'DIMENSION ousmax'
                  ous_id(ous) = itemid(iitem)
                  if (newfrm) then
                  ous_nm(ous) = itemnm(iitem)
                  ous_un(ous) = itemun(iitem)
                  else
                  ous_nm(ous) = adduni(itemnm(iitem),itemun(iitem))
                  endif
                  ous_do(ous) = outpdo(ioutp)
              else
                  oue = oue + 1
                  if ( oue .gt. ouemax ) stop 'DIMENSION ouemax'
                  oue_id(oue) = itemid(iitem)
                  if (newfrm) then
                  oue_nm(oue) = itemnm(iitem)
                  oue_un(oue) = itemun(iitem)
                  else
                  oue_nm(oue) = adduni(itemnm(iitem),itemun(iitem))
                  endif
                  oue_do(oue) = outpdo(ioutp)

c                 SCAN VELO and DISP TABLES FOR LINES ASSOCIATED WITH
c                 CURRENT OUTPUT ITEM ON EXCHANGE LEVEL

c                 scan dispersion lines table for FIRST occurence of item
                  call zoek ( itemid(iitem), ndisp, dispit, 10, ioffs2)
                  naant2 = 0
                  if ( ioffs2 .gt. 0 ) then

c                     loop over all DISP rows related to this item

  450                 continue
                      naant2 = naant2+1
                      dis = dis + 1
                      if ( dis .gt. dismax ) stop 'dimension DISMAX'

c                     Process current row

                      idisp = ioffs2 + naant2-1
                      dis_su(dis) = dispsu(idisp)
                      dis_it(dis) = dispit(idisp)
                      dis_sc(dis) = dispsc(idisp)

c                     Back for next row in table DISP,
c                     if it still matches current item

                      if ( (idisp+1) .le. ndisp ) then
                          call zoek ( itemid(iitem), 1, dispit(idisp+1),
     j                          10, jndex )
                          if ( jndex .gt. 0 ) goto 450
                      endif
                  endif

c                 scan velocity lines table for FIRST occurence of item
                  call zoek ( itemid(iitem), nvelo, veloit, 10, ioffs2)
                  naant2 = 0
                  if ( ioffs2 .gt. 0 ) then

c                     loop over all VELO rows related to this item

  460                 continue
                      naant2 = naant2+1
                      vel = vel + 1
                      if ( vel .gt. velmax ) stop 'dimension VELMAX'

c                     Process current row

                      ivelo = ioffs2 + naant2-1
                      vel_su(vel) = velosu(ivelo)
                      vel_it(vel) = veloit(ivelo)
                      vel_sc(vel) = velosc(ivelo)

c                     Back for next row in table VELO,
c                     if it still matches current item

                      if ( (ivelo+1) .le. nvelo ) then
                          call zoek ( itemid(iitem), 1, veloit(ivelo+1),
     j                          10, jndex )
                          if ( jndex .gt. 0 ) goto 460
                      endif
                  endif

c                 END of processing output item on exchange level!

              endif

c             Back for next row in table OUTP,
c             if it still matches current proces

              if ( (ioutp+1) .le. noutp ) then
                  call zoek ( procid(iproc), 1, outppr(ioutp+1),
     j                  10, jndex )
                  if ( jndex .gt. 0 ) goto 440
              endif
          endif

C         FLUXES

c         scan output fluxes table for FIRST occurence of proces
          call zoek ( procid(iproc), noutf, outfpr, 10, ioffse )
          if ( ioffse .gt. 0 ) then

c             loop over all FLUX rows related to this process

  470         continue
              flu = flu + 1
              if ( flu .gt. flumax ) stop 'dimension FLUMAX'

c             Process current row

c             Lookup flux in items table
              ioutf = ioffse + flu-1
c             write (11,*) ' flu ',flu,' ioutf ', ioutf
              call zoek ( outffl(ioutf), nitem, itemid, 10, iitem)
              if ( iitem .le. 0 ) stop 'unknown FLUX'

c             Find and store flux properties
              flu_id(flu) = itemid(iitem)
              if (newfrm) then
              flu_nm(flu) = itemnm(iitem)
              flu_un(flu) = itemun(iitem)
              else
              flu_nm(flu) = adduni(itemnm(iitem),itemun(iitem))
              endif
              flu_do(flu) = outfdo(ioutf)

c             SCAN STOCHI TABLE FOR LINES ASSOCIATED WITH PRESENT FLUX

c             scan stochi lines table for FIRST occurence of flux
              call zoek ( itemid(iitem), nstoc, stocfl, 10, ioffs2)
              naant2 = 0
              if ( ioffs2 .gt. 0 ) then

c                 loop over all STOC rows related to this flux

  480             continue
                  naant2 = naant2+1
                  sto = sto + 1
                  if ( sto .gt. stomax ) stop 'dimension STOMAX'

c                 Process current row

                  istoc = ioffs2 + naant2-1
                  sto_su(sto) = stocsu(istoc)
                  sto_fl(sto) = stocfl(istoc)
                  sto_sc(sto) = stocsc(istoc)

c                 Back for next row in table STOC,
c                 if it still matches current flux

                  if ( (istoc+1) .le. nstoc ) then
                      call zoek ( itemid(iitem), 1, stocfl(istoc+1),
     j                      10, jndex )
                      if ( jndex .gt. 0 ) goto 480
                  endif
              endif

c             Back for next row in table OUTF,
c             if it still matches current proces

              if ( (ioutf+1) .le. noutf ) then
                  call zoek ( procid(iproc), 1, outfpr(ioutf+1),
     j                  10, jndex )
                  if ( jndex .gt. 0 ) goto 470
              endif
          endif

c----------------------------------------------------------------------c
c         WRITE PROCESS
c----------------------------------------------------------------------c

c         Write PDF file (formats as in HARMONIZE to allow comparison)

          if (newfrm) then
          call wripdn ( procid(iproc), procnm(iproc), procco(iproc),
     j                  procfo(iproc), 15 )
          else
          call wripdf ( procid(iproc), procnm(iproc), procco(iproc),
     j                  procfo(iproc), 15 )
          endif
  800 continue
      close (15)

c     Write all active coefficients to COEFEDIT.DAT in the Sobek-format
      call coefed(serial,itmswi)

  900 continue
      close (11)

      stop 'Normal end'
      end
      function adduni(name,unit)
      character*50 adduni, name
      character*20 unit

      integer      lennam, lenuni, i

c     find length of name and unit

      lennam = -1
      do 10 i = 50,1,-1
          if ( name(i:i) .ne. ' ' ) then
              lennam = i
              goto 11
          endif
   10 continue
   11 continue
      if ( lennam .le. 1 ) then
          write (*,*) ' ',name
          stop 'ADDUNI Fatal Error 1'
      endif

      lenuni = 0
      if ( unit(2:3) .eq. 'no' .and.
     j     unit(5:8) .eq. 'unit' ) then
          lenuni = 0
      else
          do 20 i = 20,1,-1
              if ( unit(i:i) .ne. ' ' ) then
                  lenuni = i
                  goto 21
              endif
   20     continue
   21     continue
      endif
      if ( lenuni .lt. 0 ) then
          write (*,*) ' ',unit
          stop 'ADDUNI Fatal Error 1'
      endif

      if ( lennam + lenuni .gt. 50 ) then
          lennam = 50-lenuni
      endif

      write ( adduni(1          :lennam) , '(a)' ) name(1:lennam)
      do 30 i = 1,50-lennam-lenuni
   30 adduni(lennam+i:lennam+i) = ' '
      if (lenuni.gt.0)
     j write ( adduni(50-lenuni+1:50    ) , '(a)' ) unit(1:lenuni)

      return
      end
