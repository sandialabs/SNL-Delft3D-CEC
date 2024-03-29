      subroutine wridoc ( procid, procnm, procco, procfo, lun )
      character*50 procnm
      character*10 procid, procfo
      integer      procco, lun   , i
      include 'pdf.inc'

      write ( lun , 1000 ) procid,procnm
      write ( lun , 1010 ) procfo
      write ( lun , 1020 ) procco
 1000 format (//'========================================',
     j          '========================================'/
     j          'PROCESS: ',a10,' (',a50,')'/
     j          '========================================',
     j          '========================================')
 1010 format (  'Documented under : ',a10)
 1020 format (  'Transport code   : ',i3)

      if ( ins .gt. 0 ) then
          write ( lun , 1030 )
          write ( lun , 1040 )(i,ins_id(i),ins_va(i),ins_nm(i),i=1,ins)
      endif
 1030 format (/'OVERVIEW OF INPUT ITEMS ON SEGMENT LEVEL:'/
     j         'Nr  Id          Default     Description'/
     j         '--- ----------  ----------  -----------')
 1040 format (i3,1x,a10,2x,g10.3,2x,a50)

      if ( ine .gt. 0 ) then
          write ( lun , 1050 )
          write ( lun , 1040 )(i,ine_id(i),ine_va(i),ine_nm(i),i=1,ine)
      endif
 1050 format (/'OVERVIEW OF INPUT ITEMS ON EXCHANGE LEVEL:'/
     j         'Nr  Id          Default     Description'/
     j         '--- ----------  ----------  -----------')

      if ( ous .gt. 0 ) then
          write ( lun , 1060 )
          write ( lun , 1070 )(i,ous_id(i),ous_nm(i),i=1,ous)
      endif
 1060 format (/'OVERVIEW OF OUTPUT ITEMS ON SEGMENT LEVEL:'/
     j         'Nr  Id          Description'/
     j         '--- ----------  -----------')
 1070 format (i3,1x,a10,2x,a50)

      if ( oue .gt. 0 ) then
          write ( lun , 1080 )
          write ( lun , 1070 )(i,oue_id(i),oue_nm(i),i=1,oue)
      endif
 1080 format (/'OVERVIEW OF OUTPUT ITEMS ON EXCHANGE LEVEL:'/
     j         'Nr  Id          Description'/
     j         '--- ----------  -----------')

      if ( flu .gt. 0 ) then
          write ( lun , 1090 )
          write ( lun , 1070 )(i,flu_id(i),flu_nm(i),i=1,flu)
      endif
 1090 format (/'OVERVIEW OF COMPUTED FLUXES:'/
     j         'Nr  Id          Description'/
     j         '--- ----------  -----------')

      if ( sto .gt. 0 ) then
          write ( lun , 1100 )
          write ( lun , 1110 )(sto_fl(i),sto_su(i),sto_sc(i),i=1,sto)
      endif
 1100 format (/'INTERACTIONS BETWEEN FLUXES AND SUBSTANCES:'/
     j         'Flux        Substance   Scale factor'/
     j         '----------  ----------  ------------')
 1110 format (a10,2x,a10,2x,f10.5)

      if ( dis .gt. 0 ) then
          write ( lun , 1120 )
          write ( lun , 1110 )(dis_it(i),dis_su(i),dis_sc(i),i=1,dis)
      endif
 1120 format (/'DISPERSION VECTORS ON SUBSTANCES:'/
     j         'Flux        Substance   Scale factor'/
     j         '----------  ----------  ------------')

      if ( vel .gt. 0 ) then
          write ( lun , 1130 )
          write ( lun , 1110 )(vel_it(i),vel_su(i),vel_sc(i),i=1,vel)
      endif
 1130 format (/'VELOCITY VECTORS ON SUBSTANCES:'/
     j         'Flux        Substance   Scale factor'/
     j         '----------  ----------  ------------')

      write ( lun , 1140 ) procid
 1140 format ( /'========================================',
     j          '========================================'/
     j          'END OF PROCESS ',a10/
     j          '========================================',
     j          '========================================')

      return
      end
