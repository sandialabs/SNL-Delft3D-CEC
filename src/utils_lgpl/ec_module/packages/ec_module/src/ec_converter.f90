!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.            
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     

!  $Id: ec_converter.f90 5640 2015-12-10 09:24:34Z hummel $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/ec_module/packages/ec_module/src/ec_converter.f90 $

!> This module contains all the methods for the datatype tEcConverter.
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.spee@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_converter
   use m_ec_typedefs
   use m_ec_message
   use mathconsts
   use m_ec_support
   use m_ec_alloc
   use m_ec_magic_number
   use m_ec_parameters

   implicit none
   
   private 
   
   public :: ecConverterCreate
   public :: ecConverterFree1dArray
   public :: ecConverterSetType
   public :: ecConverterSetOperand
   public :: ecConverterUpdateWeightFactors
   public :: ecConverterPerformConversions
   public :: ecConverterSetElement
   public :: ecConverterSetInterpolation
   public :: ecConverterSetMask
   
   contains
      
      ! =======================================================================
      
      !> Construct a new Converter with the specified id.
      !! Failure is indicated by returning a null pointer.
      function ecConverterCreate(converterId) result(converterPtr)
         type(tEcConverter), pointer            :: converterPtr !< the new Converter, intent(out)
         integer,                    intent(in) :: converterId  !< unique Converter id
         !
         integer :: istat !< allocate() status
         !
         ! allocation
         allocate(converterPtr, stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_converter::ecConverterCreate: Unable to allocate additional memory.")
            converterPtr => null()
            return
         end if
         ! initialization
         converterPtr%id = converterId
         converterPtr%ofType = convType_undefined
         converterPtr%operandType = operand_undefined
         converterPtr%interpolationType = interpolate_unknown
         converterPtr%targetIndex = ec_undef_int
      end function ecConverterCreate
      
      ! =======================================================================

      !> Free a tEcConverter, after which it can be deallocated.
      function ecConverterFree(converter) result (success)
         logical                           :: success   !< function status
         type(tEcConverter), intent(inout) :: converter !< tEcConverter to free
         !
         integer :: istat !< deallocate() status
         !
         success = .true.
         !
         ! The Converter owns the dynamically allocated memory, so deallocate.
         if (associated(converter%indexWeight)) then
            if (associated(converter%indexWeight%indices)) then
               deallocate(converter%indexWeight%indices, stat = istat)
               if (istat /= 0) success = .false.
            end if
            if (associated(converter%indexWeight%weightFactors)) then
               deallocate(converter%indexWeight%weightFactors, stat = istat)
               if (istat /= 0) success = .false.
            end if
            deallocate(converter%indexWeight, stat = istat)
            if (istat /= 0) success = .false.
         end if
      end function ecConverterFree
      
      ! =======================================================================
      
      !> Frees a 1D array of tEcConverterPtrs, after which the converterPtr is deallocated.
      function ecConverterFree1dArray(converterPtr, nConverters) result (success)
         logical                                      :: success      !< function status
         type(tEcConverterPtr), dimension(:), pointer :: converterPtr !< intent(inout)
         integer                                      :: nConverters  !< number of Converters
         !
         integer :: i      !< loop counter
         integer :: istat  !< deallocate() status
         !
         success = .true.
         !
         if (.not. associated(converterPtr)) then
            call setECMessage("WARNING: ec_converter::ecConverterFree1dArray: Dummy argument converterPtr is already disassociated.")
         else
            ! Free and deallocate all tEcConverterPtrs in the 1d array.
            do i=1, nConverters
               if (ecConverterFree(converterPtr(i)%ptr)) then
                  deallocate(converterPtr(i)%ptr, stat = istat)
                  if (istat /= 0) success = .false.
               else
                  success = .false.
               end if
            end do
            ! Finally deallocate the tEcConverterPtr(:) pointer.
            if (success) then
               deallocate(converterPtr, stat = istat)
               if (istat /= 0) success = .false.
            end if
         end if
      end function ecConverterFree1dArray
      
      ! =======================================================================
      ! Set methods
      ! =======================================================================
      
      !> Attach a mask instance to the converter masking source points (used in case of arcinfo data)
      function ecConverterSetMask(instancePtr, converterId, srcmask) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: converterId !< unique Converter id
         type(tEcMask),             intent(in) :: srcmask     !< new type of the Converter
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%srcmask = srcmask
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetMask: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetMask
      
      ! =======================================================================

      !> Change the type of the Converter corresponding to converterId.
      function ecConverterSetType(instancePtr, converterId, ofType) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: converterId !< unique Converter id
         integer,                   intent(in) :: ofType      !< new type of the Converter
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%ofType = ofType
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetType: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetType
      
      ! =======================================================================
      
      !> Change the operand of the Converter corresponding to converterId.
      function ecConverterSetOperand(instancePtr, converterId, operand) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: converterId !< unique Converter id
         integer,                   intent(in) :: operand     !< new operand of the Converter
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%operandType = operand
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetOperand: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetOperand
      
      ! =======================================================================
      
      !> Change the interpolation type of the Converter corresponding to converterId.
      function ecConverterSetInterpolation(instancePtr, converterId, interpolationType) result(success)
         logical                               :: success           !< function status
         type(tEcInstance), pointer            :: instancePtr       !< intent(in)
         integer,                   intent(in) :: converterId       !< unique Converter id
         integer,                   intent(in) :: interpolationType !< new interpolation type of the Converter
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%interpolationType = interpolationType
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetInterpolation: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetInterpolation
      
      ! =======================================================================
      
      !> Set the array index of the target Item's field to write to of the Converter corresponding to converterId.
      function ecConverterSetElement(instancePtr, converterId, indx) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: converterId !< unique Converter id
         integer,                   intent(in) :: indx        !< array index in target item array where to put the converted data. NOTE: call site should already account for the vectormax/n_data in this index.
         !
         type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
         !
         success = .false.
         converterPtr => null()
         !
         converterPtr => ecSupportFindConverter(instancePtr, converterId)
         if (associated(converterPtr)) then
            converterPtr%targetIndex = indx
            success = .true.
         else
            call setECMessage("ERROR: ec_converter::ecConverterSetElement: Cannot find a Converter with the supplied id.")
         end if
      end function ecConverterSetElement
      
      ! =======================================================================
      
      !> Update the weight factors of a Converter.
      function ecConverterUpdateWeightFactors(instancePtr, connection) result (success)
         logical                            :: success     !< function status
         type(tEcInstance), pointer         :: instancePtr !< intent(inout)
         type(tEcConnection), intent(inout) :: connection  !< access to Converter and Items
         !
         type(tEcIndexWeight), pointer :: weight           !< the new IndexWeight
         type(tEcElementSet),  pointer :: sourceElementSet !< source ElementSet
         type(tEcElementSet),  pointer :: targetElementSet !< target ElementSet
         integer  :: i !< loop counter
         integer  :: n_cols, n_rows, n_points
         integer  :: inside, mp, np, in, jn !< return values of findnm
         real(hp) :: wf(4) !< return value of findnm
         integer  :: fmask(4) !< return value of findnm
         real(hp) :: fsum 
         type(tEcMask), pointer :: srcmask 
         !
         success = .false.
         sourceElementSet => null()
         !
         ! Safety check while interpolate_spacetimeSaveWeightFactors is still liberally used.
         if (.not. (connection%converterPtr%ofType == convType_curvi .or. &
                    connection%converterPtr%ofType == convType_polytim .or. &
                    connection%converterPtr%ofType == convType_netcdf)) then
            success = .true.
            return
         end if
         !
         ! Check whether there is anything to be done.
         if (connection%converterPtr%interpolationType == interpolate_spacetimeSaveWeightFactors) then
            if (associated(connection%converterPtr%indexWeight)) then
               success = .true.
               return
            end if
         end if
         !
         ! Check whether the Converter's weights need to be allocated.
         if (.not. associated(connection%converterPtr%indexWeight)) then
            allocate(weight)
            connection%converterPtr%indexWeight => weight
         else
            weight => connection%converterPtr%indexWeight
         end if
         !
         ! Calculate and update the Converter's weights.
         srcmask => connection%converterPtr%srcmask
         select case(connection%converterPtr%ofType)
            case(convType_curvi, convType_netcdf)
               ! bilinear interpolation
               sourceElementSet => connection%sourceItemsPtr(1)%ptr%elementSetPtr
               targetElementSet => connection%targetItemsPtr(1)%ptr%elementSetPtr
               n_cols = sourceElementSet%n_cols
               n_rows = sourceElementSet%n_rows
               n_points = targetElementSet%nCoordinates
               if (associated(weight%indices)) deallocate(weight%indices)
               allocate(weight%indices(2, n_points))
               weight%indices = ec_undef_int
               if (associated(weight%weightFactors)) deallocate(weight%weightFactors)
               allocate(weight%weightFactors(4, n_points))
               weight%weightFactors = ec_undef_hp
               do i=1, n_points
                  if (sourceElementSet%ofType == elmSetType_cartesian) then
                     call findnm(targetElementSet%x(i), targetElementSet%y(i), sourceElementSet%x, sourceElementSet%y, &
                              n_cols, n_rows, n_cols, n_rows, inside, mp, np, in, jn, wf)
                  elseif (sourceElementSet%ofType == elmSetType_spheric) then
                     call findnm(targetElementSet%x(i), targetElementSet%y(i), sourceElementSet%lon, sourceElementSet%lat, &
                              n_cols, n_rows, n_cols, n_rows, inside, mp, np, in, jn, wf)
                  endif 
                  if (inside == 1) then
                     if (allocated(srcmask%msk)) then
                        fmask(1) = (srcmask%msk((np   -srcmask%nmin)*srcmask%mrange+mp   -srcmask%mmin+1))
                        fmask(2) = (srcmask%msk((np   -srcmask%nmin)*srcmask%mrange+mp+1 -srcmask%mmin+1))
                        fmask(3) = (srcmask%msk((np+1 -srcmask%nmin)*srcmask%mrange+mp+1 -srcmask%mmin+1))
                        fmask(4) = (srcmask%msk((np+1 -srcmask%nmin)*srcmask%mrange+mp   -srcmask%mmin+1))
                        fsum = sum((1.d0-fmask)*wf)            ! fmask = 1 for DISCARDED corners               
                        if (fsum>=1.0e-03) then         
                           wf = (wf*(1.d0-fmask))/fsum
                        endif
                     endif 
                     weight%weightFactors(1,i) = wf(1)    
                     weight%weightFactors(2,i) = wf(2)    
                     weight%weightFactors(3,i) = wf(3)    
                     weight%weightFactors(4,i) = wf(4)    
                     weight%indices(1,i) = mp
                     weight%indices(2,i) = np
                  endif
               end do
               success = .true.
            case(convType_polytim)
               sourceElementSet => connection%sourceItemsPtr(1)%ptr%elementSetPtr
               targetElementSet => connection%targetItemsPtr(1)%ptr%elementSetPtr
               n_points = targetElementSet%nCoordinates
               if (associated(weight%indices)) deallocate(weight%indices)
               allocate(weight%indices(2, n_points))
               weight%indices = ec_undef_int
               if (associated(weight%weightFactors)) deallocate(weight%weightFactors)
               allocate(weight%weightFactors(2, n_points))
               weight%weightFactors = ec_undef_hp
               do i=1, n_points
                  call polyindexweight(targetElementSet%x(i), &
                                       targetElementSet%y(i), &
                                       targetElementSet%xyen(1,i), &
                                       targetElementSet%xyen(2,i), &
                                       sourceElementSet%x, &
                                       sourceElementSet%y, &
                                       sourceElementSet%mask, &
                                       sourceElementSet%nCoordinates, &
                                       weight%indices(1,i), &       ! kL
                                       weight%weightFactors(1,i), & ! wL
                                       weight%indices(2,i), &       ! kR
                                       weight%weightFactors(2,i))   ! wR
               end do
               success = .true.
            case default
               success = .true.
         end select
      end function ecConverterUpdateWeightFactors
      
      ! =======================================================================

      subroutine findnm(xl, yl, x, y, mmax, nmax, mc, nc, inside, mv, nv, in, jn, wf)
         real(hp),               intent(in)  :: xl, yl !< coordinates of point in target grid
         real(hp), dimension(:), intent(in)  :: x, y !< coordinates of points in source grid
         integer,                intent(in)  :: mmax, nmax, mc, nc !< maximum and actual dimensions
         integer,                intent(out) :: inside, mv, nv, in, jn
         real(hp),               intent(out) :: wf(4)
         !
         real(hp) :: xx(4), yy(4), xk(3), yk(3)
         integer :: ishot, i, j, mz, nz, m1, m2, n1, n2, insidet, i1, i2, ier
         integer :: mvol = 0
         integer :: nvol = 0
         real(hp) :: dx, dy, r, rmin, xxc, yyc
         !
         if (mc == 0 .or. nc == 0) return
         ishot = 0
         rmin  = 99d+20
         !
5        continue
         mv = 0
         nv = 0
         if (mvol /= 0) then
            mz = mvol
            nz = nvol
         else
            ! Loop over all source ElementSet grid points.
            ! FM's rows,columns have been flattened into a 1D array.
            do i = 1,mc
               do j = 1,nc
                  dx = xl - x(i+(j-1)*mc)
                  dy = yl - y(i+(j-1)*mc)
                  r  = dx*dx + dy*dy
                  if (r < rmin) then
                     rmin = r
                     mz   = i
                     nz   = j
                  endif
               end do
            end do
         endif
         !
         m1     = max(1,mz-2)
         n1     = max(1,nz-2)
         m2     = min(mc-1 ,mz+1)
         n2     = min(nc-1 ,nz+1)
         inside = 0
         mvol   = 0
         nvol   = 0
         do i = m1,m2
            do j = n1-1,n2-1
               xx(1) = x(i+(j*mc))
               xx(2) = x(i+1+(j*mc))
               xx(3) = x(i+1+((j+1)*mc))
               xx(4) = x(i+((j+1)*mc))
               yy(1) = y(i+(j*mc))
               yy(2) = y(i+1+(j*mc))
               yy(3) = y(i+1+((j+1)*mc))
               yy(4) = y(i+((j+1)*mc))
               call pinpok(xl, yl, 4, xx, yy, inside)
               if (inside == 1) then
                  call bilin5( xx, yy, xl, yl, wf, ier)
                  mvol = i
                  nvol = j+1
                  mv   = i
                  nv   = j+1
                  ! determine quadrant
                  xxc  = ( xx(1) + xx(2) + xx(3) + xx(4) )/4
                  yyc  = ( yy(1) + yy(2) + yy(3) + yy(4) )/4
                  in   = 0
                  jn   = 0
                  do i1 = 1,4
                     i2    = mod(i1,4) + 1
                     xk(1) = xx(i1)
                     yk(1) = yy(i1)
                     xk(2) = xx(i2)
                     yk(2) = yy(i2)
                     xk(3) = xxc
                     yk(3) = yyc
                     call pinpok(xl, yl, 3, xk, yk, insidet)
                     if (insidet == 1) then
                        if ( i1 == 1) jn = -1
                        if ( i1 == 2) in =  1
                        if ( i1 == 3) jn =  1
                        if ( i1 == 4) in = -1
                        return
                     else if (i1 == 4) then
                        return
                     endif
                  end do
               endif
            end do
         end do
         !
         if (ishot == 1) return
         ishot = 1
         goto 5
      end subroutine findnm   
      
      ! =======================================================================

      subroutine bilin5(xa, ya, x0, y0, w, ier)
         ! Author: H. Petit
         integer               , intent(out) :: ier
         real(hp)              , intent(in)  :: x0
         real(hp)              , intent(in)  :: y0
         real(hp), dimension(4), intent(out) :: w
         real(hp), dimension(4), intent(in)  :: xa
         real(hp), dimension(4), intent(in)  :: ya
         !
         ! Local variables
         !
         real(hp) :: a
         real(hp) :: a21
         real(hp) :: a22
         real(hp) :: a31
         real(hp) :: a32
         real(hp) :: a41
         real(hp) :: a42
         real(hp) :: b
         real(hp) :: c
         real(hp) :: det
         real(hp) :: discr
         real(hp) :: eta
         real(hp) :: x
         real(hp) :: x1
         real(hp) :: x2
         real(hp) :: x3
         real(hp) :: x3t
         real(hp) :: x4
         real(hp) :: xi
         real(hp) :: xt
         real(hp) :: y
         real(hp) :: y1
         real(hp) :: y2
         real(hp) :: y3
         real(hp) :: y3t
         real(hp) :: y4
         real(hp) :: yt
         !
         !! executable statements -------------------------------------------------------
         !
         ! read(12,*)x1,y1,f1
         x1 = xa(1)
         y1 = ya(1)
         ! read(12,*)x2,y2,f2
         x2 = xa(2)
         y2 = ya(2)
         ! read(12,*)x3,y3,f3
         x3 = xa(3)
         y3 = ya(3)
         ! read(12,*)x4,y4,f4
         x4 = xa(4)
         y4 = ya(4)
         x  = x0
         y  = y0
         !
         ! The bilinear interpolation problem is first transformed
         ! to the quadrangle with nodes (0,0),(1,0),(x3t,y3t),(0,1)
         ! and required location (xt,yt)
         !
         a21 = x2 - x1
         a22 = y2 - y1
         a31 = x3 - x1
         a32 = y3 - y1
         a41 = x4 - x1
         a42 = y4 - y1
         det = a21*a42 - a22*a41
         if (abs(det) < 1.0e-20_hp) then
            ier = 1
            goto 99999
         endif
         x3t = (  a42*a31      - a41*a32     ) / det
         y3t = ( -a22*a31      + a21*a32     ) / det
         xt  = (  a42*(x - x1) - a41*(y - y1)) / det
         yt  = ( -a22*(x - x1) + a21*(y - y1)) / det
         if ((x3t < 0.0_hp) .or. (y3t < 0.0_hp)) then
            ! write (*, *) 'distorted quadrangle'
            ier = 1
            goto 99999
         endif
         if (abs(x3t - 1.0_hp) < 1.0e-7_hp) then
            xi = xt
            if (abs(y3t - 1.0_hp) < 1.0e-7_hp) then
               eta = yt
            elseif (abs(1.0_hp + (y3t - 1.0_hp)*xt) < 1.0e-6_hp) then
               ! write (*, *) 'extrapolation over too large a distance'
               ier = 1
               goto 99999
            else
               eta = yt / (1.0_hp + (y3t - 1.0_hp)*xt)
            endif
         elseif (abs(y3t - 1.0_hp) < 1.0e-6_hp) then
            eta = yt
            if (abs(1.0_hp + (x3t - 1.0_hp)*yt) < 1.0e-6_hp) then
               ! write (*, *) 'extrapolation over too large a distance'
               ier = 1
               goto 99999
            else
               xi = xt / (1.0_hp + (x3t - 1.0_hp)*yt)
            endif
         else
            a     = y3t - 1.0_hp
            b     = 1.0_hp + (x3t - 1.0_hp)*yt - (y3t - 1.0_hp)*xt
            c     = -xt
            discr = b*b - 4.0_hp*a*c
            if (discr < 1.0e-6_hp) then
               ! write (*, *) 'extrapolation over too large a distance'
               ier = 1
               goto 99999
            endif
            xi  = (-b + sqrt(discr)) / (2.0_hp*a)
            eta = ((y3t - 1.0_hp)*(xi - xt) + (x3t - 1.0_hp)*yt) / (x3t - 1.0_hp)
         endif
         w(1) = (1.0_hp-xi) * (1.0_hp-eta)
         w(2) =         xi  * (1.0_hp-eta)
         w(3) =         xi  *         eta 
         w(4) =        eta  * (1.0_hp-xi ) 
         return
99999    continue
      end subroutine bilin5

      ! =======================================================================

      subroutine pinpok(xl, yl, n, x, y, inside)
         ! Author: H. Kernkamp
         double precision              , intent(in)  :: xl, yl           ! point under consideration 
         integer                       , intent(in)  :: n
         double precision, dimension(n), intent(in)  :: x, y             ! polygon(n)
         integer                      , intent(out)  :: inside

         double precision, parameter      :: dmiss_default  = -999.0_hp       ! Default missing value in meteo arrays
         integer  :: i, i1, i2, np, rechts
         double precision :: rl, rm, x1, x2, y1, y2

         if (n .le. 2) then
            inside = 1
         else
            np = 0
 5          continue
            np = np + 1
            if (np .le. n) then
               if ( x(np) .ne. dmiss_default) goto 5
            endif
            np = np - 1
            inside = 0
            rechts = 0
            i = 0
10          continue
            i1 = mod(i,np) + 1
            i2 = mod(i1,np) + 1
            x1 = x(i1)
            x2 = x(i2)
            y1 = y(i1)
            y2 = y(i2)
            if (xl .ge. min(x1,x2) .and. xl .le. max(x1,x2) ) then
               if (xl .eq. x1 .and. yl .eq. y1 .or. &                     ! tussen of op lijnstuk
                  (x1 .eq. x2 .and. &                                     ! op punt 1
                   yl .ge. min(y1,y2) .and. yl .le. max(y1,y2) ) .or. &   ! op verticale lijn
                  (yl .eq. y1 .and. y1 .eq. y2)  ) then                   ! op horizontale lijn    
                  inside = 1
                  return
               else if (x1 .ne. x2) then                                  ! scheve lijn
                  rl = ( xl - x1 )  / ( x2 - x1 )
                  rm = ( y1 - yl )  + rl * ( y2 - y1 )
                  if (rm .eq. 0) then                                     ! op scheve lijn
                     inside = 1
                     return
                  else if (rm .gt. 0d0) then                              ! onder scheve lijn
                     if (xl .eq. x1 .or. xl .eq. x2) then
                        if (x1 .gt. xl .or. x2 .gt. xl) then
                           rechts = rechts + 1
                        endif
                     endif
                     inside = 1 - inside
                  endif
               endif
            endif
            i = i + 1
            if (i .lt. np) goto 10
            if (mod(rechts,2) .ne. 0) inside = 1 - inside
         endif
      end subroutine pinpok
      
      ! =======================================================================
      
      !> Execute the Converters in the Connection sequentially.
      function ecConverterPerformConversions(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps since the kernel's reference date
         !
         success = .false.
         !
         select case(connection%converterPtr%ofType)
            case (convType_uniform)
               success = ecConverterUniform(connection, timesteps)
            case (convType_uniform_to_magnitude)
               success = ecConverterUniformToMagnitude(connection, timesteps)
            case (convType_unimagdir)
               success = ecConverterUnimagdir(connection, timesteps)
            case(convType_spiderweb)
               success = ecConverterSpiderweb(connection, timesteps)
            case(convType_fourier)
               success = ecConverterFourier(connection, timesteps)
            case(convType_arcinfo)
               success = ecConverterArcinfo(connection, timesteps)
            case(convType_curvi)
               success = ecConverterCurvi(connection, timesteps)
            case(convType_polytim)
               success = ecConverterPolytim(connection, timesteps)
            case(convType_netcdf)
               success = ecConverterNetcdf(connection, timesteps)
            case(convType_qhtable)
               success = ecConverterQhtable(connection, timesteps)
            case(convType_samples)
               success = ecConverterSamples(connection, timesteps)
            case default
               call setECMessage("ERROR: ec_converter::ecConverterPerformConversions: Unknown Converter type requested.")
         end select
      end function ecConverterPerformConversions
      
      ! =======================================================================
      
      !> Calculate weight factors from times.
      subroutine time_weight_factors(a0, a1, timesteps, t0, t1, extrapolated)
         !
         ! parameters
         real(hp), intent(inout) :: a0, a1
         real(hp), intent(in)    :: t1, t0, timesteps
         logical, optional       :: extrapolated
         !
         ! locals
         logical :: l_extrapolated
         !
         ! body
         l_extrapolated = .false.
         if (comparereal(timesteps,t0) == -1) then
            l_extrapolated = .true.
            a0 = 1.0_hp
            a1 = 0.0_hp
         endif
         if (comparereal(timesteps,t1) == 1) then
            l_extrapolated = .true.
            a0 = 0.0_hp
            a1 = 1.0_hp
         endif
         if (.not. l_extrapolated) then    
            a0 = 1.0_hp
            a1 = 0.0_hp
            !
            if (comparereal(t0, t1) /= 0) then
               a1 = real((timesteps-t0)/(t1-t0), hp)
               a0 = a0 - a1
            endif
         endif
         if (present(extrapolated)) extrapolated = l_extrapolated
      end subroutine time_weight_factors
      
      ! =======================================================================

      !> Perform the configured conversion, if supported, for a uniform FileReader.
      !! Supports linear interpolation in time, no interpolation in space and no weights.
      !! Supports overwriting and adding-to the entire target Field array, as well all as overwriting only one array element.
      !! Converts source(i) to target(i).
      function ecConverterUniform(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         real(hp)                            :: t0, t1        !< source item t0 and t1
         real(hp)                            :: a0, a1        !< weight for source t0 and t1 data
         integer                             :: n_data        !< number of values
         real(hp), dimension(:), pointer     :: valuesT0      !< values at time t0
         real(hp), dimension(:), pointer     :: valuesT1      !< values at time t1
         real(hp), dimension(:), allocatable :: valuesT       !< values at time t
         integer                             :: istat         !< allocation status
         integer                             :: i, j          !< loop counters
         type(tEcField),         pointer     :: targetField   !< Converter's result goes in here
         integer                             :: maxlay        !< maximum number of layers (3D)
         integer                             :: from, thru    !< contiguous range of indices in the target array 
         !
         success = .false.
         valuesT0 => null()
         valuesT1 => null()
         targetField => null()
         !
         ! ===== interpolation =====
         ! linear interpolation in time
         t0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%timesteps
         t1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%timesteps
         call time_weight_factors(a0, a1, timesteps, t0, t1)
         !
         valuesT0 => connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr
         valuesT1 => connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr
         n_data = connection%sourceItemsPtr(1)%ptr%quantityPtr%vectorMax
         if (associated(connection%targetItemsPtr(1)%ptr%ElementSetPtr%z)) then 
            maxlay = size(connection%targetItemsPtr(1)%ptr%ElementSetPtr%z) / size(connection%targetItemsPtr(1)%ptr%ElementSetPtr%x) 
         else
            maxlay = 1 
         endif 
         allocate(valuesT(maxlay*n_data), stat=istat)
         valuesT=ec_undef_hp
         do i=1, size(valuesT0,dim=1)
            valuesT(i) = valuesT0(i)*a0 + valuesT1(i)*a1
         end do
         !

         select case(connection%converterPtr%interpolationType)
            case (interpolate_timespace)
               ! ===== operation =====
               ! Check target Item(s).
               do i=1, connection%nTargetItems
                  if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                     call setECMessage("ERROR: ec_converter::ecConverterUniform: Target ElementSet's number of coordinates not set.")
                     return
                  end if
               end do
               ! Select the operation.
               select case(connection%converterPtr%operandType)
                  case(operand_replace)
                     ! Write all values to one target Item or each value to its own target Item.
                     if (connection%nTargetItems == 1) then                               ! All in one target item
                        targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                        do j=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                           from = (j-1)*(maxlay*n_data)+1
                           thru = (j  )*(maxlay*n_data) 
                           targetField%arr1dPtr(from:thru) = valuesT
                        end do
                        targetField%timesteps = timesteps
                     else if (connection%nTargetItems == maxlay*n_data) then              ! Separate target items   
                        do i=1, connection%nTargetItems
                           targetField => connection%targetItemsPtr(i)%ptr%targetFieldPtr
                           do j=1, connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates
                              targetField%arr1dPtr(j) = valuesT(i)
                           end do
                           targetField%timesteps = timesteps
                        end do
                     else
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Number of source Quantities does not match the number of target Items.")
                        return
                     end if
                  case(operand_replace_element) ! TODO: AvD/EB: why does operand_replace require a targetIndex, whereas operand_add does not?
                     if (connection%converterPtr%targetIndex == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Converter's target Field array index not set.")
                        return
                     end if
                     targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                     j = connection%converterPtr%targetIndex
                     from = (j-1)*(maxlay*n_data)+1
                     thru = (j  )*(maxlay*n_data) 
                     targetField%arr1dPtr(from:thru) = valuesT
                     targetField%timesteps = timesteps
                  case(operand_add) ! TODO: AvD/EB: it seems that operand_add does not support targetIndex (offset). Should we not make this available?
                     ! Add all values to one target Item or each value to its own target Item.
                     if (connection%nTargetItems == 1) then                               ! All in one target item
                        targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                        do j=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                           from = (j-1)*(maxlay*n_data)+1
                           thru = (j  )*(maxlay*n_data) 
                           targetField%arr1dPtr(from:thru) = targetField%arr1dPtr(from:thru) + valuesT
                        end do
                        targetField%timesteps = timesteps
                     else if (connection%nTargetItems == maxlay*n_data) then              ! Separate target items   
                        do i=1, connection%nTargetItems
                           targetField => connection%targetItemsPtr(i)%ptr%targetFieldPtr
                           do j=1, connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates
                              targetField%arr1dPtr(j) = targetField%arr1dPtr(j) + valuesT(i)
                           end do
                           targetField%timesteps = timesteps
                        end do
                     else
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Number of source Quantities does not match the number of target Items.")
                        return
                     end if
                  case(operand_add_element) 
                     if (connection%converterPtr%targetIndex == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Converter's target Field array index not set.")
                        return
                     end if
                     targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                     j = connection%converterPtr%targetIndex
                     from = (j-1)*(maxlay*n_data)+1
                     thru = (j  )*(maxlay*n_data) 
                     targetField%arr1dPtr(from:thru) = targetField%arr1dPtr(from:thru) + valuesT
                     targetField%timesteps = timesteps
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterUniform: Unsupported operand type requested.")
                     return
               end select
            case (interpolate_time, interpolate_time_extrapolation_ok) ! performs implicit space conversion from 2D to 3D, 
               ! ===== operation =====
               select case(connection%converterPtr%operandType)
                  case(operand_replace_element)
                     if (connection%converterPtr%targetIndex == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniform: Converter's target Field array index not set.")
                        return
                     end if
                     targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
                     from = (connection%converterPtr%targetIndex-1)*(maxlay*n_data)+1
                     thru =  connection%converterPtr%targetIndex   *(maxlay*n_data) 
                     targetField%arr1dPtr(from:thru) = valuesT

                     ! NOTE: AvD: call site now should account for n_data in the offset already, and I changed the above line to that as well.
                     !            But: can we expect from user/call site that it also accounts for maxlay? (at the moment we have no 3D target
                     !            element sets which *also* have n_data > 1, so may not be a problem yet)
                     ! targetField%arr1dPtr((connection%converterPtr%targetIndex-1)*maxlay*n_data + 1 : connection%converterPtr%targetIndex*maxlay*n_data) = valuesT
                     ! NOTE: RL: Currently this most definitely not the case: targetindex is not set correctly upon init.
                     targetField%timesteps = timesteps
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterUniform: Unsupported operand type requested.")
                     return
               end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterUniform: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterUniform

      ! =======================================================================

      !> Perform the configured conversion, if supported, for a uniform FileReader.
      !! Supports linear interpolation in time, no interpolation in space and no weights.
      !! Supports overwriting and adding-to the entire target Field array, as well all as overwriting only one array element.
      !! Converts all sources into one target: the magnitude of the wind.
      function ecConverterUniformToMagnitude(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         integer                 :: j           !< loop counter
         real(hp)                :: t0          !< source item t0
         real(hp)                :: t1          !< source item t1
         real(hp)                :: a0          !< weight for source t0 data
         real(hp)                :: a1          !< weight for source t1 data
         real(hp)                :: windXT0     !< wind x-component value at time t0
         real(hp)                :: windXT1     !< wind x-component value at time t1
         real(hp)                :: windYT0     !< wind y-component value at time t0
         real(hp)                :: windYT1     !< wind y-component value at time t1
         real(hp)                :: windXT      !< wind x-component value at time t
         real(hp)                :: windYT      !< wind y-component value at time t
         real(hp)                :: magnitude   !< wind magnitude at time t
         type(tEcField), pointer :: targetField !< Converter's result goes in here
         !
         success = .false.
         targetField => null()
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_time)
               ! linear interpolation in time
               t0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%timesteps
               t1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%timesteps
               call time_weight_factors(a0, a1, timesteps, t0, t1)
               !
               targetField => connection%targetItemsPtr(1)%ptr%targetFieldPtr
               windXT0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(1)
               windXT1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(1)
               windYT0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(2)
               windYT1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(2)
               windXT = windXT0*a0 + windXT1*a1
               windYT = windYT0*a0 + windYT1*a1
               magnitude = sqrt(windXT*windXT + windYT*windYT)
               ! ===== operation =====
               select case(connection%converterPtr%operandType)
                  case(operand_replace)
                     if (connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Target ElementSet's number of coordinates not set.")
                        return
                     end if
                     do j=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                        targetField%arr1dPtr(j) = magnitude
                     end do
                     targetField%timesteps = timesteps
                  case(operand_replace_element)
                     if (connection%converterPtr%targetIndex == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Converter's target Field array index not set.")
                        return
                     end if
                     targetField%arr1dPtr(connection%converterPtr%targetIndex) = magnitude
                     targetField%timesteps = timesteps
                  case(operand_add)
                     if (connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                        call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Target ElementSet's number of coordinates not set.")
                        return
                     end if
                     do j=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                        targetField%arr1dPtr(j) = targetField%arr1dPtr(j) + magnitude
                     end do
                     targetField%timesteps = timesteps
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Unsupported operand type requested.")
                     return
               end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterUniformToMagnitude: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterUniformToMagnitude
      
      ! =======================================================================
      
      !> Perform the configured conversion, if supported, for a unimagdir FileReader.
      !! Supports linear interpolation in time, no interpolation in space and no weights.
      !! Supports overwriting and adding-to the entire target Field array.
      !! Converts wind_magnitude and wind_direction into wind_u and wind_v.
      !! Assumes target(1) == wind_u and target(2) == wind_v.
      !! meteo1 : regdir, magdir2uv
      function ecConverterUnimagdir(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         real(hp)                        :: t0             !< source item t0
         real(hp)                        :: t1             !< source item t1
         real(hp)                        :: a0             !< weight for source t0 data
         real(hp)                        :: a1             !< weight for source t1 data
         real(hp)                        :: windspeed0     !< wind speed at timesteps t0
         real(hp)                        :: windspeed1     !< wind speed at timesteps t1
         real(hp)                        :: windspeed      !< time interpolated wind speed
         real(hp)                        :: winddirection0 !< wind direction at timesteps t0
         real(hp)                        :: winddirection1 !< wind direction at timesteps t1
         real(hp)                        :: winddirection  !< time interpolated wind direction
         real(hp), dimension(:), pointer :: u              !< calculated u value
         real(hp), dimension(:), pointer :: v              !< calculated v value
         integer                         :: i              !< loop counter
         real(hp)                        :: targetU        !< new u value to be written to all target grid points
         real(hp)                        :: targetV        !< new v value to be written to all target grid points
         !
         success = .false.
         u => null()
         v => null()
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_timespace)
               ! === linear interpolation in time ===
               t0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%timesteps
               t1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%timesteps
               call time_weight_factors(a0, a1, timesteps, t0, t1)
               ! determine windspeed at t=timesteps
               windspeed0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(1)
               windspeed1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(1)
               windspeed = a0*windspeed0 + a1*windspeed1
               ! determine winddirection at t=timesteps
               winddirection0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(2)
               winddirection1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(2)
               winddirection = cyclic_interpolation(winddirection0, winddirection1, a0, a1)
               ! === space conversion using nautical convention ===
               winddirection = (270.0_hp - winddirection)*degrad
               u => connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr
               v => connection%targetItemsPtr(2)%ptr%targetFieldPtr%arr1dPtr
               targetU = windspeed * cos(winddirection)
               targetV = windspeed * sin(winddirection)
               select case(connection%converterPtr%operandType)
                  case(operand_replace)
                     do i=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                        u(i) = targetU
                        v(i) = targetV
                     end do
                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                     connection%targetItemsPtr(2)%ptr%targetFieldPtr%timesteps = timesteps
                  case(operand_add)
                     do i=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                        u(i) = u(i) + targetU
                        v(i) = v(i) + targetV
                     end do
                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                     connection%targetItemsPtr(2)%ptr%targetFieldPtr%timesteps = timesteps
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterUnimagdir: Unsupported operand type requested.")
                     return
               end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterUnimagdir: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterUnimagdir
      
      ! =======================================================================
      
      !> Execute the Converters in the Connection sequentially.
      !! meteo1 : polyint
      function ecConverterPolytim(connection, timesteps) result (success)
      use m_ec_message
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         integer  :: i, j, k,m        !< loop counters
         real(hp) :: wL, wR           !< left and right weights
         integer  :: kL, kR, kkL, kkR !< 
         integer  :: maxlay_tgt       !< size of ElementSet of the TARGET in third dimension (if relevant), a.k.a kmx 
         integer  :: maxlay_src       !< size of ElementSet of the SOURCE in third dimension (if relevant)
         integer  :: maxlay_srcL      !< number of layers at the LEFT interpolation support point
         integer  :: maxlay_srcR      !< number of layers at the RIGHT interpolation support point
         real(hp) :: fieldValue       !< 
         integer  :: kbegin, kend, kbeginL, kendL, kbeginR, kendR, idxL1, idxR1, idxL2, idxR2 !< 
         real(hp) :: sigma, wwL, wwR  !< 
         real(hp), dimension(:), allocatable :: valL1, valL2, valR1, valR2, val !< 
         real(hp), dimension(:), allocatable :: sigmaL, sigmaR !< 
         integer  :: idx              !< helper variable
         integer  :: vectormax
         integer  :: from, thru       !< contiguous range of indices in the target array 
         character(maxMessageLen) :: errormsg

         !
        success = .false.
         select case(connection%converterPtr%interpolationType)
            case (interpolate_passthrough)
               select case(connection%converterPtr%operandType)
                  case(operand_replace_element)
                     idx = connection%converterPtr%targetIndex
                     ! Highly specific: 1 source Item with 1 value.
                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(idx) = connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(idx)
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterPolytim: Unsupported operand type requested.")
                     return
                  end select
            case (interpolate_spacetimeSaveWeightFactors, interpolate_spacetime)
               !! RL: Deze check kan eruit, is al bij de aanleg van de items vastgesteld 
               !if (connection%targetItemsPtr(1)%ptr%vectorMax /= connection%sourceItemsPtr(1)%ptr%vectorMax) then
               !   write(errormsg, '(a,i0,a,i0,a,i0,a,i0,a)') &
               !                   'ERROR: ec_converter::ecConverterPolytim: Vector max size in source item ', connection%sourceItemsPtr(1)%ptr%id, &
               !                   ' does not match the target item ', connection%targetItemsPtr(1)%ptr%id, &
               !                   ' (', vectormax, '<>', vectormax_tgt, ').'
               !   call setECMessage(errormsg)
               !   return
               !endif  

               vectormax = connection%sourceItemsPtr(1)%ptr%quantityPtr%vectorMax   
               if (allocated(valL1)) deallocate(valL1)
               allocate(valL1(vectormax))
               if (allocated(valL2)) deallocate(valL2)
               allocate(valL2(vectormax))
               if (allocated(valR1)) deallocate(valR1)
               allocate(valR1(vectormax))
               if (allocated(valR2)) deallocate(valR2)
               allocate(valR2(vectormax))
               if (allocated(val)) deallocate(val)
               allocate(val(vectormax))
               if (associated(connection%targetItemsPtr(1)%ptr%elementSetPtr%z)) then
                  maxlay_tgt = size(connection%targetItemsPtr(1)%ptr%elementSetPtr%z) /   &
                               size(connection%targetItemsPtr(1)%ptr%elementSetPtr%x)
               else
                  maxlay_tgt = 1 
               end if
               !
               ! The polytim FileReader's source Item is actually a target Item, containing the time-interpolated wind_magnitude from the subFileReaders.
               do i=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                  kL = connection%converterPtr%indexWeight%indices(1,i)
                  kR = connection%converterPtr%indexWeight%indices(2,i)
                  wL = connection%converterPtr%indexWeight%weightFactors(1,i)
                  wR = connection%converterPtr%indexWeight%weightFactors(2,i)
                  select case(connection%converterPtr%operandType)
                     case(operand_replace, operand_add)
                        ! Are the subproviders 3D or 2D?
                        if (associated(connection%sourceItemsPtr(1)%ptr%elementSetPtr%z) .and. &     ! source has sigma
                               associated(connection%targetItemsPtr(1)%ptr%elementSetPtr%z)) then    ! target has sigma
                           ! 3D subproviders
                           maxlay_src = size(connection%sourceItemsPtr(1)%ptr%ElementSetPtr%z) /   &
                                        size(connection%sourceItemsPtr(1)%ptr%ElementSetPtr%x)
                           if (allocated(sigmaL)) deallocate(sigmaL)
                           allocate(sigmaL(maxlay_src))
                           if (allocated(sigmaR)) deallocate(sigmaR)
                           allocate(sigmaR(maxlay_src))
                           ! deal with one-sided interpolation
                           if ( kL == 0 .and. kR /= 0 ) then
                              kL = kR
                              wL = 0.0_hp
                           end if
                           if ( kR == 0 .and. kL /= 0 ) then
                              kR = kL
                              wR = 0.0_hp
                           end if
                           if (kL > 0) then
                              if (kR > 0) then
                                 kbegin  = maxlay_tgt*(i-1)+1                        ! refers to target column 
                                 kend    = maxlay_tgt*i

                                 kbeginL = maxlay_src*(kL-1)+1                       ! refers to source left column 
                                 kendL   = maxlay_src*kL
                                 sigmaL  = connection%sourceItemsPtr(1)%ptr%ElementSetPtr%z(kbeginL:kendL)

                                 kbeginR = maxlay_src*(kR-1)+1                       ! refers to source right column 
                                 kendR   = maxlay_src*kR
                                 sigmaR  = connection%sourceItemsPtr(1)%ptr%ElementSetPtr%z(kbeginR:kendR)
                                 !
                                 do maxlay_srcL=maxlay_src,1,-1
                                    if (sigmaL(maxlay_srcL)>0.5*ec_undef_hp) exit
                                 enddo 
                                 if (maxlay_srcL<1) then 
                                    write(errormsg,'(a,i0,a,i4.4)') "ERROR: ec_converter::ecConverterPolytim: No valid sigma (layer) associated with point ", &
                                                                      kL," of polytim item ", connection%sourceItemsPtr(1)%ptr%id
                                    call setECMessage(errormsg)
                                    return                                    
                                 endif 
                                 do maxlay_srcR=maxlay_src,1,-1
                                    if (sigmaR(maxlay_srcR)>0.5*ec_undef_hp) exit
                                 enddo 
                                 if (maxlay_srcR<1) then 
                                    write(errormsg,'(a,i0,a,i4.4)') "ERROR: ec_converter::ecConverterPolytim: No valid sigma (layer) associated with point ", &
                                                                      kR," of polytim item ", connection%sourceItemsPtr(1)%ptr%id
                                    call setECMessage(errormsg)
                                    return                                    
                                 endif 
                                 !
                                 do k=kbegin,kend
                                    ! RL: BUG!!! z(k) not initialised if the model is not 3D !!! TO BE FIXED !!!!!!!!!!!!!!
                                    sigma = connection%targetItemsPtr(1)%ptr%elementSetPtr%z(k)
                                    if ( sigma < 0.5*ec_undef_hp ) cycle
                                    do kkL=0, maxlay_srcL-1                       ! find vertical indices and weights for the LEFT point
                                       if (sigma<=sigmaL(kkL+1)) exit 
                                    enddo
                                    if (kkL==0) then                              ! only use upper of idxL1 and idxL2
                                       wwL = 0.5d0
                                       idxL2 = maxlay_src*(kL-1) + kkL + 1
                                       idxL1 = idxL2
                                    elseif (kkL==maxlay_srcL) then                ! only use lower of idxL1 and idxL2
                                       wwL = 0.5d0
                                       idxL1 = maxlay_src*(kL-1) + kkL 
                                       idxL2 = idxL1
                                    else                                          ! save to use both idxL1 AND idxL2
                                       wwL = (sigmaL(kkL+1)-sigma) / (sigmaL(kkL+1)-sigmaL(kkL))
                                       idxL1 = maxlay_src*(kL-1) + kkL
                                       idxL2 = maxlay_src*(kL-1) + kkL + 1
                                    endif 

                                    do kkR=0, maxlay_srcR-1                       ! find vertical indices and weights for the RIGHT point
                                       if (sigma<=sigmaR(kkR+1)) exit 
                                    enddo
                                    if (kkR==0) then
                                       wwR = 0.5d0
                                       idxR2 = maxlay_src*(kR-1) + kkR + 1
                                       idxR1 = idxR2
                                    elseif (kkR==maxlay_srcR) then 
                                       wwR = 0.5d0
                                       idxR1 = maxlay_src*(kR-1) + kkR
                                       idxR2 = idxR1
                                    else 
                                       wwR = (sigmaR(kkR+1)-sigma) / (sigmaR(kkR+1)-sigmaR(kkR))
                                       idxR1 = maxlay_src*(kR-1) + kkR
                                       idxR2 = maxlay_src*(kR-1) + kkR + 1
                                    endif 

                                    ! idx are in terms of vector for a specific pli-point and layer 
                                    valL1(1:vectormax) = connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1Dptr((idxL1-1)*vectormax+1:(idxL1)*vectormax)
                                    valL2(1:vectormax) = connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1Dptr((idxL2-1)*vectormax+1:(idxL2)*vectormax)
                                    valR1(1:vectormax) = connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1Dptr((idxR1-1)*vectormax+1:(idxR1)*vectormax)
                                    valR2(1:vectormax) = connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1Dptr((idxR2-1)*vectormax+1:(idxR2)*vectormax)
                                    !
                                    select case(connection%sourceItemsPtr(1)%ptr%quantityPtr%zInterpolationType)
                                       case(zinterpolate_unknown)
                                          call setECMessage("WARNING: ec_converter::ecConverterPolytim: Unknown vertical interpolation type given, will proceed with linear method.")
                                          val = wL*(wwL*valL1 + (1.0_hp-wwL)*valL2) + wR*(wwR*valR1 + (1.0_hp-wwR)*valR2)
                                       case(zinterpolate_linear)
                                          val = wL*(wwL*valL1 + (1.0_hp-wwL)*valL2) + wR*(wwR*valR1 + (1.0_hp-wwR)*valR2)
                                       case(zinterpolate_block)
                                          val = wL*valL1  + wR*valR1
                                       case(zinterpolate_log)
                                          val = wL*(valL1**wwL)*(valL2**(1.0_hp-wwL)) + wR*(valR1**wwR)*(valR2**(1.0_hp-wwR))
                                       case default
                                          call setECMessage("ERROR: ec_converter::ecConverterPolytim: Unsupported vertical interpolation type requested.")
                                          return
                                    end select
                                    !
                                    if (connection%converterPtr%operandType == operand_replace) then
                                       connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((k-1)*vectormax+1:k*vectormax) = val(1:vectormax)
                                    else if (connection%converterPtr%operandType == operand_add) then
                                       connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((k-1)*vectormax+1:k*vectormax)   &
                                             = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((k-1)*vectormax+1:k*vectormax) + val(1:vectormax)
                                    end if
                                    !
                                 end do            ! target layers 
                              end if               ! kR > 0: right support point exists 
                           end if                  ! kL > 0: left support point exists
                        else                       ! no vertical coordinate assigned to this source item, i.e. 3D source 
                           ! 2D subproviders
                           connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps   !!!!! ???????
                           ! Determine value
                           if (kL > 0) then
                              if (kR > 0) then
                                 val(1:vectormax) = wL*connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((kL-1)*vectormax+1:kL*vectormax) &
                                                  + wR*connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((kR-1)*vectormax+1:kR*vectormax)
                              else ! Just left point
                                 val(1:vectormax) = wL*connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((kL-1)*vectormax+1:kL*vectormax)
                              end if
                           else if (kR > 0) then ! Just right point
                                 val(1:vectormax) = wR*connection%sourceItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr((kR-1)*vectormax+1:kR*vectormax)
                           end if 
                              !
                           if( kL /= 0 .or. kR /= 0 ) then
                              ! Write value
                              do k = 1,maxlay_tgt
                                 from = (i-1)*maxlay_tgt*vectormax+(k-1)*vectormax+1
                                 thru = (i-1)*maxlay_tgt*vectormax+k*vectormax
                                 if (connection%converterPtr%operandType == operand_replace) then
                                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(from:thru) = val(1:vectormax)

                                 else if (connection%converterPtr%operandType == operand_add) then
                                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(from:thru) = &
                                     connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(from:thru) + val(1:vectormax)
                                 end if 
                              enddo
                           end if            ! valid left or right point ?
                        end if               ! vertical coordinate for this source item, i.e. is it a 3D source  ? 
                    case default
                       call setECMessage("ERROR: ec_converter::ecConverterPolytim: Unsupported operand type requested.")
                       return
                  end select
               end do
            case default
               call setECMessage("ERROR: ec_converter::ecConverterPolytim: Unsupported interpolation type requested.")
               return
         end select
         success = .true.   
      end function ecConverterPolytim
!     maxlaysource 

      ! =======================================================================

      !> Perform the configured conversion, if supported, for a curvi FileReader.
      !! Supports linear interpolation in time and interpolation is space.
      !! Supports overwriting and adding-to the entire target Field array.
      !! Converts data from source Item i to target Item i.
      !! unstruc : gettimespacevalue 
      function ecConverterCurvi(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         type(tEcField),       pointer :: sourceT0Field    !< helper pointer
         type(tEcField),       pointer :: sourceT1Field    !< helper pointer
         type(tEcField),       pointer :: targetField      !< helper pointer
         type(tEcIndexWeight), pointer :: indexWeight      !< helper pointer, saved index weights
         type(tEcElementSet),  pointer :: sourceElementSet !< source ElementSet
         integer :: n_cols, n_points
         integer :: mp, np
         integer :: i, j
         real(hp) :: a0, a1
         real(hp) :: t0, t1
         real(hp), dimension(4) :: wf_i !< helper containing indexWeight%weightFactors(1:4,i)
         real(hp) :: tmp
         !
         success = .false.
         sourceT0Field    => null()
         sourceT1Field    => null()
         targetField      => null()
         indexWeight      => null()
         sourceElementSet => null()
         !
         if (connection%nSourceItems /= connection%nTargetItems) then
            call setECMessage("ERROR: ec_converter::ecConverterCurvi: The number of source and target Items differ and should have been identical.")
            return
         end if
         !
         select case(connection%converterPtr%interpolationType)
         case (interpolate_spacetimeSaveWeightFactors)
            indexWeight => connection%converterPtr%indexWeight
            sourceElementSet => connection%sourceItemsPtr(1)%ptr%elementSetPtr
            n_points = connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
            n_cols = sourceElementSet%n_cols
            !
            sourceT0Field => connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr
            sourceT1Field => connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr
            t0 = sourceT0Field%timesteps
            t1 = sourceT1Field%timesteps
            call time_weight_factors(a0, a1, timesteps, t0, t1)
            !
            do j=1, connection%nSourceItems
               sourceT0Field => connection%sourceItemsPtr(j)%ptr%sourceT0FieldPtr
               sourceT1Field => connection%sourceItemsPtr(j)%ptr%sourceT1FieldPtr
               targetField   => connection%targetItemsPtr(j)%ptr%targetFieldPtr
               !
               select case(connection%converterPtr%operandType)
               case(operand_replace)                        
                  do i=1, n_points
                     mp = indexWeight%indices(1,i)
                     np = indexWeight%indices(2,i)
                     if (mp > 0 .and. np > 0) then
                        wf_i = indexWeight%weightFactors(1:4,i)
                        ! FM's 2D to EC's 1D array mapping requires np = np-1 from this point on.
                        tmp = a0 * (wf_i(1)*sourceT0Field%arr1d(mp  +n_cols* (np-1))    + &
                                    wf_i(2)*sourceT0Field%arr1d(mp+1+n_cols* (np-1))    + &
                                    wf_i(3)*sourceT0Field%arr1d(mp+1+n_cols*((np-1)+1)) + &
                                    wf_i(4)*sourceT0Field%arr1d(mp  +n_cols*((np-1)+1)))  &
                              + &
                              a1 * (wf_i(1)*sourceT1Field%arr1d(mp  +n_cols* (np-1))    + &
                                    wf_i(2)*sourceT1Field%arr1d(mp+1+n_cols* (np-1))    + &
                                    wf_i(3)*sourceT1Field%arr1d(mp+1+n_cols*((np-1)+1)) + &
                                    wf_i(4)*sourceT1Field%arr1d(mp  +n_cols*((np-1)+1)))
                     
                        targetField%arr1dPtr(i) = tmp
                     
                     end if
                  end do
                  targetField%timesteps = timesteps
               case(operand_add)                        
                  do i=1, n_points
                     mp = indexWeight%indices(1,i)
                     np = indexWeight%indices(2,i)
                     if (mp > 0 .and. np > 0) then
                        wf_i = indexWeight%weightFactors(1:4,i)
                        ! FM's 2D to EC's 1D array mapping requires np = np-1 from this point on.
                        tmp =  a0 * (wf_i(1)*sourceT0Field%arr1d(mp  +n_cols* (np-1))    + &
                                       wf_i(2)*sourceT0Field%arr1d(mp+1+n_cols* (np-1))    + &
                                       wf_i(3)*sourceT0Field%arr1d(mp+1+n_cols*((np-1)+1)) + &
                                       wf_i(4)*sourceT0Field%arr1d(mp  +n_cols*((np-1)+1)))  &
                                 + &
                                 a1 * (wf_i(1)*sourceT1Field%arr1d(mp  +n_cols* (np-1))    + &
                                       wf_i(2)*sourceT1Field%arr1d(mp+1+n_cols* (np-1))    + &
                                       wf_i(3)*sourceT1Field%arr1d(mp+1+n_cols*((np-1)+1)) + &
                                       wf_i(4)*sourceT1Field%arr1d(mp  +n_cols*((np-1)+1)))
                           
                        targetField%arr1dPtr(i) = targetField%arr1dPtr(i) + tmp
                           
                     end if
                  end do
                  targetField%timesteps = timesteps
               case default
                  call setECMessage("ERROR: ec_converter::ecConverterCurvi: Unsupported operand type requested.")
                  return
               end select
               !
            end do
         case default
            call setECMessage("ERROR: ec_converter::ecConverterCurvi: Unsupported interpolation type requested.")
            return
         end select
         success = .true.   
      end function ecConverterCurvi
      
      ! =======================================================================
      
      !> Perform the configured conversion, if supported, for a arcinfo FileReader.
      !! Supports linear interpolation in time, interpolation in space and no weights.
      !! Supports overwriting and adding-to the entire target Field array.
      !! meteo1 : gettimespacevalue
      function ecConverterArcinfo(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         real(hp) :: x01, y01, dx1, dy1 !< uniform grid parameters
         integer  :: mx                 !< n_cols (x or latitude coordinate)
         integer  :: nx                 !< n_rows (y or longitude coordinate)
         integer  :: n, ic              !< loop counter
         real(hp) :: x1, y1             !< 
         integer  :: i1, j1             !< 
         real(hp) :: di1, dj1           !< 
         real(hp), dimension(4) :: f    !< spatial weights
         real(hp), dimension(4) :: fmask!< spatial mask
         real(hp) :: fsum               !< summed spatial weights*masks 
         real(hp), dimension(4) :: u    !< source u, v or p at timesteps=t0
         real(hp), dimension(4) :: v    !< source u, v or p at timesteps=t1
         real(hp) :: a0, a1             !< 
         real(hp) :: t0, t1             !< 
         real(hp) :: vv0                !< target u, v or p at timesteps=t0
         real(hp) :: vv1                !< target u, v or p at timesteps=t1
         real(hp) :: rr                 !< target u, v or p at timesteps=t
         type(tEcField), pointer :: sourceT0Field !< helper pointer
         type(tEcField), pointer :: sourceT1Field !< helper pointer
         type(tEcMask), pointer :: srcmask 
         !
         success = .false.
         sourceT0Field => null()
         sourceT1Field => null()
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_timespace, interpolate_spacetime, interpolate_time_extrapolation_ok)
               ! TODO: cleanup this FM-compliancy code segment.
               if (connection%sourceItemsPtr(1)%ptr%elementSetPtr%ofType == elmSetType_cartesian) then
                  ! Only legal for uniform grids...
                  x01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%x(1)
                  y01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%y(1)
                  dx1 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%x(2)-connection%sourceItemsPtr(1)%ptr%elementSetPtr%x(1)
                  dy1 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%y(2)-connection%sourceItemsPtr(1)%ptr%elementSetPtr%y(1)
               else
                  x01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%x0
                  y01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%y0
                  dx1 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%dx
                  dy1 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%dy
               end if
               mx = connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_cols
               nx = connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_rows
               sourceT0Field => connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr
               sourceT1Field => connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr
               !
               do n=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                  ! TODO : Calculate target value depending on ElementSet mask.
                  ! === interpolate in space ===
                  x1 = (connection%targetItemsPtr(1)%ptr%elementSetPtr%x(n) - x01)/dx1
                  if (x1 < -0.5_hp .or. x1 > mx - 0.5_hp) cycle
                  !
                  y1 = (connection%targetItemsPtr(1)%ptr%elementSetPtr%y(n) - y01)/dy1
                  if (y1 < -0.5_hp .or. y1 > nx - 0.5_hp) cycle
                  !
                  i1  = int(x1 + 1)
                  i1  = min(mx - 1,max(1, i1))
                  di1 = x1 + 1 - i1
                  !
                  j1  = int(y1 + 1)
                  j1  = min(nx - 1,max(1, j1))
                  dj1 = y1 + 1 - j1
                  ! spatial weight factors
                  f(1) = (1-di1)*(1-dj1)
                  f(2) = (  di1)*(1-dj1)
                  f(3) = (  di1)*(  dj1)
                  f(4) = (1-di1)*(  dj1)
                  !
                  u(1) = sourceT0Field%arr1dPtr(i1+mx*(j1-1))
                  u(2) = sourceT0Field%arr1dPtr(i1+1+mx*(j1-1))
                  u(3) = sourceT0Field%arr1dPtr(i1+1+mx*j1)
                  u(4) = sourceT0Field%arr1dPtr(i1+mx*j1)
                  v(1) = sourceT1Field%arr1dPtr(i1+mx*(j1-1))
                  v(2) = sourceT1Field%arr1dPtr(i1+1+mx*(j1-1))
                  v(3) = sourceT1Field%arr1dPtr(i1+1+mx*j1)
                  v(4) = sourceT1Field%arr1dPtr(i1+mx*j1)
                  
                  ! === deal with missing values ===
                  srcmask => connection%converterPtr%srcmask
                  fmask = 1 

                  ! For now, the mask consists of ones and zeroes
                  if (allocated(srcmask%msk)) then
                     fmask(1) = (srcmask%msk((j1   -srcmask%nmin)*srcmask%mrange+i1   -srcmask%mmin+1))
                     fmask(2) = (srcmask%msk((j1   -srcmask%nmin)*srcmask%mrange+i1+1 -srcmask%mmin+1))
                     fmask(3) = (srcmask%msk((j1+1 -srcmask%nmin)*srcmask%mrange+i1+1 -srcmask%mmin+1))
                     fmask(4) = (srcmask%msk((j1+1 -srcmask%nmin)*srcmask%mrange+i1   -srcmask%mmin+1))
                  endif 

                  fsum = sum(f*(1.d0-fmask))
                  if (fsum>=1.0e-03) then         
                     f = (f*(1.d0-fmask))/fsum
                  endif

                  vv0  = u(1)*f(1) + u(2)*f(2) + u(3)*f(3) + u(4)*f(4)
                  vv1  = v(1)*f(1) + v(2)*f(2) + v(3)*f(3) + v(4)*f(4)
                  ! === linear interpolation in time ===
                  t0 = sourceT0Field%timesteps
                  t1 = sourceT1Field%timesteps
                  call time_weight_factors(a0, a1, timesteps, t0, t1)            
                  rr = a0*vv0 + a1*vv1
                  select case(connection%converterPtr%operandType)
                     case(operand_replace)
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = rr
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                     case(operand_add)
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) + rr
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                     case default
                        call setECMessage("ERROR: ec_converter::ecConverterArcinfo: Unsupported operand type requested.")
                        return
                  end select
               end do
            case default
               call setECMessage("ERROR: ec_converter::ecConverterArcinfo: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterArcinfo
      
      ! =======================================================================
      
      !> Perform the configured conversion, if supported, for a samples FileReader.
      !! Supports linear triangle interpolation in space, no time, no weights.
      !! meteo1 : timespaceinitialfield
      function ecConverterSamples(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         real(hp) :: x01, y01, dx1, dy1 !< uniform grid parameters
         integer  :: mx                 !< n_cols (x or latitude coordinate)
         integer  :: nx                 !< n_rows (y or longitude coordinate)
         integer  :: n, ic              !< loop counter
         real(hp) :: x1, y1             !< 
         integer  :: i1, j1             !< 
         real(hp) :: di1, dj1           !< 
         real(hp), dimension(4) :: f    !< spatial weights
         real(hp), dimension(4) :: fmask!< spatial mask
         real(hp) :: fsum               !< summed spatial weights*masks 
         real(hp), dimension(4) :: u    !< source u, v or p at timesteps=t0
         real(hp), dimension(4) :: v    !< source u, v or p at timesteps=t1
         real(hp) :: a0, a1             !< 
         real(hp) :: t0, t1             !< 
         real(hp) :: vv0                !< target u, v or p at timesteps=t0
         real(hp) :: vv1                !< target u, v or p at timesteps=t1
         real(hp) :: rr                 !< target u, v or p at timesteps=t
         type(tEcField), pointer :: sourceT0Field !< helper pointer
         type(tEcField), pointer :: sourceT1Field !< helper pointer
         type(tEcMask), pointer :: srcmask 
         integer :: nSamples
         !
         success = .false.
         sourceT0Field => null()
         sourceT1Field => null()
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_triangle)
               x01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%x(1)
               y01 = connection%sourceItemsPtr(1)%ptr%elementSetPtr%y(1)

               nSamples = connection%sourceItemsPtr(1)%ptr%elementSetPtr%nCoordinates
               sourceT1Field => connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr

               call setECMessage('ERROR: ec_converter::ecConverterSamples: triangle interpolation is work in progress.')
               return
               rr = 0d0 ! TODO: AvD: WIP
                  !select case(connection%converterPtr%operandType)
                  !   case(operand_replace)
                  !      connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = rr
                  !      connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                  !   case(operand_add)
                  !      connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) + rr
                  !      connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                  !   case default
                  !      call setECMessage("ERROR: ec_converter::ecConverterSamples: Unsupported operand type requested.")
                  !      return
                  !end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterSamples: Unsupported interpolation type requested.")
               return
         end select
         success = .true.
      end function ecConverterSamples

      ! =======================================================================
      
      !> Perform the configured conversion, if supported, for a qhtable FileReader.
      !! No interpolation is supported. Data is constant over time.
      !! Supports overwriting an array element of the target Field's data array.
      function ecConverterQhtable(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         integer :: i, j !< loop counters
         integer :: start_j
         integer :: nx
         !
         success = .false.
         !
         select case(connection%converterPtr%interpolationType)
            case (interpolate_passthrough)
               select case(connection%converterPtr%operandType)
                  case(operand_replace_element)
                     ! Calculate for whole magic_array, as the proper index is not known here. Polytim converter will select the correct answer. 
                     do i=1, size(magic_array)
                        if (magic_array(i) < connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(1)) then
                           connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(i) = connection%sourceItemsPtr(2)%ptr%sourceT0FieldPtr%arr1dPtr(1) ! waterlevel(i)
                           cycle
                        end if 
                        nx = connection%sourceItemsPtr(1)%ptr%elementSetPtr%nCoordinates
                        if (magic_array(i) > connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(nx)) then
                           connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(i) = connection%sourceItemsPtr(2)%ptr%sourceT0FieldPtr%arr1dPtr(nx) ! waterlevel(nx)
                           cycle
                        end if 
                        do j=2, nx
                           if (magic_array(i) < connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(j)) then ! discharge(j)
                              start_j = j
                              exit
                           end if
                        end do
                        connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(i) = connection%sourceItemsPtr(3)%ptr%sourceT0FieldPtr%arr1dPtr(start_j-1) * magic_array(i) &
                                                                                                                         + connection%sourceItemsPtr(4)%ptr%sourceT0FieldPtr%arr1dPtr(start_j-1)! slope(start_j-1)* magic_array(i) + crossing(start_j-1)
                     end do
                  case default
                     call setECMessage("ERROR: ec_converter::ecConverterQhtable: Unsupported operand type requested.")
                     return
               end select
            case default
               call setECMessage("ERROR: ec_converter::ecConverterQhtable: Unsupported interpolation type requested.")
               return
         end select
         success = .true.   
      end function ecConverterQhtable
      
      ! =======================================================================
      
      !> Perform the configured conversion, if supported, for a unimagdir FileReader.
      !! No interpolation is supported. Data is generated from seed values.
      !! Supports overwriting and adding-to the target Field's single data value, as well all as overwriting only one array element.
      !! Converts angular velocity, phase and magnitude into an amplitude.
      !! meteo1 : readfouriercompstim
      function ecConverterFourier(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         integer                                          :: i, j      !< loop counters
         character(len=maxNameLen)                        :: str       !< helper variable
         logical                                          :: is_astro  !< flag for astronomical signal
         real(hp)                                         :: tseconds  !< time in seconds
         real(hp)                                         :: tnodal    !< start time (seconds after reftime)
         real(hp)                                         :: omega     !< angular velocity [rad/minute]
         real(hp)                                         :: phase0    !< phase at t=t0 [rad]
         real(hp)                                         :: magnitude !< magnitude [m]
         real(hp)                                         :: phase     !< phase at t=timesteps [rad]
         real(hp)                                         :: deflection !< summed result of the Fourier component effects
         !
         success = .false.
         deflection = 0.0_hp
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
         case (interpolate_passthrough)
            ! Source data at timesteps t is generated from the seed data in sourceT0Field.
            ! === FileReader contains periods. ===
            is_astro = .false.
            do j=1, connection%sourceItemsPtr(1)%ptr%elementSetPtr%nCoordinates
               do i=1, connection%nSourceItems
                  str = connection%sourceItemsPtr(i)%ptr%quantityPtr%name
                  if (trim(str) == 'period') then
                     omega = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                     is_astro = allocated(connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%astro_components)
                     tnodal = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%timesteps
                  else if (trim(str) == 'phase') then
                     phase0 = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                  else if (trim(str) == 'magnitude') then
                     magnitude = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                  end if
               end do
               if(is_astro) then
                  tseconds = timesteps - tnodal
               else
                  tseconds = timesteps
               endif
               phase = omega * (tseconds/60) - phase0                         ! omega, angle velocity [rad/minute]
               deflection = deflection + magnitude * cos(phase)
            end do
            select case(connection%converterPtr%operandType)
               case(operand_replace)
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(1) = deflection
               case(operand_replace_element)
                  ! Only used by poly_tim.
                  if (connection%converterPtr%targetIndex == ec_undef_int) then
                     call setECMessage("ERROR: ec_converter::ecConverterFourier: Converter's target Field array index not set.")
                     return
                  end if
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(connection%converterPtr%targetIndex) = deflection
               case(operand_add)
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(1) = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(1) + deflection
               case default
                  call setECMessage("ERROR: ec_converter::ecConverterFourier: Unsupported operand type requested.")
                  return
            end select
         case default
            call setECMessage("ERROR: ec_converter::ecConverterFourier: Unsupported interpolation type requested.")
            return
         end select
         success = .true.         
      end function ecConverterFourier
      
      ! =======================================================================
      
      !> Cyclic interpolation of two scalars, based on periodicity of 360 (degrees)
      !! Sort data in monotonically increasing order and rotate over smallest angle
      function cyclic_interpolation(var1, var2, weight1, weight2)
         real(hp), intent(in) :: var1                !< First input argument for in interpolation functions using a scalar weight value
         real(hp), intent(in) :: var2                !< Second input argument for in interpolation functions using a scalar weight value
         real(hp), intent(in) :: weight1             !< Value for weighing two variables: 'weight1' holds for var1
         real(hp), intent(in) :: weight2             !< Value for weighing two variables: 'weight2' holds for var2
         real(hp)             :: cyclic_interpolation   !< Result value after linear interpolation between var1 and var2 using weightvalue weight
         !
         real(hp)             :: minangle
         real(hp)             :: maxangle
         real(hp)             :: delta
         real(hp)             :: weightfac
    
         minangle  = var1
         maxangle  = var2
         weightfac = weight1
         if (var2 < var1) then
            minangle  = var2
            maxangle  = var1
            weightfac = 1.0_hp - weightfac
         end if
         delta = maxangle - minangle
       
         ! Carry out the interpolation
         if (delta <= 180.0_hp) then
            cyclic_interpolation = (1.0_hp - weightfac)*delta
         else
            cyclic_interpolation = (1.0_hp - weightfac)*delta + weightfac*360.0_hp
         end if
       
         ! Rotate backwards over the smallest angle
         cyclic_interpolation = cyclic_interpolation + minangle
         cyclic_interpolation = modulo(cyclic_interpolation, 360.0_hp)
      end function
      
      ! =======================================================================
      
      !> Execute the Converters in the Connection sequentially.
      !! meteo1: gettimespacevalue
      function ecConverterSpiderweb(connection, timesteps) result (success)
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         real(hp) :: spwdphi !< angular increment: 2pi/#colums
         real(hp) :: spwdrad !< radial increment: radius/#rows
         real(hp) :: a0, a1
         real(hp) :: t0, t1
         real(hp) :: xeye0
         real(hp) :: yeye0
         real(hp) :: xeye1
         real(hp) :: yeye1
         real(hp) :: xeye 
         real(hp) :: yeye
         real(hp) :: dlat
         real(hp) :: dlon
         real(hp) :: xc, yc
         real(hp) :: spwradhat
         real(hp) :: spwphihat
         real(hp) :: uintp
         real(hp) :: vintp
         real(hp) :: pintp
         real(hp) :: spwr1
         real(hp) :: spwd1
         real(hp) :: spwp1
         real(hp) :: spwr2
         real(hp) :: spwd2
         real(hp) :: spwp2
         real(hp) :: spwr3
         real(hp) :: spwd3
         real(hp) :: spwp3
         real(hp) :: spwr4
         real(hp) :: spwd4
         real(hp) :: spwp4
         real(hp) :: wphi 
         real(hp) :: wrad 
         real(hp) :: spwrA
         real(hp) :: spwrB
         real(hp) :: spwdA
         real(hp) :: spwdB
         real(hp) :: spwpA
         real(hp) :: spwpB
         real(hp) :: rintp
         real(hp) :: dintp
         real(hp) :: h1, h2
         real(hp) :: fa, fi
         real(hp) :: earthrad
         real(hp) :: tmp !< helper temporary
         integer :: n_rows, n_cols
         integer :: n !< loop counter
         integer :: mf, nf
         ! TODO : get from MDU
         real(hp) :: paver !< average absolute pressure, to which p_drop is relative.
         paver = 101325.0_hp
         !
         success = .false.
         fa = pi / 180.0_hp
         fi = 180.0_hp / pi
         earthrad = 6378137.0_hp
         n_rows = connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_rows
         n_cols = connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_cols
         !
         ! Calculate the basic spiderweb grid settings
         spwdphi = 360.0_hp / (n_cols - 1) ! 0 == 360 degrees, so -1
         spwdrad = connection%sourceItemsPtr(1)%ptr%elementSetPtr%radius / (n_rows - 1)
         !
         ! Determine the (x,y) coordinate pair of the cyclone eye at t=timesteps.
         t0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%timesteps
         t1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%timesteps
         call time_weight_factors(a0, a1, timesteps, t0, t1)
         xeye0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%x_spw_eye
         yeye0 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%y_spw_eye
         xeye1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%x_spw_eye
         yeye1 = connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%y_spw_eye
         xeye = cyclic_interpolation(xeye0, xeye1, a0, a1) ! cyclic interpolation (spheric coord)
         yeye = cyclic_interpolation(yeye0, yeye1, a0, a1) ! cyclic inteprolation (spheric coord)
         !
         do n=1, connection%targetItemsPtr(1)%ptr%elementSetPtr%nCoordinates
            xc = connection%targetItemsPtr(1)%ptr%elementSetPtr%lat(n)
            yc = connection%targetItemsPtr(1)%ptr%elementSetPtr%lon(n)
            dlat = modulo(yc, 360.0_hp) - yeye
            dlon = modulo(xc, 360.0_hp) - xeye
            h1 = (sin(dlat/2.0_hp*fa))**2 + cos(yeye*fa)*cos(yc*fa)*(sin(dlon/2.0_hp*fa))**2
            h2 = 2.0_hp*atan(sqrt(h1)/sqrt(1.0_hp-h1))
            spwradhat = earthrad*h2
            spwphihat = cos(yeye*fa)*sin(yc*fa) - sin(yeye*fa)*cos(yc*fa)*cos(dlon*fa)
            if (.not. comparereal(spwphihat, 0d0) == 0) then
               spwphihat = atan( sin(dlon*fa)*cos(yc*fa) / (spwphihat) )*fi
            else
               spwphihat = 0d0
            end if
            if ((dlat < 0.0_hp .and. dlon <= 0.0_hp .and. spwphihat >= 0.0_hp) .or. &
                (dlat < 0.0_hp .and. dlon >= 0.0_hp .and. spwphihat <= 0.0_hp) ) then
               spwphihat = spwphihat + 180.0_hp
            endif
            spwphihat = modulo(spwphihat, 360.0_hp)
            ! Find the four nearest points in the spiderweb
            mf = floor(spwphihat/spwdphi) + 1 ! find orientation in windrose
            nf = floor(spwradhat/spwdrad) + 1 ! find orientation on radius
            ! If outside spiderweb or exactly in eye, then set windspeed to zero, else find weightfactors and interpolate
            if (nf >= connection%sourceItemsPtr(1)%ptr%elementSetPtr%n_rows .or. (dlat == 0.0_hp .and. dlon == 0.0_hp)) then
               uintp = 0.0_hp
               vintp = 0.0_hp
               pintp = paver ! TODO : obtain the average ambient pressure from the kernel.
            else
               ! Get data from stencil (mf (+1), nf (+1))
               spwr1 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*(nf-1))*a0 + &
                       connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*(nf-1))*a1 ! linear time interp of magnitude
               spwd1 = cyclic_interpolation(connection%sourceItemsPtr(2)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*(nf-1)), &
                       connection%sourceItemsPtr(2)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*(nf-1)), a0, a1) ! cyclic time interp of direction
               spwp1 = connection%sourceItemsPtr(3)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*(nf-1))*a0 + &
                       connection%sourceItemsPtr(3)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*(nf-1))*a1 ! linear time interp of pressure
               spwr2 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1))*a0 + &
                       connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1))*a1 ! linear time interp of magnitude  
               spwd2 = cyclic_interpolation(connection%sourceItemsPtr(2)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1)), &
                       connection%sourceItemsPtr(2)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1)), a0, a1) ! cyclic time interp of direction
               spwp2 = connection%sourceItemsPtr(3)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1))*a0 + &
                       connection%sourceItemsPtr(3)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*(nf-1))*a1 ! linear time interp of pressure
               spwr3 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*nf)*a0 + &
                       connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*nf)*a1 ! linear time interp of magnitude
               spwd3 =  cyclic_interpolation(connection%sourceItemsPtr(2)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*nf), &
                       connection%sourceItemsPtr(2)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*nf), a0, a1) ! cyclic time interp of direction
               spwp3 = connection%sourceItemsPtr(3)%ptr%sourceT0FieldPtr%arr1dPtr(mf+n_cols*nf)*a0 + &
                       connection%sourceItemsPtr(3)%ptr%sourceT1FieldPtr%arr1dPtr(mf+n_cols*nf)*a1 ! linear time interp of pressure
               spwr4 = connection%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*nf)*a0 + &
                       connection%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*nf)*a1 ! linear time interp of magnitude
               spwd4 = cyclic_interpolation(connection%sourceItemsPtr(2)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*nf), &
                       connection%sourceItemsPtr(2)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*nf), a0, a1) ! cyclic time interp of direction
               spwp4 = connection%sourceItemsPtr(3)%ptr%sourceT0FieldPtr%arr1dPtr(mf+1+n_cols*nf)*a0 + &
                       connection%sourceItemsPtr(3)%ptr%sourceT1FieldPtr%arr1dPtr(mf+1+n_cols*nf)*a1 ! linear time interp of pressure
               ! Safety at center
               if (nf == 1) then
                  spwr1 = 0.0_hp
                  spwd1 = spwd3
               endif
               !
               ! Interpolate over wind direction
               wphi  = 1.0_hp - (spwphihat - (mf-1)*spwdphi)/(spwdphi)   ! weightfactor for the direction
               wrad  = 1.0_hp - (spwradhat - (nf-1)*spwdrad)/(spwdrad)   ! weightfactor for the radius
               spwrA = spwr1*wphi + spwr2*(1.0_hp-wphi)                  ! space interp magnitude (direction)
               spwrB = spwr3*wphi + spwr4*(1.0_hp-wphi)                  ! space interp magnitude (direction)
               tmp = 1.0_hp-wphi
               spwdA = cyclic_interpolation(spwd1, spwd2, wphi, tmp) ! space interp direction (direction)
               spwdB = cyclic_interpolation(spwd3, spwd4, wphi, tmp) ! space interp direction (direction)
               spwpA = spwp1*wphi + spwp2*(1.0_hp-wphi)                  ! space interp pressure (direction)
               spwpB = spwp3*wphi + spwp4*(1.0_hp-wphi)                  ! space interp pressure (direction)
               !
               ! Interpolate over radial direction                     
               rintp = spwrA*wrad + spwrB*(1.0_hp-wrad)                  ! space interp (radius)
               tmp = 1.0_hp-wrad
               dintp = cyclic_interpolation(spwdA, spwdB, wrad, tmp) ! space interp (radius)
               pintp = spwpA*wrad + spwpB*(1.0_hp-wrad)                  ! space interp (radius)
               !
               ! Final treatment of the data
               dintp =  90.0_hp - dintp         ! revert from nautical conventions
               dintp =  modulo(dintp, 360.0_hp)    ! for debug purposes
               uintp = -rintp*cos(dintp*fa)     ! minus sign: wind from N points to S
               vintp = -rintp*sin(dintp*fa)     ! minus sign: wind from N points to S
               pintp =  paver - pintp           ! relate pressure drop to actual pressure
            endif
            select case(connection%converterPtr%operandType)
               case(operand_replace)
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = uintp
                  connection%targetItemsPtr(2)%ptr%targetFieldPtr%arr1dPtr(n) = vintp
                  connection%targetItemsPtr(3)%ptr%targetFieldPtr%arr1dPtr(n) = pintp
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                  connection%targetItemsPtr(2)%ptr%targetFieldPtr%timesteps = timesteps
                  connection%targetItemsPtr(3)%ptr%targetFieldPtr%timesteps = timesteps
               case(operand_add)
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) = connection%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(n) + uintp
                  connection%targetItemsPtr(2)%ptr%targetFieldPtr%arr1dPtr(n) = connection%targetItemsPtr(2)%ptr%targetFieldPtr%arr1dPtr(n) + vintp
                  connection%targetItemsPtr(3)%ptr%targetFieldPtr%arr1dPtr(n) = connection%targetItemsPtr(3)%ptr%targetFieldPtr%arr1dPtr(n) + pintp
                  connection%targetItemsPtr(1)%ptr%targetFieldPtr%timesteps = timesteps
                  connection%targetItemsPtr(2)%ptr%targetFieldPtr%timesteps = timesteps
                  connection%targetItemsPtr(3)%ptr%targetFieldPtr%timesteps = timesteps
               case default
                  call setECMessage("ERROR: ec_converter::ecConverterSpiderweb: Unsupported operand type requested.")
                  return
            end select
         end do
         success = .true.
      end function ecConverterSpiderweb
      
      ! =======================================================================

      !> Perform the configured conversion, if supported, for a NetCDF FileReader.
      !! Supports linear interpolation in time, no interpolation in space and no weights.
      !! Supports overwriting .
      !! Converts source(i) to target(i).
      function ecConverterNetcdf(connection, timesteps) result (success)
      use m_alloc
      implicit none
         logical                            :: success    !< function status
         type(tEcConnection), intent(inout) :: connection !< access to Converter and Items
         real(hp),            intent(in)    :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         !
         integer                 :: i,j,ipt       !< loop counters
         logical                 :: extrapolated  !< .true.: timesteps is outside [t0,t1]
         real(hp)                :: t0            !< source item t0
         real(hp)                :: t1            !< source item t1
         real(hp)                :: a0            !< weight for source t0 data
         real(hp)                :: a1            !< weight for source t1 data
         real(hp)                :: sourceValueT0 !< source value at t0
         real(hp)                :: sourceValueT1 !< source value at t1
         type(tEcField), pointer :: targetField   !< Converter's result goes in here
         type(tEcField), pointer :: sourceT0Field !< helper pointer
         type(tEcField), pointer :: sourceT1Field !< helper pointer
         type(tEcIndexWeight), pointer :: indexWeight !< helper pointer, saved index weights
         type(tEcElementSet), pointer :: sourceElementSet !< source ElementSet
         integer :: n_cols, mp, np, n_points
         type(tEcItem), pointer  :: windxPtr => null() ! pointer to item for windx     
         type(tEcItem), pointer  :: windyPtr => null() ! pointer to item for windy
         real(hp), dimension(:), allocatable :: targetValues
         double precision        :: PI, phi, xtmp
         !
         PI = datan(1.d0)*4.d0
         success = .false.
         targetField => null()
         sourceT0Field => null()
         sourceT1Field => null()
         indexWeight => null()
         sourceElementSet => null()
         ! ===== preprocessing: rotate windx and windy of the source fields if the array of rotations exists in the filereader =====
         do i=1, connection%nSourceItems
            if (connection%SourceItemsPtr(i)%ptr%quantityPtr%name=='eastward_wind') then 
               windxPtr => connection%SourceItemsPtr(i)%ptr
            endif 
            if (connection%SourceItemsPtr(i)%ptr%quantityPtr%name=='northward_wind') then 
               windyPtr => connection%SourceItemsPtr(i)%ptr
            endif 
         enddo 
         if (associated(windxPtr) .and. associated(windyPtr)) then 
            if (associated(windxPtr%elementsetPtr%dir)) then 
               do ipt = 1,  windxPtr%elementsetPtr%ncoordinates
                  phi = windxPtr%elementsetPtr%dir(ipt)*PI/180.d0
                  xtmp = windxPtr%SourceT0fieldptr%arr1dptr(ipt)*cos(phi) + windyPtr%SourceT0fieldptr%arr1dptr(ipt)*sin(phi)
                  windyPtr%SourceT0fieldptr%arr1dptr(ipt) = windxPtr%SourceT0fieldptr%arr1dptr(ipt)*(-sin(phi)) + windyPtr%SourceT0fieldptr%arr1dptr(ipt)*cos(phi)
                  windxPtr%SourceT0fieldptr%arr1dptr(ipt) = xtmp

                  xtmp = windxPtr%SourceT1fieldptr%arr1dptr(ipt)*cos(phi) + windyPtr%SourceT1fieldptr%arr1dptr(ipt)*sin(phi)
                  windyPtr%SourceT1fieldptr%arr1dptr(ipt) = windxPtr%SourceT1fieldptr%arr1dptr(ipt)*(-sin(phi)) + windyPtr%SourceT1fieldptr%arr1dptr(ipt)*cos(phi)
                  windxPtr%SourceT1fieldptr%arr1dptr(ipt) = xtmp
               enddo 
            endif    
         endif 
         !
         ! ===== interpolation =====
         select case(connection%converterPtr%interpolationType)
            case (interpolate_spacetimeSaveWeightFactors)
               ! bilinear interpolation in space
               do i=1, connection%nSourceItems
                  sourceT0Field => connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr
                  sourceT1Field => connection%sourceItemsPtr(i)%ptr%sourceT1FieldPtr
                  indexWeight => connection%converterPtr%indexWeight
                  sourceElementSet => connection%sourceItemsPtr(i)%ptr%elementSetPtr

                  n_points = connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates
                  if (.not.(allocated(targetValues))) then 
                     allocate(targetValues(n_points))
                  elseif (size(targetValues,dim=1)<n_points) then 
                     call realloc(targetValues,n_points,keepExisting=.False.)
                  endif 

                  n_cols = sourceElementSet%n_cols
                  t0 = sourceT0Field%timesteps
                  t1 = sourceT1Field%timesteps
                  call time_weight_factors(a0, a1, timesteps, t0, t1)
                  do j=1, n_points
                     mp = indexWeight%indices(1,j)
                     np = indexWeight%indices(2,j)
                     if (mp > 0 .and. np > 0) then
                        ! FM's 2D to EC's 1D array mapping requires np = np-1 from this point on.
                        !
                        targetValues(j) =  a0 * (indexWeight%weightFactors(1,j)*sourceT0Field%arr1d(mp+n_cols*(np-1)) + &
                                                 indexWeight%weightFactors(2,j)*sourceT0Field%arr1d(mp+1+n_cols*(np-1)) + &
                                                 indexWeight%weightFactors(3,j)*sourceT0Field%arr1d(mp+1+n_cols*((np-1)+1)) + &
                                                 indexWeight%weightFactors(4,j)*sourceT0Field%arr1d(mp+n_cols*((np-1)+1))) &
                                            + &
                                           a1 * (indexWeight%weightFactors(1,j)*sourceT1Field%arr1d(mp+n_cols*(np-1)) + &
                                                 indexWeight%weightFactors(2,j)*sourceT1Field%arr1d(mp+1+n_cols*(np-1)) + &
                                                 indexWeight%weightFactors(3,j)*sourceT1Field%arr1d(mp+1+n_cols*((np-1)+1)) + &
                                                 indexWeight%weightFactors(4,j)*sourceT1Field%arr1d(mp+n_cols*((np-1)+1)))
                     else
                        targetValues(j) = 0.0_hp
                     end if
                  end do
                  ! ===== operation =====
                  select case(connection%converterPtr%operandType)
                     case(operand_replace)
                        do j=1, n_points
                           connection%targetItemsPtr(i)%ptr%targetFieldPtr%arr1dPtr(j) = targetValues(j)
                        end do
                        connection%targetItemsPtr(i)%ptr%targetFieldPtr%timesteps = timesteps
                     case(operand_add)
                        do j=1, n_points
                           connection%targetItemsPtr(i)%ptr%targetFieldPtr%arr1dPtr(j) = connection%targetItemsPtr(i)%ptr%targetFieldPtr%arr1dPtr(j) + targetValues(j)
                        end do
                        connection%targetItemsPtr(i)%ptr%targetFieldPtr%timesteps = timesteps
                     case default
                        call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Unsupported operand type requested.")
                        return
                  end select
               end do
            case (interpolate_time, interpolate_time_extrapolation_ok)
               ! linear interpolation in time
               do i=1, connection%nSourceItems
                  t0 = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%timesteps
                  t1 = connection%sourceItemsPtr(i)%ptr%sourceT1FieldPtr%timesteps
                  targetField => connection%targetItemsPtr(i)%ptr%targetFieldPtr
                  if (t0 > t1) then
                     call setECMessage("WARNING: ec_converter::ecConverterNetcdf: Only one data field available.")
                     if (connection%converterPtr%interpolationType == interpolate_time) then
                        call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Extrapolation not allowed.")
                        return
                     endif
                     ! ===== operation =====
                     select case(connection%converterPtr%operandType)
                        case(operand_replace)
                           if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                              call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Target ElementSet's number of coordinates not set.")
                              return
                           endif
                           if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates /= &
                             & connection%sourceItemsPtr(i)%ptr%elementSetPtr%nCoordinates) then
                              call setECMessage("ERROR: ec_converter::ecConverterNetcdf: ElementSet's number of coordinates differs (Source vs. Target).")
                              return
                           endif
                           do j=1, connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates
                              targetField%arr1dPtr(j) = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                           enddo
                           targetField%timesteps = timesteps
                        case default
                           call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Unsupported operand type requested.")
                           return
                     end select
                  else
                     call time_weight_factors(a0, a1, timesteps, t0, t1, extrapolated)
                     if (extrapolated .and. connection%converterPtr%interpolationType==interpolate_time) then
                        call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Extrapolation not allowed.")
                        return
                     endif
                     ! ===== operation =====
                     select case(connection%converterPtr%operandType)
                        case(operand_replace)
                           if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates == ec_undef_int) then
                              call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Target ElementSet's number of coordinates not set.")
                              return
                           endif
                           if (connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates /= &
                             & connection%sourceItemsPtr(i)%ptr%elementSetPtr%nCoordinates) then
                              call setECMessage("ERROR: ec_converter::ecConverterNetcdf: ElementSet's number of coordinates differs (Source vs. Target).")
                              return
                           endif
                           do j=1, connection%targetItemsPtr(i)%ptr%elementSetPtr%nCoordinates
                              sourceValueT0           = connection%sourceItemsPtr(i)%ptr%sourceT0FieldPtr%arr1dPtr(j)
                              sourceValueT1           = connection%sourceItemsPtr(i)%ptr%sourceT1FieldPtr%arr1dPtr(j)
                              targetField%arr1dPtr(j) = sourceValueT0*a0 + sourceValueT1*a1
                           enddo
                           targetField%timesteps = timesteps
                        case default
                           call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Unsupported operand type requested.")
                           return
                     end select
                  endif
               end do
            case default
               call setECMessage("ERROR: ec_converter::ecConverterNetcdf: Unsupported interpolation type requested.")
               return
         end select
         success = .true.   
      end function ecConverterNetcdf
      
      ! =======================================================================
      
      !> Selects the index of the polyline segment that intersects with line e--en
      !! with the intersection closest to point e.
      !! The search range is thus from e to en, and not a distance rdis as before.
      !! The normal direction is now
      !! defined by e--en and not normal to the polyline. Also, *all* polyline
      !! segments are checked, not the closest based on dbdistance of pli points.
      subroutine polyindexweight( xe, ye, xen, yen, xs, ys, kcs, ns, kL, wL, kR, wR)
         integer,                intent(in)  :: ns       !< Dimension of polygon OR LINE BOUNDARY
         real(hp), dimension(:), intent(in)  :: xs       !< polygon
         real(hp), dimension(:), intent(in)  :: ys
         integer,  dimension(:), intent(in)  :: kcs      !< polygon mask
         real(hp),               intent(in)  :: xe, ye   ! 
         real(hp),               intent(in)  :: xen, yen !< in input uitstekers, on output SL and CRP
         integer,                intent(out) :: kL       !< Index of left nearest polyline point (with kcs==1!)
         real(hp),               intent(out) :: wL       !< Relative weight of left nearest polyline point.
         integer,                intent(out) :: kR       !< Index of right nearest polyline point (with kcs==1!)
         real(hp),               intent(out) :: wR       !< Relative weight of right nearest polyline point.
         !
         integer      :: k, km, klastvalid, JACROS
         real(hp) :: dis, disM, disL, disR
         real(hp) :: SL,SM,SMM,SLM,XCR,YCR,CRP,CRPM,DEPS
         !
         DISM = huge(DISM)
         kL   = 0 ! Default: No valid point found
         kR   = 0 ! idem
         wL   = 0.0_hp
         wR   = 0.0_hp
         km   = 0
         crpm = 0.0_hp
         disL = 0.0_hp
         disR = 0.0_hp
         DEPS = 1.0E-3_hp
         !
         do k = 1, ns-1
            crp = 0.0_hp
            call CROSS(xe, ye, xen, yen, xs(k), ys(k), xs(k+1), ys(k+1), JACROS,SL,SM,XCR,YCR,CRP)
            if (SL >= 0.0_hp .and. SL <= 1.0_hp .and. SM > -DEPS .and. SM < 1.0_hp+DEPS) then ! instead of jacros==1, solves firmijn's problem  
               DIS = DBDISTANCE(XE, YE, XCR, YCR) 
               if (DIS < DISM) then ! Found a better intersection point
                  DISM = DIS
                  km   = k
                  SMM  = SM
                  SLM  = SL
                  CRPM = CRP
               end if
            end if
         enddo
         !
         if (km > 0) then
            dis  = dbdistance(xs(km), ys(km), xs(km+1), ys(km+1)) ! Length of this segment.
            ! Find nearest valid polyline point left of the intersection (i.e.: kcs(kL) == 1)
            disL = SMM*dis
            do k = km,1,-1
               if (kcs(k) == 1) then
                  kL = k
                  exit ! Valid point on the left (distance was already included in disL)
               else if (k > 1) then
                  disL = disL + dbdistance(xs(k-1), ys(k-1), xs(k), ys(k)) ! Add entire length of this segment.
               end if
            end do
            ! Find nearest valid polyline point right of the intersection (i.e.: kcs(kR) == 1)
            disR = (1.0_hp-SMM)*dis
            do k = km+1,ns
               if (kcs(k) == 1) then
                  kR = k
                  exit ! Valid point on the left (distance was already included in disL)
               else if (k < ns) then
                  disR = disR + dbdistance(xs(k), ys(k), xs(k+1), ys(k+1)) ! Add entire length of this segment.
               end if
            end do
         end if
         !
         if (kL /= 0 .and. kR /= 0) then
            wL = disR/(disL+disR)
            wR = 1.0_hp-wL
         else if (kL /= 0) then
            wL = 1.0_hp
         else if (kR /= 0) then
            wR = 1.0_hp
         end if
      end subroutine polyindexweight
      
      !> Checks whether lines 1-2 and 3-4 intersect.
      !! @param[in] x1,y1,x2,y2,x3,y3,x4,y4 x- and y-coords of line endpoints.
      !! @param[out] jacros 1 if lines cross (intersect), 0 if not.
      !! @param[out] sl lambda in [0,1] on line segment 1-2 (outside [0,1] if no intersection). Unchanged if no intersect!!
      !! @param[out] sm lambda in [0,1] on line segment 3-4 (outside [0,1] if no intersection). Unchanged if no intersect!!
      !! @param[out] xcr,ycr x-coord. of intersection point.
      SUBROUTINE CROSS(x1, y1, x2, y2, x3, y3, x4, y4, JACROS,SL,SM,XCR,YCR,CRP)
         double precision, intent(inout) :: crp !< crp (in)==-1234 will make crp (out) non-dimensional
         double precision :: det
         double precision :: eps
         integer :: jacros, jamakenondimensional
         double precision :: sl
         double precision :: sm
         double precision, intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
         double precision :: x21, y21, x31, y31, x43, y43, xcr, ycr
         double precision                  :: dmiss    = -999d0
     
   !     safety check on crp (in)
         if ( isnan(crp) ) then
            crp = 0.0_hp
         end if

         ! Set defaults for no crossing at all:
         jamakenondimensional = 0
         if ( abs(crp+1234d0).lt.0.5d0 ) then
            jamakenondimensional = 1
            crp = 0.0_hp
         endif
      
         JACROS = 0
         EPS    = 0.00001_hp
         SL     = DMISS
         SM     = DMISS
      
         !call getdxdy(x1,y1,x2,y2,x21,y21) ! TODO : all is cartesian, kernel must provide it as such
         x21 = x2-x1
         y21 = y2-y1
         !call getdxdy(x3,y3,x4,y4,x43,y43) ! TODO : all is cartesian, kernel must provide it as such
         x43 = x4-x3
         y43 = y4-y3
         !call getdxdy(x1,y1,x3,y3,x31,y31) ! TODO : all is cartesian, kernel must provide it as such
         x31 = x3-x1
         y31 = y3-y1

         DET =  X43*Y21 - Y43*X21

   !     SPvdP: make eps have proper dimension
         EPS = max(EPS*MAXVAL((/ X21,Y21,X43,Y43,X31,Y31 /)), tiny(0.0_hp))
         IF (ABS(DET) .LT. EPS) THEN
            RETURN
         ELSE
            SM = (Y31*X21 - X31*Y21) / DET
            IF (ABS(X21) .GT. EPS) THEN
               SL = (SM*X43 + X31) / X21
            ELSE IF (ABS(Y21) .GT. EPS) THEN
               SL = (SM*Y43 + Y31) / Y21
            ELSE
               SL   = 0.0_hp
            ENDIF
            IF (SM .GE. 0.0_hp .AND. SM .LE. 1d0 .AND. &
                SL .GE. 0.0_hp .AND. SL .LE. 1d0) THEN
               JACROS = 1
            ENDIF
            XCR = X1 + SL*(X2-X1)
            YCR = Y1 + SL*(Y2-Y1)
            if ( jamakenondimensional.eq.1 ) then  ! make crp non-dimensional (for spline2curvi)
               CRP = -DET / ( sqrt(x21**2+y21**2) * sqrt(x43**2 + y43**2) + 1d-8 )
            else
               CRP = -DET
            end if
         ENDIF
         RETURN
      END SUBROUTINE CROSS
      
      !> distance point 1 -> 2
      real(hp) function dbdistance(x1,y1,x2,y2)
         real(hp) :: x1, y1, x2, y2
         ! locals
         real(hp) :: ddx, ddy, rr
         ! double precision :: getdx, getdy ! TODO : all is cartesian, kernel must provide it as such
         real(hp) :: dmiss    = -999d0
         !
         if ( x1.eq.DMISS .or. x2.eq.DMISS .or. y1.eq.DMISS .or. y2.eq.DMISS ) then
            dbdistance = 0.0_hp
            return
         end if
         !
         !ddx = getdx(x1,y1,x2,y2) ! TODO : all is cartesian, kernel must provide it as such
         ddx = x2-x1
         !ddy = getdy(x1,y1,x2,y2) ! TODO : all is cartesian, kernel must provide it as such
         ddy = y2-y1
         rr  = ddx*ddx + ddy*ddy
         if (rr == 0.0_hp) then
            dbdistance = 0.0_hp
         else
            dbdistance = sqrt(rr)
         endif
      end function dbdistance
end module m_ec_converter
