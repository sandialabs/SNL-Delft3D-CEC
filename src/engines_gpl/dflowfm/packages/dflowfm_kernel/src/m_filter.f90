module m_filter
   use m_solver
   
   type(tsolver) :: solver_filter
 
   double precision, dimension(:), allocatable :: ALvec2  !< vector Laplacian in CRS format, matrix entries
   
   integer,          dimension(:), allocatable :: iLvec  !< vector Laplacian in CRS format, startpointers
   integer,          dimension(:), allocatable :: jLvec  !< vector Laplacian in CRS format, row numbers
   double precision, dimension(:), allocatable :: ALvec  !< vector Laplacian in CRS format, matrix entries

   double precision, dimension(:), allocatable :: sol      !< solution of "filter" solve, dim(Lnx)
   double precision, dimension(:), allocatable :: ustar    !< predictor, dim(Lnkx)
   double precision, dimension(:,:), allocatable :: eps       !< filter coefficient, dim(kmx,Lnx)
   double precision, dimension(:), allocatable :: dtmaxeps  !< maximum time step multiplied with filter coefficient, dim(Lnx)
   double precision, dimension(:), allocatable :: Deltax    !< typical mesh width, dim(Lnx)
   double precision, dimension(:), allocatable :: checkmonitor   !< "checkerboard" mode monitor, dim(kmx+1)
   
   double precision, dimension(:), allocatable :: workin      !< work array, dim(kmx+1)
   double precision, dimension(:), allocatable :: workout     !< work array, dim(kmx+1)
   
   integer                                     :: order    !< order, 1st (1) or 3rd (3)
   integer                                     :: itype    !< explicit (1), implicit (2), implicit with hor. terms (3), no filter (0)
                     
   integer                                     :: jadebug = 0
   integer,          parameter                 :: LENFILNAM = 128
   character(len=LENFILNAM)                    :: FNAM      
   integer,          dimension(:), allocatable :: num
   integer,          dimension(:), allocatable :: dum
end module m_filter