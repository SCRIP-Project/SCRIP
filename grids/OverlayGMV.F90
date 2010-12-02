!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

       program overlay_gmv

!BOP
! !PROGRAM: SCRIP_driver
!
! !DESCRIPTION:
!  This program is a standalone driver for writing a pair of NETCDF
!  spherical grids out in GMV format
!
!  The program reads the two file names from the command line
!  To convert just one netcdf grid file to GMV, repeat the filename 1
!  when queried for the second file
!
! !REVISION HISTORY:
!  SVN:$Id: $

! !USES:

      use SCRIP_KindsMod             ! module defining data types
      use SCRIP_ErrorMod             ! SCRIP error checking and logging
      use SCRIP_IOUnitsMod           ! manages I/O units
      use grids                      ! module with grid information

      implicit none

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

       integer (SCRIP_i4) :: &
        errorCode      ! error flag

       character (SCRIP_charLength) :: & 
        gridFile1,    &! filename of grid file containing grid1
        gridFile2,    &! filename of grid file containing grid2
        interpFile1,  &! filename for output remap data (map1)
        interpFile2,  &! filename for output remap data (map2)
        mapName1,     &! name for mapping from grid1 to grid2
        mapName2,     &! name for mapping from grid2 to grid1
        mapMethod,    &! choice for mapping method
        normalizeOpt, &! option for normalizing weights
        outputFormat   ! option for output conventions

!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

       integer (SCRIP_i4) :: &
        n,            &! dummy counter
        iunit          ! unit number for input configuration file

       integer (SCRIP_i4) :: i, j, j1, k, grid1_add, grid2_add, nseg=6
       integer (SCRIP_i4), dimension(:), allocatable :: &
                          grid1_imask, grid2_imask
       real (SCRIP_r8) :: lat, lon, dlat, dlon
       real (SCRIP_r8), dimension(:,:), allocatable :: &
       grid1_corner_x, grid1_corner_y, grid1_corner_z, &
       grid2_corner_x, grid2_corner_y, grid2_corner_z, &
       grid1_corner_xp, grid2_corner_xp, &
       grid1_corner_yp, grid2_corner_yp, &
       grid1_corner_xp2, grid2_corner_xp2, &
       grid1_corner_yp2, grid2_corner_yp2


!-----------------------------------------------------------------------
!
!  initialize communication environment and SCRIP package
!
!-----------------------------------------------------------------------

       errorCode = SCRIP_Success
       restrict_type = 'latitude'

!-----------------------------------------------------------------------
!
!  read input namelist
!
!-----------------------------------------------------------------------

      write(SCRIP_stdout,*) 'Grid 1 File name'
      read(SCRIP_stdin,*) gridFile1
      write(SCRIP_stdout,*) 'Grid 2 File name'
      read(SCRIP_stdin,*) gridFile2

!-----------------------------------------------------------------------
!
!  initialize grid information for both grids
!
!-----------------------------------------------------------------------

      call grid_init(gridFile1, gridFile2, errorCode)

      if (errorCode /= SCRIP_Success) then
         write(SCRIP_stdout,*) &
                          'SCRIP_driver: Error initializing grids'
         stop
      endif

      write(SCRIP_stdout, *) 'Write out combined GMV file for ', &
                          grid1_name, ' and ', grid2_name
      write(SCRIP_stdout, *) 'Lat-Lon overlay in file grids_ll.gmv'
      write(SCRIP_stdout, *) 'XYZ overlay in file grids_xyz.gmv'


!-----------------------------------------------------------------------
!
!  write out grids in GMV format
!
!-----------------------------------------------------------------------


      open(UNIT=11, FILE="grids_ll.gmv")  ! Lon-Lat
      open(UNIT=12, FILE="grids_xyz.gmv") ! XYZ
      open(UNIT=13, FILE="grids_xyp.gmv") ! Lambert
      open(UNIT=14, FILE="grids_xyp2.gmv") ! Lambert with two segs
      open(UNIT=15, FILE="grids_llc.gmv") ! LonCoslat-Lat
      open(UNIT=16, FILE="grids_llc2.gmv") !LonCoslat-Lat with two segs

      write(11,'(A14)')'gmvinput ascii'
      write(12,'(A14)')'gmvinput ascii'
      write(13,'(A14)')'gmvinput ascii'
      write(14,'(A14)')'gmvinput ascii'
      write(15,'(A14)')'gmvinput ascii'
      write(16,'(A14)')'gmvinput ascii'


      allocate(grid1_corner_x(grid1_corners,grid1_size), &
               grid1_corner_y(grid1_corners,grid1_size), &
               grid1_corner_z(grid1_corners,grid1_size), &
               grid2_corner_x(grid2_corners,grid2_size), &
               grid2_corner_y(grid2_corners,grid2_size), &
               grid2_corner_z(grid2_corners,grid2_size), &
               grid1_corner_xp(grid1_corners,grid1_size), &
               grid1_corner_yp(grid1_corners,grid1_size), &
               grid2_corner_xp(grid2_corners,grid2_size), &
               grid2_corner_yp(grid2_corners,grid2_size), &
               grid1_corner_xp2(nseg*grid1_corners,grid1_size), &
               grid1_corner_yp2(nseg*grid1_corners,grid1_size), &
               grid2_corner_xp2(nseg*grid2_corners,grid2_size), &
               grid2_corner_yp2(nseg*grid2_corners,grid2_size))
      allocate(grid1_imask(grid1_size),grid2_imask(grid2_size))


      grid1_corner_x = cos(grid1_corner_lat)*cos(grid1_corner_lon)
      grid1_corner_y = cos(grid1_corner_lat)*sin(grid1_corner_lon)
      grid1_corner_z = sin(grid1_corner_lat)
      grid2_corner_x = cos(grid2_corner_lat)*cos(grid2_corner_lon)
      grid2_corner_y = cos(grid2_corner_lat)*sin(grid2_corner_lon)
      grid2_corner_z = sin(grid2_corner_lat)


      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !******* Mapping ***********
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      do i = 1, grid1_size
        if (grid1_center_lat(i) > pi/4.0) then
           k = 0
           do j = 1, grid1_corners              
              lat = grid1_corner_lat(j,i)
              lon = grid1_corner_lon(j,i)
              grid1_corner_xp(j,i) =  2.0*sin(0.25*pi-0.5*lat)*cos(lon)
              grid1_corner_yp(j,i) =  2.0*sin(0.25*pi-0.5*lat)*sin(lon)

              j1 = 1 + mod(j,grid1_corners)
              dlat = grid1_corner_lat(j1,i)-grid1_corner_lat(j,i)
              dlon = grid1_corner_lon(j1,i)-grid1_corner_lon(j,i)
              if (dlon > pi) then
                 dlon = dlon - pi2
              else if (dlon < -pi) then
                 dlon = dlon + pi2              
              endif

              do n = 1, nseg
                 k = k+1
                 lat = grid1_corner_lat(j,i) + dlat*(n-1)/nseg
                 lon = grid1_corner_lon(j,i) + dlon*(n-1)/nseg
                 grid1_corner_xp2(k,i) =  &
                     2.0*sin(0.25*pi-0.5*lat)*cos(lon)
                 grid1_corner_yp2(k,i) =  &
                     2.0*sin(0.25*pi-0.5*lat)*sin(lon)
              enddo
           enddo
        else if (grid1_center_lat(i) < -pi/4.0) then
           k = 0
           do j = 1, grid1_corners
              lat = grid1_corner_lat(j,i)
              lon = grid1_corner_lon(j,i)
              grid1_corner_xp(j,i) = -2.0*sin(-0.25*pi-0.5*lat)*cos(lon)
              grid1_corner_yp(j,i) =  2.0*sin(-0.25*pi-0.5*lat)*sin(lon)
              grid1_corner_yp(j,i) = grid1_corner_yp(j,i) - 2.5

              j1 = 1 + mod(j,grid1_corners)
              dlat = grid1_corner_lat(j1,i)-grid1_corner_lat(j,i)
              dlon = grid1_corner_lon(j1,i)-grid1_corner_lon(j,i)
              if (dlon > pi) then
                 dlon = dlon - pi2
              else if (dlon < -pi) then
                 dlon = dlon + pi2              
              endif

              do n = 1, nseg
                 k = k+1
                 lat = grid1_corner_lat(j,i) + dlat*(n-1)/nseg
                 lon = grid1_corner_lon(j,i) + dlon*(n-1)/nseg
                 grid1_corner_xp2(k,i) =  &
                     -2.0*sin(-0.25*pi-0.5*lat)*cos(lon)
                 grid1_corner_yp2(k,i) =  &
                      2.0*sin(-0.25*pi-0.5*lat)*sin(lon) - 2.5
              enddo

           enddo
        endif
      enddo 

      do i = 1, grid2_size
        if (grid2_center_lat(i) > pi/4.0) then
           k = 0
           do j = 1, grid2_corners
              lat = grid2_corner_lat(j,i)
              lon = grid2_corner_lon(j,i)
              grid2_corner_xp(j,i) =  2.0*sin(0.25*pi-0.5*lat)*cos(lon)
              grid2_corner_yp(j,i) =  2.0*sin(0.25*pi-0.5*lat)*sin(lon)

              j1 = 1 + mod(j,grid2_corners)
              dlat = grid2_corner_lat(j1,i)-grid2_corner_lat(j,i)
              dlon = grid2_corner_lon(j1,i)-grid2_corner_lon(j,i)
              if (dlon > pi) then
                 dlon = dlon - pi2
              else if (dlon < -pi) then
                 dlon = dlon + pi2              
              endif

              do n = 1, nseg
                 k = k+1
                 lat = grid2_corner_lat(j,i) + dlat*(n-1)/nseg
                 lon = grid2_corner_lon(j,i) + dlon*(n-1)/nseg
                 grid2_corner_xp2(k,i) =  &
                     2.0*sin(0.25*pi-0.5*lat)*cos(lon)
                 grid2_corner_yp2(k,i) =  &
                     2.0*sin(0.25*pi-0.5*lat)*sin(lon)
              enddo
           enddo
        else if (grid2_center_lat(i) < -pi/4.0) then
           k = 0
           do j = 1, grid2_corners
              lat = grid2_corner_lat(j,i)
              lon = grid2_corner_lon(j,i)
              grid2_corner_xp(j,i) = -2.0*sin(-0.25*pi-0.5*lat)*cos(lon)
              grid2_corner_yp(j,i) =  2.0*sin(-0.25*pi-0.5*lat)*sin(lon)
              grid2_corner_yp(j,i) = grid2_corner_yp(j,i) - 2.5

              j1 = 1 + mod(j,grid2_corners)
              dlat = grid2_corner_lat(j1,i)-grid2_corner_lat(j,i)
              dlon = grid2_corner_lon(j1,i)-grid2_corner_lon(j,i)
              if (dlon > pi) then
                 dlon = dlon - pi2
              else if (dlon < -pi) then
                 dlon = dlon + pi2              
              endif

              do n = 1, nseg
                 k = k+1
                 lat = grid2_corner_lat(j,i) + dlat*(n-1)/nseg
                 lon = grid2_corner_lon(j,i) + dlon*(n-1)/nseg
                 grid2_corner_xp2(k,i) =  &
                     -2.0*sin(-0.25*pi-0.5*lat)*cos(lon)
                 grid2_corner_yp2(k,i) =  &
                      2.0*sin(-0.25*pi-0.5*lat)*sin(lon) - 2.5
              enddo

           enddo
        endif
      enddo 



      grid1_imask = 0
      do i = 1, grid1_size
        if (grid1_mask(i)) grid1_imask(i) = 1
      enddo
      grid2_imask = 0
      do i = 1, grid2_size
        if (grid2_mask(i)) grid2_imask(i) = 1
      enddo

      
      n = grid1_corners*grid1_size + grid2_corners*grid2_size
      write(11,'(A6,I10)')'nodev ',n
      write(12,'(A6,I10)')'nodev ',n
      write(13,'(A6,I10)')'nodev ',n
      write(14,'(A6,I10)')'nodev ',nseg*n

      do grid1_add = 1, grid1_size
         do i = 1, grid1_corners
            write(11,*) grid1_corner_lon(i,grid1_add), &
                 grid1_corner_lat(i,grid1_add),0.0
            write(12,*) grid1_corner_x(i,grid1_add), &
                 grid1_corner_y(i,grid1_add), &
                 grid1_corner_z(i,grid1_add)
            write(13,*) grid1_corner_yp(i,grid1_add), &
                 grid1_corner_xp(i,grid1_add),0.0
         enddo
         do i = 1, nseg*grid1_corners
            write(14,*) grid1_corner_yp2(i,grid1_add), &
                 grid1_corner_xp2(i,grid1_add),0.0
         enddo
      enddo
      do grid2_add = 1, grid2_size
         do i = 1, grid2_corners
            write(11,*) grid2_corner_lon(i,grid2_add), &
                 grid2_corner_lat(i,grid2_add),0.0
            write(12,*) grid2_corner_x(i,grid2_add), &
                 grid2_corner_y(i,grid2_add), &
                 grid2_corner_z(i,grid2_add)
            write(13,*) grid2_corner_yp(i,grid2_add), &
                 grid2_corner_xp(i,grid2_add),0.0
         enddo
         do i = 1, nseg*grid2_corners
            write(14,*) grid2_corner_yp2(i,grid2_add), &
                 grid2_corner_xp2(i,grid2_add),0.0
         enddo
      enddo



      write(11,'(A6,I10)') 'cells ',grid1_size+grid2_size
      write(12,'(A6,I10)') 'cells ',grid1_size+grid2_size
      write(13,'(A6,I10)') 'cells ',grid1_size+grid2_size
      write(14,'(A6,I10)') 'cells ',grid1_size+grid2_size

      n = 1
      if (grid1_corners .eq. 3) then
        do grid1_add = 1, grid1_size
          write(11,'(A6,20I10)') 'tri 3 ', (i, i=n,n+grid1_corners-1)
          write(12,'(A6,20I10)') 'tri 3 ', (i, i=n,n+grid1_corners-1)
          write(13,'(A6,20I10)') 'tri 3 ', (i, i=n,n+grid1_corners-1)
          n = n+grid1_corners
        enddo
      else if (grid1_corners .eq. 4) then
        do grid1_add = 1, grid1_size
          write(11,'(A6,20I10)') 'quad 4 ', (i, i=n,n+grid1_corners-1)
          write(12,'(A6,20I10)') 'quad 4 ', (i, i=n,n+grid1_corners-1)
          write(13,'(A6,20I10)') 'quad 4 ', (i, i=n,n+grid1_corners-1)
          n = n+grid1_corners
        enddo
      else
        do grid1_add = 1, grid1_size
           write(11,'(A10)') 'general 1 '
           write(11,*) grid1_corners
           write(11,*) (i, i=n,n+grid1_corners-1)
           write(12,'(A10)') 'general 1 '
           write(12,*) grid1_corners
           write(12,*) (i, i=n,n+grid1_corners-1)
           write(13,'(A10)') 'general 1 '
           write(13,*) grid1_corners
           write(13,*) (i, i=n,n+grid1_corners-1)
           n = n+grid1_corners
        enddo
      endif
      if (grid2_corners .eq. 3) then
        do grid2_add = 1, grid2_size
          write(11,'(A6,20I10)') 'tri 3 ', (i, i=n,n+grid2_corners-1)
          write(12,'(A6,20I10)') 'tri 3 ', (i, i=n,n+grid2_corners-1)
          write(13,'(A6,20I10)') 'tri 3 ', (i, i=n,n+grid2_corners-1)
          n = n+grid2_corners
        enddo
      else if (grid2_corners .eq. 4) then
        do grid2_add = 1, grid2_size
          write(11,'(A6,20I10)') 'quad 4 ', (i, i=n,n+grid2_corners-1)
          write(12,'(A6,20I10)') 'quad 4 ', (i, i=n,n+grid2_corners-1)
          write(13,'(A6,20I10)') 'quad 4 ', (i, i=n,n+grid2_corners-1)
          n = n+grid2_corners
        enddo
      else
        do grid2_add = 1, grid2_size
           write(11,'(A10)') 'general 1 '
           write(11,*) grid2_corners
           write(11,*) (i, i=n,n+grid2_corners-1)
           write(12,'(A10)') 'general 1 '
           write(12,*) grid2_corners
           write(12,*) (i, i=n,n+grid2_corners-1)
           write(13,'(A10)') 'general 1 '
           write(13,*) grid2_corners
           write(13,*) (i, i=n,n+grid2_corners-1)
           n = n+grid2_corners
        enddo
      endif

      n = 1
      do grid1_add = 1, grid1_size
           write(14,'(A10)') 'general 1 '
           write(14,*) nseg*grid1_corners
           write(14,*) (i, i=n,n+nseg*grid1_corners-1)
         n = n+nseg*grid1_corners
      enddo
      do grid2_add = 1, grid2_size
           write(14,'(A10)') 'general 1 '
           write(14,*) nseg*grid2_corners
           write(14,*) (i, i=n,n+nseg*grid2_corners-1)
         n = n+nseg*grid2_corners
      enddo

      write(11,'(A12)') 'material 2 0'
      write(11,'(A5)') 'grid1'
      write(11,'(A5)') 'grid2'
      write(12,'(A12)') 'material 2 0'
      write(12,'(A5)') 'grid1'
      write(12,'(A5)') 'grid2'
      write(13,'(A12)') 'material 2 0'
      write(13,'(A5)') 'grid1'
      write(13,'(A5)') 'grid2'
      write(14,'(A12)') 'material 2 0'
      write(14,'(A5)') 'grid1'
      write(14,'(A5)') 'grid2'

      write(11,'(10I2)') (1, i = 1,grid1_size)
      write(11,'(10I2)') (2, i = 1,grid2_size)
      write(12,'(10I2)') (1, i = 1,grid1_size)
      write(12,'(10I2)') (2, i = 1,grid2_size)
      write(13,'(10I2)') (1, i = 1,grid1_size)
      write(13,'(10I2)') (2, i = 1,grid2_size)
      write(14,'(10I2)') (1, i = 1,grid1_size)
      write(14,'(10I2)') (2, i = 1,grid2_size)

      write(11,'(A8)') 'variable'
      write(11,'(A10)') 'grid_mask 0'
      write(11,'(10I2)') (grid1_imask(i), i = 1,grid1_size)
      write(11,'(10I2)') (grid2_imask(i), i = 1,grid2_size)
      write(12,'(A8)') 'variable'
      write(12,'(A10)') 'grid_mask 0'
      write(12,'(10I2)') (grid1_imask(i), i = 1,grid1_size)
      write(12,'(10I2)') (grid2_imask(i), i = 1,grid2_size)
      write(13,'(A8)') 'variable'
      write(13,'(A10)') 'grid_mask 0'
      write(13,'(10I2)') (grid1_imask(i), i = 1,grid1_size)
      write(13,'(10I2)') (grid2_imask(i), i = 1,grid2_size)
      write(14,'(A8)') 'variable'
      write(14,'(A10)') 'grid_mask 0'
      write(14,'(10I2)') (grid1_imask(i), i = 1,grid1_size)
      write(14,'(10I2)') (grid2_imask(i), i = 1,grid2_size)

      write(11,'(A7)') 'endvars'
      write(12,'(A7)') 'endvars'
      write(13,'(A7)') 'endvars'
      write(14,'(A7)') 'endvars'

      write(11,'(A6)') 'endgmv'
      write(12,'(A6)') 'endgmv'
      write(13,'(A6)') 'endgmv'
      write(14,'(A6)') 'endgmv'

      close(11)
      close(12)
      close(13)
      close(14)
      

      end program overlay_gmv

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
