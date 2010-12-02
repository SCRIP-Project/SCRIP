!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     this program is a short driver that tests the remappings using
!     a simple analytic field.  the results are written in netCDF
!     format.
!
!     CVS: $Id: scrip_test.f,v 1.6 2000/04/19 21:45:09 pwjones Exp $
!
!-----------------------------------------------------------------------
!
!     Copyright (c) 1997, 1998 the Regents of the University of 
!       California.
!
!     This software and ancillary information (herein called software) 
!     called SCRIP is made available under the terms described here.  
!     The software has been approved for release with associated 
!     LA-CC Number 98-45.
!
!     Unless otherwise indicated, this software has been authored
!     by an employee or employees of the University of California,
!     operator of the Los Alamos National Laboratory under Contract
!     No. W-7405-ENG-36 with the U.S. Department of Energy.  The U.S.
!     Government has rights to use, reproduce, and distribute this
!     software.  The public may copy and use this software without
!     charge, provided that this Notice and any statement of authorship
!     are reproduced on all copies.  Neither the Government nor the
!     University makes any warranty, express or implied, or assumes
!     any liability or responsibility for the use of this software.
!
!     If software is modified to produce derivative works, such modified
!     software should be clearly marked, so as not to confuse it with 
!     the version available from Los Alamos National Laboratory.
!
!***********************************************************************

      program remap_test

!-----------------------------------------------------------------------

      use SCRIP_KindsMod   ! defines common data types
      use SCRIP_CommMod    ! for SCRIP parallel environment
      use SCRIP_ErrorMod   ! SCRIP error checking and logging
      use SCRIP_IOUnitsMod ! manages I/O units
      use SCRIP_InitMod    ! initialize SCRIP
      use constants    ! defines common constants
      use netcdf_mod   ! netcdf I/O stuff
      use netcdf
      use grids        ! module containing grid info
      use remap_vars   ! module containing remapping info
      use remap_mod    ! module containing remapping routines
      use remap_read   ! routines for reading remap files

      implicit none

!-----------------------------------------------------------------------
!
!     input namelist variables
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4) ::
     &  field_choice   ! choice of field to be interpolated

      character (SCRIP_charLength) :: 
     &  interp_file,   ! filename containing remap data (map1)
     &  output_file    ! filename for test results

      namelist /remap_inputs/ field_choice, interp_file, output_file

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4) ::
     &   errorCode

      character (SCRIP_charLength) :: 
     &        map_name      ! name for mapping from grid1 to grid2

      integer (SCRIP_i4) ::    ! netCDF ids for files and arrays
     &        ncstat, nc_outfile_id, 
     &        nc_srcgrdcntrlat_id, nc_srcgrdcntrlon_id,
     &        nc_dstgrdcntrlat_id, nc_dstgrdcntrlon_id,
     &        nc_srcgrdimask_id, nc_dstgrdimask_id,
     &        nc_srcgrdarea_id, nc_dstgrdarea_id,
     &        nc_srcgrdfrac_id, nc_dstgrdfrac_id,
     &        nc_srcarray_id, nc_srcgradlat_id, nc_srcgradlon_id,
     &        nc_dstarray1_id, nc_dstarray1a_id, nc_dstarray2_id,
     &        nc_dsterror1_id, nc_dsterror1a_id, nc_dsterror2_id

      integer (SCRIP_i4), dimension(:), allocatable ::
     &        nc_grid1size_id, nc_grid2size_id

!-----------------------------------------------------------------------

      character (SCRIP_charLength) :: 
     &          dim_name    ! netCDF dimension name

      integer (SCRIP_i4) :: i,j,n,imin,imax,idiff,
     &    ip1,im1,jp1,jm1,nx,ny, ! for computing bicub gradients
     &    in,is,ie,iw,ine,inw,ise,isw,
     &    iunit                  ! unit number for namelist file

      integer (SCRIP_i4), dimension(:), allocatable ::
     &    grid1_imask, grid2_imask, grid2_count

      real (SCRIP_r8) ::
     &    delew, delns,     ! variables for computing bicub gradients
     &    length            ! length scale for cosine hill test field

      real (SCRIP_r8), dimension(:), allocatable ::
     &    grid1_array, 
     &    grid1_tmp, 
     &    grad1_lat, 
     &    grad1_lon, 
     &    grad1_latlon, 
     &    grad1_lat_zero, 
     &    grad1_lon_zero, 
     &    grid2_array, 
     &    grid2_err,
     &    grid2_tmp

      real (SCRIP_i4), dimension(:,:), allocatable ::
     &    grid1itmp2d, grid2itmp2d

      real (SCRIP_r8), dimension(:,:), allocatable ::
     &    grid1tmp2d, grid2tmp2d

!-----------------------------------------------------------------------
!
!     initialize SCRIP
!
!-----------------------------------------------------------------------

      errorCode = SCRIP_Success
      call SCRIP_CommInitMessageEnvironment
      call SCRIP_Initialize(errorCode)
      if (SCRIP_errorCheck(errorCode,'error initializing SCRIP')) then
         call SCRIP_ErrorPrint(errorCode, SCRIP_masterTask)
         call SCRIP_CommExitMessageEnvironment
         stop
      endif

!-----------------------------------------------------------------------
!
!     read namelist for file and mapping info
!
!-----------------------------------------------------------------------

      call SCRIP_IOUnitsGet(iunit)
      open(iunit, file='scrip_test_in', status='old', form='formatted')
      read(iunit, nml=remap_inputs)
      call SCRIP_IOUnitsRelease(iunit)
      write(*,nml=remap_inputs)

!-----------------------------------------------------------------------
!
!     read remapping data
!
!-----------------------------------------------------------------------

      call read_remap(map_name, interp_file)

!-----------------------------------------------------------------------
!
!     allocate arrays
!
!-----------------------------------------------------------------------

      allocate (grid1_array    (grid1_size), 
     &          grid1_tmp      (grid1_size),
     &          grad1_lat      (grid1_size), 
     &          grad1_lon      (grid1_size), 
     &          grad1_lat_zero (grid1_size), 
     &          grad1_lon_zero (grid1_size), 
     &          grid1_imask    (grid1_size),
     &          grid2_array    (grid2_size), 
     &          grid2_err      (grid2_size),
     &          grid2_tmp      (grid2_size),
     &          grid2_imask    (grid2_size),
     &          grid2_count    (grid2_size))

      where (grid1_mask)
        grid1_imask = 1
      elsewhere
        grid1_imask = 0
      endwhere
      where (grid2_mask)
        grid2_imask = 1
      elsewhere
        grid2_imask = 0
      endwhere

!-----------------------------------------------------------------------
!
!     setup a NetCDF file for output
!
!-----------------------------------------------------------------------

      !***
      !*** create netCDF dataset 
      !***

      ncstat = nf90_create (output_file, NF90_CLOBBER, nc_outfile_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_put_att(nc_outfile_id, NF90_GLOBAL, 'title',
     &                      map_name)
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid size dimensions
      !***

      allocate( nc_grid1size_id(grid1_rank),
     &          nc_grid2size_id(grid2_rank))


      do n=1,grid1_rank
        write(dim_name,1000) 'grid1_dim',n
        ncstat = nf90_def_dim(nc_outfile_id, dim_name, 
     &                        grid1_dims(n), nc_grid1size_id(n))
        call netcdf_error_handler(ncstat)
      end do

      do n=1,grid2_rank
        write(dim_name,1000) 'grid2_dim',n
        ncstat = nf90_def_dim(nc_outfile_id, dim_name, 
     &                        grid2_dims(n), nc_grid2size_id(n))
        call netcdf_error_handler(ncstat)
      end do
 1000 format(a9,i1)

      !***
      !*** define grid center latitude array
      !***

      ncstat = nf90_def_var(nc_outfile_id, 'src_grid_center_lat', 
     &                      NF90_DOUBLE, nc_grid1size_id, 
     &                      nc_srcgrdcntrlat_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_put_att(nc_outfile_id, nc_srcgrdcntrlat_id, 
     &                      'units', 'radians')
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'dst_grid_center_lat', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dstgrdcntrlat_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_put_att(nc_outfile_id, nc_dstgrdcntrlat_id, 
     &                      'units', 'radians')
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid center longitude array
      !***

      ncstat = nf90_def_var(nc_outfile_id, 'src_grid_center_lon', 
     &                      NF90_DOUBLE, nc_grid1size_id, 
     &                      nc_srcgrdcntrlon_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_put_att(nc_outfile_id, nc_srcgrdcntrlon_id, 
     &                      'units', 'radians')
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'dst_grid_center_lon', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dstgrdcntrlon_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_put_att(nc_outfile_id, nc_dstgrdcntrlon_id, 
     &                      'units', 'radians')
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid mask
      !***

      ncstat = nf90_def_var(nc_outfile_id, 'src_grid_imask', NF90_INT,
     &                      nc_grid1size_id, nc_srcgrdimask_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_put_att(nc_outfile_id, nc_srcgrdimask_id, 
     &                      'units', 'unitless')
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'dst_grid_imask', NF90_INT,
     &                      nc_grid2size_id, nc_dstgrdimask_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_put_att(nc_outfile_id, nc_dstgrdimask_id, 
     &                      'units', 'unitless')
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid area arrays
      !***

      ncstat = nf90_def_var(nc_outfile_id, 'src_grid_area', 
     &                      NF90_DOUBLE, nc_grid1size_id, 
     &                      nc_srcgrdarea_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'dst_grid_area', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dstgrdarea_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid fraction arrays
      !***

      ncstat = nf90_def_var(nc_outfile_id, 'src_grid_frac', 
     &                      NF90_DOUBLE, nc_grid1size_id, 
     &                      nc_srcgrdfrac_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'dst_grid_frac', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dstgrdfrac_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** define source array
      !***

      ncstat = nf90_def_var(nc_outfile_id, 'src_array', 
     &                      NF90_DOUBLE, nc_grid1size_id, 
     &                      nc_srcarray_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** define gradient arrays
      !***

      ncstat = nf90_def_var(nc_outfile_id, 'src_grad_lat', 
     &                      NF90_DOUBLE, nc_grid1size_id, 
     &                      nc_srcgradlat_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'src_grad_lon', 
     &                      NF90_DOUBLE, nc_grid1size_id, 
     &                      nc_srcgradlon_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** define destination arrays
      !***

      ncstat = nf90_def_var(nc_outfile_id, 'dst_array1', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dstarray1_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'dst_array1a', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dstarray1a_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'dst_array2', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dstarray2_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** define error arrays
      !***

      ncstat = nf90_def_var(nc_outfile_id, 'dst_error1', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dsterror1_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'dst_error1a', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dsterror1a_id)
      call netcdf_error_handler(ncstat)

      ncstat = nf90_def_var(nc_outfile_id, 'dst_error2', 
     &                      NF90_DOUBLE, nc_grid2size_id, 
     &                      nc_dsterror2_id)
      call netcdf_error_handler(ncstat)

      !***
      !*** end definition stage
      !***

      ncstat = nf90_enddef(nc_outfile_id)
      call netcdf_error_handler(ncstat)

!-----------------------------------------------------------------------
!
!     write some grid info
!
!-----------------------------------------------------------------------

      !***
      !*** write grid center latitude array
      !***

      allocate(grid1tmp2d(grid1_dims(1),grid1_dims(2)),
     &         grid2tmp2d(grid2_dims(1),grid2_dims(2)))
      n = 0
      do j=1,grid1_dims(2)
      do i=1,grid1_dims(1)
         n = n+1
         grid1tmp2d(i,j) = grid1_center_lat(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_srcgrdcntrlat_id,
     &                      grid1tmp2d)
      call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_center_lat(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_dstgrdcntrlat_id,
     &                      grid2tmp2d)
      call netcdf_error_handler(ncstat)

      !***
      !*** write grid center longitude array
      !***

      n = 0
      do j=1,grid1_dims(2)
      do i=1,grid1_dims(1)
         n = n+1
         grid1tmp2d(i,j) = grid1_center_lon(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_srcgrdcntrlon_id,
     &                      grid1tmp2d)
      call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_center_lon(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_dstgrdcntrlon_id,
     &                      grid2tmp2d)
      call netcdf_error_handler(ncstat)

      !***
      !*** write grid mask
      !***

      allocate(grid1itmp2d(grid1_dims(1),grid1_dims(2)),
     &         grid2itmp2d(grid2_dims(1),grid2_dims(2)))
      n = 0
      do j=1,grid1_dims(2)
      do i=1,grid1_dims(1)
         n = n+1
         grid1itmp2d(i,j) = grid1_imask(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_srcgrdimask_id,
     &                      grid1itmp2d)
      call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2itmp2d(i,j) = grid2_imask(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_dstgrdimask_id,
     &                      grid2itmp2d)
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid area arrays
      !***

      n = 0
      do j=1,grid1_dims(2)
      do i=1,grid1_dims(1)
         n = n+1
         grid1tmp2d(i,j) = grid1_area(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_srcgrdarea_id,
     &                      grid1tmp2d)
      call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_area(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_dstgrdarea_id,
     &                      grid2tmp2d)
      call netcdf_error_handler(ncstat)

      !***
      !*** define grid fraction arrays
      !***

      n = 0
      do j=1,grid1_dims(2)
      do i=1,grid1_dims(1)
         n = n+1
         grid1tmp2d(i,j) = grid1_frac(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_srcgrdfrac_id,
     &                      grid1tmp2d)
      call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_frac(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_dstgrdfrac_id,
     &                      grid2tmp2d)
      call netcdf_error_handler(ncstat)

!-----------------------------------------------------------------------
!
!     set up fields for test cases based on user choice
!
!-----------------------------------------------------------------------

      select case (field_choice)
      case(1)  !*** cosine hill at lon=pi and lat=0

        length = 0.1*pi2

        grid1_array = cos(grid1_center_lat)*cos(grid1_center_lon)
        grid2_array = cos(grid2_center_lat)*cos(grid2_center_lon)

        grid1_tmp = acos(-grid1_array)/length
        grid2_tmp = acos(-grid2_array)/length

        where (grid1_tmp <= one)
          grad1_lat   = (pi/length)*sin(pi*grid1_tmp)*
     &                  sin(grid1_center_lat)*cos(grid1_center_lon)/
     &                  sqrt(one-grid1_array**2)
          grad1_lon   = (pi/length)*sin(pi*grid1_tmp)*
     &                  sin(grid1_center_lon)/
     &                  sqrt(one-grid1_array**2)
          grid1_array = two + cos(pi*grid1_tmp)
        elsewhere
          grid1_array = one
          grad1_lat   = zero
          grad1_lon   = zero
        endwhere
        
        where (grid2_tmp <= one)
          grid2_array = two + cos(pi*grid2_tmp)
        elsewhere
          grid2_array = one
        endwhere
        
        where (.not. grid1_mask)
          grid1_array = zero
          grad1_lat   = zero
          grad1_lon   = zero
        end where

        where (grid2_frac < .001) grid2_array = zero

      case(2)  !*** pseudo-spherical harmonic l=2,m=2

        where (grid1_mask)
          grid1_array = two + cos(grid1_center_lat)**2*
     &                    cos(two*grid1_center_lon)
          grad1_lat   = -sin(two*grid1_center_lat)*
     &                   cos(two*grid1_center_lon)
          grad1_lon   = -two*cos(grid1_center_lat)*
     &                   sin(two*grid1_center_lon)
        elsewhere
          grid1_array = zero
          grad1_lat   = zero
          grad1_lon   = zero
        end where

        where (grid2_frac > .001) 
          grid2_array = two + cos(grid2_center_lat)**2*
     &                        cos(two*grid2_center_lon)
        elsewhere
          grid2_array = zero
        end where

      case(3)  !*** pseudo-spherical harmonic l=32, m=16

        where (grid1_mask)
          grid1_array = two + sin(two*grid1_center_lat)**16*
     &                        cos(16.*grid1_center_lon)
          grad1_lat   = 32.*sin(two*grid1_center_lat)**15*
     &                      cos(two*grid1_center_lat)*
     &                      cos(16.*grid1_center_lon)
          grad1_lon   = -32.*sin(two*grid1_center_lat)**15*
     &                       sin(grid1_center_lat)*
     &                   sin(16.*grid1_center_lon)
        elsewhere
          grid1_array = zero
          grad1_lat   = zero
          grad1_lon   = zero
        end where

        where (grid2_frac > .001) 
          grid2_array = two + sin(two*grid2_center_lat)**16*
     &                        cos(16.*grid2_center_lon)
        elsewhere
          grid2_array = zero
        end where

      case default

        stop 'Bad choice for field to interpolate'

      end select

!-----------------------------------------------------------------------
!
!     if bicubic, we need 3 gradients in logical space
!
!-----------------------------------------------------------------------

      if (map_type == map_type_bicubic) then
        allocate (grad1_latlon (grid1_size)) 

        nx = grid1_dims(1)
        ny = grid1_dims(2)

        do n=1,grid1_size

          grad1_lat(n) = zero
          grad1_lon(n) = zero
          grad1_latlon(n) = zero

          if (grid1_mask(n)) then

            delew = half
            delns = half

            j = (n-1)/nx + 1
            i = n - (j-1)*nx

            ip1 = i+1
            im1 = i-1
            jp1 = j+1
            jm1 = j-1

            if (ip1 > nx) ip1 = ip1 - nx
            if (im1 < 1 ) im1 = nx
            if (jp1 > ny) then
              jp1 = j
              delns = one
            endif
            if (jm1 < 1 ) then
              jm1 = j
              delns = one
            endif

            in  = (jp1-1)*nx + i
            is  = (jm1-1)*nx + i
            ie  = (j  -1)*nx + ip1
            iw  = (j  -1)*nx + im1

            ine = (jp1-1)*nx + ip1
            inw = (jp1-1)*nx + im1
            ise = (jm1-1)*nx + ip1
            isw = (jm1-1)*nx + im1

            !*** compute i-gradient

            if (.not. grid1_mask(ie)) then
              ie = n
              delew = one
            endif
            if (.not. grid1_mask(iw)) then
              iw = n
              delew = one
            endif
 
            grad1_lat(n) = delew*(grid1_array(ie) - grid1_array(iw))

            !*** compute j-gradient

            if (.not. grid1_mask(in)) then
              in = n
              delns = one
            endif
            if (.not. grid1_mask(is)) then
              is = n
              delns = one
            endif
 
            grad1_lon(n) = delns*(grid1_array(in) - grid1_array(is))

            !*** compute ij-gradient

            delew = half
            if (jp1 == j .or. jm1 == j) then
              delns = one
            else 
              delns = half
            endif

            if (.not. grid1_mask(ine)) then
              if (in /= n) then
                ine = in
                delew = one
              else if (ie /= n) then
                ine = ie
                inw = iw
                if (inw == n) delew = one
                delns = one
              else
                ine = n
                inw = iw
                delew = one
                delns = one
              endif
            endif

            if (.not. grid1_mask(inw)) then
              if (in /= n) then
                inw = in
                delew = one
              else if (iw /= n) then
                inw = iw
                ine = ie
                if (ie == n) delew = one
                delns = one
              else
                inw = n
                ine = ie
                delew = one
                delns = one
              endif
            endif

            grad1_lat_zero(n) = delew*(grid1_array(ine) -
     &                                 grid1_array(inw))

            if (.not. grid1_mask(ise)) then
              if (is /= n) then
                ise = is
                delew = one
              else if (ie /= n) then
                ise = ie
                isw = iw
                if (isw == n) delew = one
                delns = one
              else
                ise = n
                isw = iw
                delew = one
                delns = one
              endif
            endif

            if (.not. grid1_mask(isw)) then
              if (is /= n) then
                isw = is
                delew = one
              else if (iw /= n) then
                isw = iw
                ise = ie
                if (ie == n) delew = one
                delns = one
              else
                isw = n
                ise = ie
                delew = one
                delns = one
              endif
            endif

            grad1_lon_zero(n) = delew*(grid1_array(ise) -
     &                                 grid1_array(isw))

            grad1_latlon(n) = delns*(grad1_lat_zero(n) -
     &                               grad1_lon_zero(n))

          endif
        enddo
      endif

!-----------------------------------------------------------------------
!
!     test a first-order map from grid1 to grid2
!
!-----------------------------------------------------------------------

      grad1_lat_zero = zero
      grad1_lon_zero = zero

      if (map_type /= map_type_bicubic) then
        call remap(grid2_tmp, wts_map1, grid2_add_map1, grid1_add_map1,
     &             grid1_array)
      else
        call remap(grid2_tmp, wts_map1, grid2_add_map1, grid1_add_map1,
     &             grid1_array, src_grad1=grad1_lat,
     &                          src_grad2=grad1_lon,
     &                          src_grad3=grad1_latlon)
      endif

      if (map_type == map_type_conserv .or. 
     &    map_type == map_type_particle) then
        select case (norm_opt)
        case (norm_opt_none)
          grid2_err = grid2_frac*grid2_area
          where (grid2_err /= zero)
            grid2_tmp = grid2_tmp/grid2_err
          else where
            grid2_tmp = zero
          end where
        case (norm_opt_frcarea)
        case (norm_opt_dstarea)
          where (grid2_frac /= zero)
            grid2_tmp = grid2_tmp/grid2_frac
          else where
            grid2_tmp = zero
          end where
        end select
      end if

      where (grid2_frac > .999)
        grid2_err = (grid2_tmp - grid2_array)/grid2_array
      elsewhere 
        grid2_err = zero
      end where

      print *,'First order mapping from grid1 to grid2:'
      print *,'----------------------------------------'
      print *,'Grid1 min,max: ',minval(grid1_array),maxval(grid1_array)
      print *,'Grid2 min,max: ',minval(grid2_tmp  ),maxval(grid2_tmp  )
      print *,' Err2 min,max: ',minval(grid2_err),maxval(grid2_err)
      print *,' Err2    mean: ',sum(abs(grid2_err))/
     &                          count(grid2_frac > .999)

      !***
      !*** Conservation Test
      !***

      print *,'Conservation:'
      print *,'Grid1 Integral = ',sum(grid1_array*grid1_area*grid1_frac)
      print *,'Grid2 Integral = ',sum(grid2_tmp  *grid2_area*grid2_frac)

!-----------------------------------------------------------------------
!
!     write results to NetCDF file
!
!-----------------------------------------------------------------------

      n = 0
      do j=1,grid1_dims(2)
      do i=1,grid1_dims(1)
         n = n+1
         grid1tmp2d(i,j) = grid1_array(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_srcarray_id,
     &                      grid1tmp2d)
      call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_tmp(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_dstarray1_id,
     &                      grid2tmp2d  )
      call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_err(n)
      end do
      end do
      ncstat = nf90_put_var(nc_outfile_id, nc_dsterror1_id,
     &                      grid2tmp2d)
      call netcdf_error_handler(ncstat)

!-----------------------------------------------------------------------
!
!     for conservative mappings:
!     test a second-order map from grid1 to grid2 with only lat grads
!
!-----------------------------------------------------------------------

      if (map_type == map_type_conserv) then

        call remap(grid2_tmp, wts_map1, grid2_add_map1, grid1_add_map1,
     &             grid1_array, src_grad1=grad1_lat,
     &                          src_grad2=grad1_lon_zero) 

        select case (norm_opt)
        case (norm_opt_none)
          grid2_err = grid2_frac*grid2_area
          where (grid2_err /= zero)
            grid2_tmp = grid2_tmp/grid2_err
          else where
            grid2_tmp = zero
          end where
        case (norm_opt_frcarea)
        case (norm_opt_dstarea)
          where (grid2_frac /= zero)
            grid2_tmp = grid2_tmp/grid2_frac
          else where
            grid2_tmp = zero
          end where
        end select

        where (grid2_frac > .999)
          grid2_err = (grid2_tmp - grid2_array)/grid2_array
        elsewhere 
          grid2_err = zero
        end where

        print *,'Second order mapping from grid1 to grid2 (lat only):'
        print *,'----------------------------------------'
        print *,'Grid1 min,max: ',minval(grid1_array),
     &                            maxval(grid1_array)
        print *,'Grid2 min,max: ',minval(grid2_tmp  ),
     &                            maxval(grid2_tmp  )
        print *,' Err2 min,max: ',minval(grid2_err),maxval(grid2_err)
        print *,' Err2    mean: ',sum(abs(grid2_err))/
     &                            count(grid2_frac > .999)

        !***
        !*** Conservation Test
        !***

        print *,'Conservation:'
        print *,'Grid1 Integral = ',
     &          sum(grid1_array*grid1_area*grid1_frac)
        print *,'Grid2 Integral = ',
     &          sum(grid2_tmp  *grid2_area*grid2_frac)

!-----------------------------------------------------------------------
!
!       write results to NetCDF file
!
!-----------------------------------------------------------------------

      n = 0
      do j=1,grid1_dims(2)
      do i=1,grid1_dims(1)
         n = n+1
         grid1tmp2d(i,j) = grad1_lat(n)
      end do
      end do
        ncstat = nf90_put_var(nc_outfile_id, nc_srcgradlat_id,
     &                        grid1tmp2d)
        call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_tmp(n)
      end do
      end do
        ncstat = nf90_put_var(nc_outfile_id, nc_dstarray1a_id,
     &                        grid2tmp2d  )
        call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_err(n)
      end do
      end do
        ncstat = nf90_put_var(nc_outfile_id, nc_dsterror1a_id,
     &                        grid2tmp2d)
        call netcdf_error_handler(ncstat)

!-----------------------------------------------------------------------
!
!     for conservative mappings:
!     test a second-order map from grid1 to grid2
!
!-----------------------------------------------------------------------

        call remap(grid2_tmp,wts_map1,grid2_add_map1,grid1_add_map1,
     &             grid1_array, src_grad1=grad1_lat,
     &                          src_grad2=grad1_lon) 

        select case (norm_opt)
        case (norm_opt_none)
          grid2_err = grid2_frac*grid2_area
          where (grid2_err /= zero)
            grid2_tmp = grid2_tmp/grid2_err
          else where
            grid2_tmp = zero
          end where
        case (norm_opt_frcarea)
        case (norm_opt_dstarea)
          where (grid2_frac /= zero)
            grid2_tmp = grid2_tmp/grid2_frac
          else where
            grid2_tmp = zero
          end where
        end select

        where (grid2_frac > .999)
          grid2_err = (grid2_tmp - grid2_array)/grid2_array
        elsewhere 
          grid2_err = zero
        end where

        print *,'Second order mapping from grid1 to grid2:'
        print *,'-----------------------------------------'
        print *,'Grid1 min,max: ',minval(grid1_array),
     &                            maxval(grid1_array)
        print *,'Grid2 min,max: ',minval(grid2_tmp  ),
     &                            maxval(grid2_tmp  )
        print *,' Err2 min,max: ',minval(grid2_err),maxval(grid2_err)
        print *,' Err2    mean: ',sum(abs(grid2_err))/
     &                            count(grid2_frac > .999)

        !***
        !*** Conservation Test
        !***

        print *,'Conservation:'
        print *,'Grid1 Integral = ',
     &           sum(grid1_array*grid1_area*grid1_frac)
        print *,'Grid2 Integral = ',
     &           sum(grid2_tmp  *grid2_area*grid2_frac)

!-----------------------------------------------------------------------
!
!       write results to NetCDF file
!
!-----------------------------------------------------------------------

      n = 0
      do j=1,grid1_dims(2)
      do i=1,grid1_dims(1)
         n = n+1
         grid1tmp2d(i,j) = grad1_lon(n)
      end do
      end do
        ncstat = nf90_put_var(nc_outfile_id, nc_srcgradlon_id,
     &                        grid1tmp2d)
        call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_tmp(n)
      end do
      end do
        ncstat = nf90_put_var(nc_outfile_id, nc_dstarray2_id,
     &                        grid2tmp2d  )
        call netcdf_error_handler(ncstat)

      n = 0
      do j=1,grid2_dims(2)
      do i=1,grid2_dims(1)
         n = n+1
         grid2tmp2d(i,j) = grid2_err(n)
      end do
      end do
        ncstat = nf90_put_var(nc_outfile_id, nc_dsterror2_id,
     &                        grid2tmp2d)
        call netcdf_error_handler(ncstat)

      endif

!-----------------------------------------------------------------------
!
!     close netCDF file
!
!-----------------------------------------------------------------------

      ncstat = nf90_close(nc_outfile_id)
      call netcdf_error_handler(ncstat)

!-----------------------------------------------------------------------
!
!     calculate some statistics
!
!-----------------------------------------------------------------------

      grid2_count = zero
      grid2_tmp   = zero
      grid2_err   = zero

      print *,'number of sparse matrix entries ',num_links_map1
      do n=1,num_links_map1
        grid2_count(grid2_add_map1(n)) = 
     &  grid2_count(grid2_add_map1(n)) + 1
        if (wts_map1(1,n) > one .or. wts_map1(1,n) < zero) then
          grid2_tmp(grid2_add_map1(n)) = 
     &    grid2_tmp(grid2_add_map1(n)) + 1
          grid2_err(grid2_add_map1(n)) = max(abs(wts_map1(1,n)),
     &    grid2_err(grid2_add_map1(n)) )
        endif
      end do

      do n=1,grid2_size
        if (grid2_tmp(n) > zero) print *,n,nint(grid2_tmp(n)),
     &                                     grid2_err(n)
      end do

      imin = minval(grid2_count, mask=(grid2_count > 0))
      imax = maxval(grid2_count)
      idiff =  (imax - imin)/10 + 1
      print *,'total number of dest cells ',grid2_size
      print *,'number of cells participating in remap ',
     &   count(grid2_count > zero)
      print *,'min no of entries/row = ',imin
      print *,'max no of entries/row = ',imax

      imax = imin + idiff
      do n=1,10
        print *,'num of rows with entries between ',imin,' - ',imax-1,
     &     count(grid2_count >= imin .and. grid2_count < imax)
        imin = imin + idiff
        imax = imax + idiff
      end do

!-----------------------------------------------------------------------

      end program remap_test

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
