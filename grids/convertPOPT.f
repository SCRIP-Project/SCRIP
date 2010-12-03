!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This file converts a POP grid.dat file to a remapping grid file
!     in netCDF format.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: convertPOPT.f,v 1.4 2001/08/21 21:22:56 pwjones Exp $
!
!     Copyright (c) 1997, 1998 the Regents of the University of 
!       California.
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
!***********************************************************************

      program convertPOPT

!-----------------------------------------------------------------------
!
!     This file converts a POP grid.dat file to a remapping grid file.
!
!-----------------------------------------------------------------------

      use SCRIP_KindsMod
      use constants
      use SCRIP_IOUnitsMod
      use SCRIP_NetcdfMod
      use netcdf

      implicit none

!-----------------------------------------------------------------------
!
!     variables that describe the grid
!       4/3       nx = 192, ny = 128
!       2/3 (mod) nx = 384, ny = 288
!       x3p Greenland DP nx = 100, ny = 116
!       x2p Greenland DP nx = 160, ny = 192
!       x1p Greenland DP nx = 320, ny = 384
!
!-----------------------------------------------------------------------

      integer (kind=SCRIP_i4), parameter ::
!     &             nx = 1560, ny = 1080,
     &             nx = 1280, ny = 720,
     &             grid_size = nx*ny,
     &             grid_rank = 2,
     &             grid_corners = 4

      logical, parameter :: 
     &             global = .false.

      integer (kind=SCRIP_i4), dimension(2) ::
     &             grid_dims   ! size of each dimension

      character(SCRIP_CharLength), parameter :: 
!     &    grid_name = 'RACM ar9v4 grid',
!     &    grid_file_in  = '/fs/cgd/csm/inputdata/ocn/pop/ar9v4/grid/'//
!     &                    'grid.ar9v4.ocn.20100623.ieeer8',
!     &    grid_topo_in  = '/fs/cgd/csm/inputdata/ocn/pop/ar9v4/grid/'//
!     &                    'kmt.ar9v4.ocn.20100920.ieeei4',
!     &    grid_file_out = './ar9v4_100920.nc'
     &    grid_name = 'RACM ar9v3 grid',
     &    grid_file_in  = '/fs/cgd/csm/inputdata/ocn/pop/ar9v3/grid/'//
     &                    'grid.ar9v3.ocn.20100622.ieeer8',
     &    grid_topo_in  = '/fs/cgd/csm/inputdata/ocn/pop/ar9v3/grid/'//
     &                    'kmt.ar9v3.ocn.20100622.ieeei4',
     &    grid_file_out = './ar9v3_101129.nc'

      real (kind=SCRIP_r8), parameter ::
     &  radius    = 6370.0e5_SCRIP_r8       ! radius of Earth (cm)
     &, area_norm = one/(radius*radius)

!-----------------------------------------------------------------------
!
!     grid coordinates and masks
!
!-----------------------------------------------------------------------

      integer (kind=SCRIP_i4), dimension(grid_size) ::
     &             grid_imask

      real (kind=SCRIP_r8), dimension(grid_size) ::
     &             grid_area      ,  ! area as computed in POP
     &             grid_center_lat,  ! lat/lon coordinates for
     &             grid_center_lon   ! each grid center in radians

      real (kind=SCRIP_r8), dimension(grid_corners,grid_size) ::
     &             grid_corner_lat,  ! lat/lon coordinates for
     &             grid_corner_lon   ! each grid corner in radians

      real (kind=SCRIP_r8), dimension(nx,ny) ::
     &             HTN, HTE          ! T-cell grid lengths

!-----------------------------------------------------------------------
!
!     other local variables
!
!-----------------------------------------------------------------------

      integer (kind=SCRIP_i4) :: i, j, n, iunit, ocn_add, im1, jm1
      integer (kind=SCRIP_i4) :: ip1, jp1

      integer (kind=SCRIP_i4) ::
     &        ncstat,            ! general netCDF status variable
     &        nc_grid_id,        ! netCDF grid dataset id
     &        nc_gridsize_id,    ! netCDF grid size dim id
     &        nc_gridcorn_id,    ! netCDF grid corner dim id
     &        nc_gridrank_id,    ! netCDF grid rank dim id
     &        nc_griddims_id,    ! netCDF grid dimensions id
     &        nc_grdcntrlat_id,  ! netCDF grid center lat id
     &        nc_grdcntrlon_id,  ! netCDF grid center lon id
     &        nc_grdimask_id,    ! netCDF grid mask id
     &        nc_gridarea_id,    ! netCDF grid area id
     &        nc_grdcrnrlat_id,  ! netCDF grid corner lat id
     &        nc_grdcrnrlon_id   ! netCDF grid corner lon id

      integer (kind=SCRIP_i4), dimension(2) ::
     &        nc_dims2_id        ! netCDF dim id array for 2-d arrays

      real (kind=SCRIP_r8) :: tmplon, dxt, dyt
      real (kind=SCRIP_r8) :: lat1,lat2,lat3,lat4,lon1,lon2,lon3,lon4
      real (kind=SCRIP_r8) :: tx,ty,tz,coslat
      real (kind=SCRIP_r8) :: x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      integer (kind=SCRIP_i4) :: errorCode
      character (11), parameter :: rtnName = 'convertPOPT'

!-----------------------------------------------------------------------
!
!     read in grid info
!     lat/lon info is on velocity points which correspond
!     to the NE corner (in logical space) of the grid cell.
!
!-----------------------------------------------------------------------

      iunit=11
      open(unit=iunit, file=grid_topo_in, status='old', 
     &     form='unformatted', access='direct', recl=grid_size*4)
      read (unit=iunit,rec=1) grid_imask

      iunit=12
      open(unit=iunit, file=grid_file_in, status='old', 
     &     form='unformatted', access='direct', recl=grid_size*8)
      read (unit=iunit, rec=1) grid_corner_lat(3,:)
      read (unit=iunit, rec=2) grid_corner_lon(3,:)
      read (unit=iunit, rec=3) HTN
      read (unit=iunit, rec=4) HTE

      grid_dims(1) = nx
      grid_dims(2) = ny

      write(6,*) 'nx,ny=',nx,ny
      write(6,*) 'grid_corner_lat3 ',minval(grid_corner_lat(3,:)),
     &  maxval(grid_corner_lat(3,:))
      write(6,*) 'grid_corner_lon3 ',minval(grid_corner_lon(3,:)),
     &  maxval(grid_corner_lon(3,:))
      write(6,*) 'HTN ',minval(HTN),maxval(HTN)
      write(6,*) 'HTE ',minval(HTE),maxval(HTE)

!-----------------------------------------------------------------------
!
!     convert KMT field to integer grid mask
!
!-----------------------------------------------------------------------

      grid_imask = min(grid_imask, 1)

!-----------------------------------------------------------------------
!
!     compute remaining corners
!
!-----------------------------------------------------------------------

      if (global) then

        write(6,*) 'generating corners, global grid'

        do j=1,ny
          do i=1,nx
            ocn_add = (j-1)*nx + i
            if (i .ne. 1) then
              im1 = ocn_add - 1
            else
              im1 = ocn_add + nx - 1
            endif

            grid_corner_lat(4,ocn_add) = grid_corner_lat(3,im1)
            grid_corner_lon(4,ocn_add) = grid_corner_lon(3,im1)
          end do
        end do

        do j=2,ny
          do i=1,nx
            ocn_add = (j-1)*nx + i
            jm1 = (j-2)*nx + i

            grid_corner_lat(2,ocn_add) = grid_corner_lat(3,jm1)
            grid_corner_lat(1,ocn_add) = grid_corner_lat(4,jm1)

            grid_corner_lon(2,ocn_add) = grid_corner_lon(3,jm1)
            grid_corner_lon(1,ocn_add) = grid_corner_lon(4,jm1)
          end do
        end do

        !     mock up the lower row boundaries

        do i=1,nx
          grid_corner_lat(1,i) = -pih + tiny
          grid_corner_lat(2,i) = -pih + tiny

          grid_corner_lon(1,i) = grid_corner_lon(4,i)
          grid_corner_lon(2,i) = grid_corner_lon(3,i)
        end do

      else  ! not global
            ! extrapolates via lon/lat, could change to great circle

        write(6,*) 'generating corners, non global grid'

        !  copy 3 to neighbor 4

        do j=1,ny
          do i=2,nx
            ocn_add = (j-1)*nx + i
            if (i .ne. 1) then
              im1 = ocn_add - 1
            else
              im1 = ocn_add + nx - 1
            endif

            grid_corner_lat(4,ocn_add) = grid_corner_lat(3,im1)
            grid_corner_lon(4,ocn_add) = grid_corner_lon(3,im1)
          end do
        end do

        !  mock up the left column boundaries via extrapolation

        if (nx <= 1) stop  ! nx > 1 for this to work
        do j = 1,ny
          ocn_add = (j-1)*nx + 1
          ip1 =     (j-1)*nx + 2
          grid_corner_lat(4,ocn_add) = 2.0 * grid_corner_lat(3,ocn_add)
     &                                     - grid_corner_lat(3,ip1)
          grid_corner_lon(4,ocn_add) = 2.0 * grid_corner_lon(3,ocn_add)
     &                                     - grid_corner_lon(3,ip1)
        enddo

        !  copy 3 to neighbor 2

        do j=2,ny
          do i=1,nx
            ocn_add = (j-1)*nx + i
            jm1 =     (j-2)*nx + i

            grid_corner_lat(2,ocn_add) = grid_corner_lat(3,jm1)
            grid_corner_lat(1,ocn_add) = grid_corner_lat(4,jm1)

            grid_corner_lon(2,ocn_add) = grid_corner_lon(3,jm1)
            grid_corner_lon(1,ocn_add) = grid_corner_lon(4,jm1)
          end do
        end do

        !  mock up the lower row boundaries via extrapolation

        if (ny <= 1) stop  ! ny > 1 for this to work
        do i = 1,nx
          ocn_add = i
          jp1 =     nx + i
          grid_corner_lat(2,ocn_add) = 2.0 * grid_corner_lat(3,ocn_add)
     &                                     - grid_corner_lat(3,jp1)
          grid_corner_lon(2,ocn_add) = 2.0 * grid_corner_lon(3,ocn_add)
     &                                     - grid_corner_lon(3,jp1)
          if (i < nx) then
            ip1 =     i + 1
            grid_corner_lat(1,ip1) = grid_corner_lat(2,ocn_add)
            grid_corner_lon(1,ip1) = grid_corner_lon(2,ocn_add)
          endif
        enddo

        !  mock up the lower left hand corner via extrapolation

        ocn_add = 1
        grid_corner_lat(1,ocn_add) = grid_corner_lat(2,ocn_add) + 
     &                               grid_corner_lat(4,ocn_add) -
     &                               grid_corner_lat(3,ocn_add)
        grid_corner_lon(1,ocn_add) = grid_corner_lon(2,ocn_add) + 
     &                               grid_corner_lon(4,ocn_add) -
     &                               grid_corner_lon(3,ocn_add)

      endif   ! global 

!-----------------------------------------------------------------------
!
!     correct for 0,2pi longitude crossings
!
!-----------------------------------------------------------------------

      do ocn_add=1,grid_size
        if (grid_corner_lon(1,ocn_add) > pi2) 
     &      grid_corner_lon(1,ocn_add) = 
     &      grid_corner_lon(1,ocn_add) - pi2
        if (grid_corner_lon(1,ocn_add) < 0.0) 
     &      grid_corner_lon(1,ocn_add) = 
     &      grid_corner_lon(1,ocn_add) + pi2
        do n=2,grid_corners
          tmplon = grid_corner_lon(n  ,ocn_add) - 
     &             grid_corner_lon(n-1,ocn_add) 
          if (tmplon < -three*pih) grid_corner_lon(n,ocn_add) = 
     &                             grid_corner_lon(n,ocn_add) + pi2
          if (tmplon >  three*pih) grid_corner_lon(n,ocn_add) = 
     &                             grid_corner_lon(n,ocn_add) - pi2
        end do
      end do

!-----------------------------------------------------------------------
!
!     compute ocean cell centers by averaging corner values
!
!-----------------------------------------------------------------------

      if (.false.) then  ! old 4 cell average

        do ocn_add=1,grid_size
          grid_center_lat(ocn_add) = grid_corner_lat(1,ocn_add)
          grid_center_lon(ocn_add) = grid_corner_lon(1,ocn_add)
          do n=2,grid_corners
            grid_center_lat(ocn_add) = grid_center_lat(ocn_add) + 
     &                                 grid_corner_lat(n,ocn_add)
            grid_center_lon(ocn_add) = grid_center_lon(ocn_add) + 
     &                                 grid_corner_lon(n,ocn_add)
          end do
          grid_center_lat(ocn_add) = grid_center_lat(ocn_add)/
     &                                    float(grid_corners)
          grid_center_lon(ocn_add) = grid_center_lon(ocn_add)/
     &                                    float(grid_corners)
          if (grid_center_lon(ocn_add) > pi2) 
     &        grid_center_lon(ocn_add) = grid_center_lon(ocn_add) - pi2
          if (grid_center_lon(ocn_add) < 0.0) 
     &        grid_center_lon(ocn_add) = grid_center_lon(ocn_add) + pi2
        end do

      else ! avg corner values in 3D space without pole problem

        do ocn_add=1,grid_size
          lat1 = grid_corner_lat(1,ocn_add)
          lat2 = grid_corner_lat(2,ocn_add)
          lat3 = grid_corner_lat(3,ocn_add)
          lat4 = grid_corner_lat(4,ocn_add)

          lon1 = grid_corner_lon(1,ocn_add)
          lon2 = grid_corner_lon(2,ocn_add)
          lon3 = grid_corner_lon(3,ocn_add)
          lon4 = grid_corner_lon(4,ocn_add)

          coslat = cos(lat1)
          x1 = cos(lon1)*coslat
          y1 = sin(lon1)*coslat
          z1 = sin(lat1)

          coslat = cos(lat2)
          x2 = cos(lon2)*coslat
          y2 = sin(lon2)*coslat
          z2 = sin(lat2)

          coslat = cos(lat3)
          x3 = cos(lon3)*coslat
          y3 = sin(lon3)*coslat
          z3 = sin(lat3)

          coslat = cos(lat4)
          x4 = cos(lon4)*coslat
          y4 = sin(lon4)*coslat
          z4 = sin(lat4)

          tx = (x1+x2+x3+x4)/four
          ty = (y1+y2+y3+y4)/four
          tz = (z1+z2+z3+z4)/four

          tz = tz/sqrt(tx**2+ty**2+tz**2) ! normalize (unnecessary?)

          grid_center_lon(ocn_add) = zero
          if (tx /= zero .or. ty /= zero) 
     &      grid_center_lon(ocn_add) = atan2(ty,tx)
          if (grid_center_lon(ocn_add) < zero) 
     &      grid_center_lon(ocn_add) = grid_center_lon(ocn_add) + pi2
          grid_center_lat(ocn_add) = asin(tz)
        end do

      end if  ! method

!-----------------------------------------------------------------------
!
!     compute cell areas in same way as POP
!
!-----------------------------------------------------------------------

      n = 0
      do j=1,ny
        if (j > 1) then
          jm1 = j-1
        else
          jm1 = 1
        endif
        do i=1,nx
          if (i > 1) then
            im1 = i-1
          else
            im1 = nx
          endif

          n = n+1

          dxt = half*(HTN(i,j) + HTN(i,jm1))
          dyt = half*(HTE(i,j) + HTE(im1,j))
          if (dxt == zero) dxt=one
          if (dyt == zero) dyt=one

          grid_area(n) = dxt*dyt*area_norm
        end do
      end do

      write(6,*) 'grid_corner_lat1 ',minval(grid_corner_lat(1,:)),
     &  maxval(grid_corner_lat(1,:))
      write(6,*) 'grid_corner_lon1 ',minval(grid_corner_lon(1,:)),
     &  maxval(grid_corner_lon(1,:))
      write(6,*) 'grid_corner_lat2 ',minval(grid_corner_lat(2,:)),
     &  maxval(grid_corner_lat(2,:))
      write(6,*) 'grid_corner_lon2 ',minval(grid_corner_lon(2,:)),
     &  maxval(grid_corner_lon(2,:))
      write(6,*) 'grid_corner_lat3 ',minval(grid_corner_lat(3,:)),
     &  maxval(grid_corner_lat(3,:))
      write(6,*) 'grid_corner_lon3 ',minval(grid_corner_lon(3,:)),
     &  maxval(grid_corner_lon(3,:))
      write(6,*) 'grid_corner_lat4 ',minval(grid_corner_lat(4,:)),
     &  maxval(grid_corner_lat(4,:))
      write(6,*) 'grid_corner_lon4 ',minval(grid_corner_lon(4,:)),
     &  maxval(grid_corner_lon(4,:))
      write(6,*) 'grid_center_lat ',minval(grid_center_lat(:)),
     &  maxval(grid_center_lat(:))
      write(6,*) 'grid_center_lon ',minval(grid_center_lon(:)),
     &  maxval(grid_center_lon(:))

!-----------------------------------------------------------------------
!
!     set up attributes for netCDF file
!
!-----------------------------------------------------------------------

      !***
      !*** create netCDF dataset for this grid
      !***

      ncstat = nf90_create (grid_file_out, NF90_CLOBBER,
     &                    nc_grid_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_att (nc_grid_id, NF90_GLOBAL, 'title',
     &                       grid_name)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid size dimension
      !***

      ncstat = nf90_def_dim (nc_grid_id, 'grid_size', grid_size, 
     &                     nc_gridsize_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid rank dimension
      !***

      ncstat = nf90_def_dim (nc_grid_id, 'grid_rank', grid_rank, 
     &                     nc_gridrank_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid corner dimension
      !***

      ncstat = nf90_def_dim (nc_grid_id, 'grid_corners', grid_corners, 
     &                     nc_gridcorn_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid dim size array
      !***

      ncstat = nf90_def_var (nc_grid_id, 'grid_dims', NF90_INT,
     &                       nc_gridrank_id, nc_griddims_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid center latitude array
      !***

      ncstat = nf90_def_var (nc_grid_id, 'grid_center_lat', NF90_DOUBLE,
     &                       nc_gridsize_id, nc_grdcntrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_att (nc_grid_id, nc_grdcntrlat_id, 'units',
     &                       'radians')
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid center longitude array
      !***

      ncstat = nf90_def_var (nc_grid_id, 'grid_center_lon', NF90_DOUBLE,
     &                       nc_gridsize_id, nc_grdcntrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_att (nc_grid_id, nc_grdcntrlon_id, 'units',
     &                       'radians')
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid area array
      !***

      ncstat = nf90_def_var (nc_grid_id, 'grid_area', NF90_DOUBLE,
     &                       nc_gridsize_id, nc_gridarea_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid mask
      !***

      ncstat = nf90_def_var (nc_grid_id, 'grid_imask', NF90_INT,
     &                       nc_gridsize_id, nc_grdimask_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_att (nc_grid_id, nc_grdimask_id, 'units',
     &                       'unitless')
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid corner latitude array
      !***

      nc_dims2_id(1) = nc_gridcorn_id
      nc_dims2_id(2) = nc_gridsize_id

      ncstat = nf90_def_var (nc_grid_id, 'grid_corner_lat', NF90_DOUBLE,
     &                       nc_dims2_id, nc_grdcrnrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_att (nc_grid_id, nc_grdcrnrlat_id, 'units',
     &                            'radians')
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** define grid corner longitude array
      !***

      ncstat = nf90_def_var (nc_grid_id, 'grid_corner_lon', NF90_DOUBLE,
     &                       nc_dims2_id, nc_grdcrnrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_att (nc_grid_id, nc_grdcrnrlon_id, 'units',
     &                            'radians')
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      !***
      !*** end definition stage
      !***

      ncstat = nf90_enddef(nc_grid_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

!-----------------------------------------------------------------------
!
!     write grid data
!
!-----------------------------------------------------------------------

      ncstat = nf90_put_var(nc_grid_id, nc_griddims_id, grid_dims)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_var(nc_grid_id, nc_grdcntrlat_id, 
     &                           grid_center_lat)
      ncstat = nf90_put_var(nc_grid_id, nc_grdimask_id, grid_imask)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_var(nc_grid_id, nc_gridarea_id, 
     &                           grid_area)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_var(nc_grid_id, nc_grdcntrlat_id, 
     &                           grid_center_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_var(nc_grid_id, nc_grdcntrlon_id, 
     &                           grid_center_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_var(nc_grid_id, nc_grdcrnrlat_id, 
     &                           grid_corner_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_put_var(nc_grid_id, nc_grdcrnrlon_id, 
     &                           grid_corner_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &     'error ')) call convertPOPTexit(errorCode)

      ncstat = nf90_close(nc_grid_id)

!***********************************************************************

      end program convertPOPT



!***********************************************************************
! !IROUTINE: convertPOPTexit
! !INTERFACE:

      subroutine convertPOPTexit(errorCode)

! !DESCRIPTION:
!  This program exits the convertgauss program. It first calls the 
!  SCRIP error print function to print any errors encountered and then
!  stops the execution.
!
! !REVISION HISTORY:
!  SVN:$Id: $

! !USES:

      use SCRIP_KindsMod
      use SCRIP_ErrorMod

! !INPUT PARAMETERS:

      integer (SCRIP_i4), intent(in) ::
     &     errorCode            ! error flag to detect any errors encountered

!-----------------------------------------------------------------------
!
!  call SCRIP error print function to output any logged errors that
!  were encountered during execution.  Then stop.
!
!-----------------------------------------------------------------------

      call SCRIP_ErrorPrint(errorCode)
      
      stop
      
!-----------------------------------------------------------------------
      
      end subroutine convertPOPTexit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
