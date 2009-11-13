!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This module reads in and initializes two grids for remapping.
!     NOTE: grid1 must be the master grid -- the grid that determines
!           which cells participate (e.g. land mask) and the fractional
!           area of grid2 cells that participate in the remapping.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: grids.f,v 1.6 2001/08/21 21:06:41 pwjones Exp $
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

      module grids

!-----------------------------------------------------------------------

      use SCRIP_KindsMod    ! defines data types
      use SCRIP_ErrorMod    ! error tracking
      use SCRIP_IOUnitsMod  ! manages I/O units
      use constants    ! common constants
      use SCRIP_NetcdfMod   ! netCDF stuff
      use netcdf            ! netCDF library module

      implicit none

!-----------------------------------------------------------------------
!
!     variables that describe each grid
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4), save ::
     &             grid1_size, grid2_size, ! total points on each grid
     &             grid1_rank, grid2_rank, ! rank of each grid
     &             grid1_corners, grid2_corners ! number of corners
                                                ! for each grid cell

      integer (SCRIP_i4), dimension(:), allocatable, save ::
     &             grid1_dims, grid2_dims  ! size of each grid dimension

      character(SCRIP_charLength), save :: 
     &             grid1_name, grid2_name  ! name for each grid

      character (SCRIP_charLength), save :: 
     &             grid1_units, ! units for grid coords (degs/radians)
     &             grid2_units  ! units for grid coords

      real (SCRIP_r8), parameter ::
     &      deg2rad = pi/180.   ! conversion for deg to rads

!-----------------------------------------------------------------------
!
!     grid coordinates and masks
!
!-----------------------------------------------------------------------

      logical (SCRIP_logical), dimension(:), allocatable, target,save ::
     &             grid1_mask,        ! flag which cells participate
     &             grid2_mask         ! flag which cells participate

      real (SCRIP_r8), dimension(:), allocatable, target, save ::
     &             grid1_center_lat,  ! lat/lon coordinates for
     &             grid1_center_lon,  ! each grid center in radians
     &             grid2_center_lat, 
     &             grid2_center_lon,
     &             grid1_area,        ! tot area of each grid1 cell
     &             grid2_area,        ! tot area of each grid2 cell
     &             grid1_area_in,     ! area of grid1 cell from file
     &             grid2_area_in,     ! area of grid2 cell from file
     &             grid1_frac,        ! fractional area of grid cells
     &             grid2_frac         ! participating in remapping

      real (SCRIP_r8), dimension(:,:), allocatable, target, save ::
     &             grid1_corner_lat,  ! lat/lon coordinates for
     &             grid1_corner_lon,  ! each grid corner in radians
     &             grid2_corner_lat, 
     &             grid2_corner_lon

      logical (SCRIP_logical), save ::
     &             luse_grid_centers ! use centers for bounding boxes
     &,            luse_grid1_area   ! use area from grid file
     &,            luse_grid2_area   ! use area from grid file

      real (SCRIP_r8), dimension(:,:), allocatable, target, save ::
     &             grid1_bound_box,  ! lat/lon bounding box for use
     &             grid2_bound_box   ! in restricting grid searches

!-----------------------------------------------------------------------
!
!     bins for restricting searches
!
!-----------------------------------------------------------------------

      character (SCRIP_charLength), save ::
     &        restrict_type  ! type of bins to use

      integer (SCRIP_i4), save ::
     &        num_srch_bins  ! num of bins for restricted srch

      integer (SCRIP_i4), dimension(:,:), allocatable, save ::
     &        bin_addr1, ! min,max adds for grid1 cells in this lat bin
     &        bin_addr2  ! min,max adds for grid2 cells in this lat bin

      real(SCRIP_r8), dimension(:,:), allocatable, save ::
     &        bin_lats   ! min,max latitude for each search bin
     &,       bin_lons   ! min,max longitude for each search bin

!***********************************************************************

      contains

!***********************************************************************

      subroutine grid_init(grid1_file, grid2_file, errorCode)

!-----------------------------------------------------------------------
!
!     this routine reads grid info from grid files and makes any
!     necessary changes (e.g. for 0,2pi longitude range)
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      character(SCRIP_charLength), intent(in) :: 
     &             grid1_file, grid2_file  ! grid data files

!-----------------------------------------------------------------------
!
!     output variables
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4), intent(out) ::
     &   errorCode                 ! returned error code

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4) :: 
     &  n      ! loop counter
     &, nele   ! element loop counter
     &, i,j    ! logical 2d addresses
     &, ip1,jp1
     &, n_add, e_add, ne_add
     &, nx, ny

      integer (SCRIP_i4) :: 
     &         ncstat,           ! netCDF status variable
     &         nc_grid1_id,       ! netCDF grid file id
     &         nc_grid2_id,       ! netCDF grid file id
     &         nc_grid1size_id,   ! netCDF grid size dim id
     &         nc_grid2size_id,   ! netCDF grid size dim id
     &         nc_grid1corn_id,   ! netCDF grid corner dim id
     &         nc_grid2corn_id,   ! netCDF grid corner dim id
     &         nc_grid1rank_id,   ! netCDF grid rank dim id
     &         nc_grid2rank_id,   ! netCDF grid rank dim id
     &         nc_grid1area_id,   ! netCDF grid rank dim id
     &         nc_grid2area_id,   ! netCDF grid rank dim id
     &         nc_grid1dims_id,   ! netCDF grid dimension size id
     &         nc_grid2dims_id,   ! netCDF grid dimension size id
     &         nc_grd1imask_id,   ! netCDF grid imask var id
     &         nc_grd2imask_id,   ! netCDF grid imask var id
     &         nc_grd1crnrlat_id, ! netCDF grid corner lat var id
     &         nc_grd2crnrlat_id, ! netCDF grid corner lat var id
     &         nc_grd1crnrlon_id, ! netCDF grid corner lon var id
     &         nc_grd2crnrlon_id, ! netCDF grid corner lon var id
     &         nc_grd1cntrlat_id, ! netCDF grid center lat var id
     &         nc_grd2cntrlat_id, ! netCDF grid center lat var id
     &         nc_grd1cntrlon_id, ! netCDF grid center lon var id
     &         nc_grd2cntrlon_id  ! netCDF grid center lon var id

      integer (SCRIP_i4), dimension(:), allocatable :: 
     &                            imask ! integer mask read from file

      real (SCRIP_r8) :: 
     &  dlat,dlon           ! lat/lon intervals for search bins

      real (SCRIP_r8), dimension(4) ::
     &  tmp_lats, tmp_lons  ! temps for computing bounding boxes

      character (9), parameter ::
     &   rtnName = 'grid_init'

!-----------------------------------------------------------------------
!
!     open grid files and read grid size/name data
!
!-----------------------------------------------------------------------

      errorCode = SCRIP_Success

      ncstat = nf90_open(grid1_file, NF90_NOWRITE, nc_grid1_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName, 
     &                           'error opening grid1 file')) return

      ncstat = nf90_open(grid2_file, NF90_NOWRITE, nc_grid2_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error opening grid2 file')) return

      ncstat = nf90_inq_dimid(nc_grid1_id, 'grid_size', nc_grid1size_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid1 size id')) return

      ncstat = nf90_inquire_dimension(nc_grid1_id, nc_grid1size_id, 
     &                                len=grid1_size)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName, 
     &                           'error reading grid1 size')) return

      ncstat = nf90_inq_dimid(nc_grid2_id, 'grid_size', nc_grid2size_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid2 size id')) return

      ncstat = nf90_inquire_dimension(nc_grid2_id, nc_grid2size_id, 
     &                                len=grid2_size)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid2 size')) return

      ncstat = nf90_inq_dimid(nc_grid1_id, 'grid_rank', nc_grid1rank_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid1 rank id')) return

      ncstat = nf90_inquire_dimension(nc_grid1_id, nc_grid1rank_id, 
     &                                len=grid1_rank)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid1 rank')) return

      ncstat = nf90_inq_dimid(nc_grid2_id, 'grid_rank', nc_grid2rank_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid2 rank id')) return

      ncstat = nf90_inquire_dimension(nc_grid2_id, nc_grid2rank_id, 
     &                                len=grid2_rank)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid2 rank')) return

      ncstat = nf90_inq_dimid(nc_grid1_id, 'grid_corners', 
     &                                     nc_grid1corn_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                 'error reading grid1 num corner id')) return

      ncstat = nf90_inquire_dimension(nc_grid1_id, nc_grid1corn_id,
     &                                len=grid1_corners)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &          'error reading number of corners in grid1')) return

      ncstat = nf90_inq_dimid(nc_grid2_id, 'grid_corners', 
     &                                      nc_grid2corn_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                 'error reading grid2 num corner id')) return

      ncstat = nf90_inquire_dimension(nc_grid2_id, nc_grid2corn_id, 
     &                                len=grid2_corners)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &          'error reading number of corners in grid2')) return

      allocate( grid1_dims(grid1_rank),
     &          grid2_dims(grid2_rank))

      ncstat = nf90_get_att(nc_grid1_id, nf90_global, 'title',
     &                      grid1_name)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid1 name')) return

      ncstat = nf90_get_att(nc_grid2_id, nf90_global, 'title',
     &                      grid2_name)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid2 name')) return

!-----------------------------------------------------------------------
!
!     allocate grid coordinates/masks and read data
!
!-----------------------------------------------------------------------

      allocate( grid1_mask      (grid1_size),
     &          grid2_mask      (grid2_size),
     &          grid1_center_lat(grid1_size), 
     &          grid1_center_lon(grid1_size),
     &          grid2_center_lat(grid2_size), 
     &          grid2_center_lon(grid2_size),
     &          grid1_area      (grid1_size),
     &          grid1_area_in   (grid1_size),
     &          grid2_area      (grid2_size),
     &          grid2_area_in   (grid2_size),
     &          grid1_frac      (grid1_size),
     &          grid2_frac      (grid2_size),
     &          grid1_corner_lat(grid1_corners, grid1_size),
     &          grid1_corner_lon(grid1_corners, grid1_size),
     &          grid2_corner_lat(grid2_corners, grid2_size),
     &          grid2_corner_lon(grid2_corners, grid2_size),
     &          grid1_bound_box (4            , grid1_size),
     &          grid2_bound_box (4            , grid2_size))

      allocate(imask(grid1_size))

      ncstat = nf90_inq_varid(nc_grid1_id, 'grid_dims', 
     &                                     nc_grid1dims_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                'error reading id for grid1 dims')) return
      ncstat = nf90_inq_varid(nc_grid1_id, 'grid_imask', 
     &                                     nc_grd1imask_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading id for grid1 mask')) return
      ncstat = nf90_inq_varid(nc_grid1_id, 'grid_center_lat', 
     &                                   nc_grd1cntrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading id for grid center lats')) return
      ncstat = nf90_inq_varid(nc_grid1_id, 'grid_center_lon', 
     &                                   nc_grd1cntrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading id for grid center lons')) return
      ncstat = nf90_inq_varid(nc_grid1_id, 'grid_corner_lat', 
     &                                   nc_grd1crnrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading id for grid1 corner lats')) return
      ncstat = nf90_inq_varid(nc_grid1_id, 'grid_corner_lon', 
     &                                   nc_grd1crnrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading id for grid1 corner lons')) return

      ncstat = nf90_get_var(nc_grid1_id, nc_grid1dims_id, grid1_dims)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid1 dims')) return

      ncstat = nf90_get_var(nc_grid1_id, nc_grd1imask_id, imask)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid1 mask')) return

      ncstat = nf90_get_var(nc_grid1_id, nc_grd1cntrlat_id, 
     &                                   grid1_center_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid1 center lats')) return

      ncstat = nf90_get_var(nc_grid1_id, nc_grd1cntrlon_id, 
     &                                   grid1_center_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid1 center lons')) return

      ncstat = nf90_get_var(nc_grid1_id, nc_grd1crnrlat_id, 
     &                                   grid1_corner_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid1 corner lats')) return

      ncstat = nf90_get_var(nc_grid1_id, nc_grd1crnrlon_id, 
     &                                   grid1_corner_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid1 corner lons')) return

      if (luse_grid1_area) then
        ncstat = nf90_inq_varid(nc_grid1_id, 'grid_area', 
     &                                       nc_grid1area_id)
        if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                 'error getting id for grid1 area')) return
        ncstat = nf90_get_var(nc_grid1_id, nc_grid1area_id, 
     &                                     grid1_area_in)
        if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                 'error reading grid1 area')) return
      endif

      grid1_area = zero
      grid1_frac = zero

!-----------------------------------------------------------------------
!
!     initialize logical mask and convert lat/lon units if required
!
!-----------------------------------------------------------------------

      where (imask == 1)
        grid1_mask = .true.
      elsewhere
        grid1_mask = .false.
      endwhere
      deallocate(imask)

      grid1_units = ' '
      ncstat = nf90_get_att(nc_grid1_id, nc_grd1cntrlat_id, 'units',
     &                      grid1_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting grid1 center coord units')) return

      select case (grid1_units(1:7))
      case ('degrees')

        grid1_center_lat = grid1_center_lat*deg2rad
        grid1_center_lon = grid1_center_lon*deg2rad

      case ('radians')

        !*** no conversion necessary

      case default

        print *,'unknown units supplied for grid1 center lat/lon: '
        print *,'proceeding assuming radians'

      end select

      grid1_units = ' '
      ncstat = nf90_get_att(nc_grid1_id, nc_grd1crnrlat_id, 'units',
     &                      grid1_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid1 corner coord units')) return

      select case (grid1_units(1:7))
      case ('degrees')

        grid1_corner_lat = grid1_corner_lat*deg2rad
        grid1_corner_lon = grid1_corner_lon*deg2rad

      case ('radians')

        !*** no conversion necessary

      case default

        print *,'unknown units supplied for grid1 corner lat/lon: '
        print *,'proceeding assuming radians'

      end select

      ncstat = nf90_close(nc_grid1_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error closing grid1 file')) return

!-----------------------------------------------------------------------
!
!     read data for grid 2
!
!-----------------------------------------------------------------------

      allocate(imask(grid2_size))

      ncstat = nf90_inq_varid(nc_grid2_id, 'grid_dims', nc_grid2dims_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error getting grid2 dim ids')) return
      ncstat = nf90_inq_varid(nc_grid2_id, 'grid_imask', 
     &                                      nc_grd2imask_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error getting grid2 mask id')) return
      ncstat = nf90_inq_varid(nc_grid2_id, 'grid_center_lat', 
     &                                   nc_grd2cntrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting grid2 center lat id')) return
      ncstat = nf90_inq_varid(nc_grid2_id, 'grid_center_lon', 
     &                                   nc_grd2cntrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting grid2 center lon id')) return
      ncstat = nf90_inq_varid(nc_grid2_id, 'grid_corner_lat', 
     &                                   nc_grd2crnrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting grid2 corner lat id')) return
      ncstat = nf90_inq_varid(nc_grid2_id, 'grid_corner_lon', 
     &                                   nc_grd2crnrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting grid2 corner lon id')) return

      ncstat = nf90_get_var(nc_grid2_id, nc_grid2dims_id, grid2_dims)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid2 dims')) return

      ncstat = nf90_get_var(nc_grid2_id, nc_grd2imask_id, imask)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error reading grid2 mask')) return

      ncstat = nf90_get_var(nc_grid2_id, nc_grd2cntrlat_id, 
     &                                   grid2_center_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid2 center lat')) return

      ncstat = nf90_get_var(nc_grid2_id, nc_grd2cntrlon_id, 
     &                                   grid2_center_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid2 center lon')) return

      ncstat = nf90_get_var(nc_grid2_id, nc_grd2crnrlat_id, 
     &                                   grid2_corner_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid2 corner lat')) return

      ncstat = nf90_get_var(nc_grid2_id, nc_grd2crnrlon_id, 
     &                                   grid2_corner_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid2 corner lon')) return

      if (luse_grid2_area) then
        ncstat = nf90_inq_varid(nc_grid2_id, 'grid_area', 
     &                                       nc_grid2area_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting grid2 area id')) return
        ncstat = nf90_get_var(nc_grid2_id, nc_grid2area_id, 
     &                                     grid2_area_in)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid2 area')) return
      endif

      grid2_area = zero
      grid2_frac = zero

!-----------------------------------------------------------------------
!
!     initialize logical mask and convert lat/lon units if required
!
!-----------------------------------------------------------------------

      where (imask == 1)
        grid2_mask = .true.
      elsewhere
        grid2_mask = .false.
      endwhere
      deallocate(imask)

      grid2_units = ' '
      ncstat = nf90_get_att(nc_grid2_id, nc_grd2cntrlat_id, 'units',
     &                         grid2_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid2 center coord units')) return

      select case (grid2_units(1:7))
      case ('degrees')

        grid2_center_lat = grid2_center_lat*deg2rad
        grid2_center_lon = grid2_center_lon*deg2rad

      case ('radians')

        !*** no conversion necessary

      case default

        print *,'unknown units supplied for grid2 center lat/lon: '
        print *,'proceeding assuming radians'

      end select

      grid2_units = ' '
      ncstat = nf90_get_att(nc_grid2_id, nc_grd2crnrlat_id, 'units',
     &                      grid2_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading grid2 corner coord units')) return

      select case (grid2_units(1:7))
      case ('degrees')

        grid2_corner_lat = grid2_corner_lat*deg2rad
        grid2_corner_lon = grid2_corner_lon*deg2rad

      case ('radians')

        !*** no conversion necessary

      case default

        print *,'no units supplied for grid2 corner lat/lon: '
        print *,'proceeding assuming radians'

      end select

      ncstat = nf90_close(nc_grid2_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &                           'error closing grid2 file')) return


!-----------------------------------------------------------------------
!
!     convert longitudes to 0,2pi interval
!
!-----------------------------------------------------------------------

      where (grid1_center_lon .gt. pi2)  grid1_center_lon =
     &                                   grid1_center_lon - pi2
      where (grid1_center_lon .lt. zero) grid1_center_lon =
     &                                   grid1_center_lon + pi2
      where (grid2_center_lon .gt. pi2)  grid2_center_lon =
     &                                   grid2_center_lon - pi2
      where (grid2_center_lon .lt. zero) grid2_center_lon =
     &                                   grid2_center_lon + pi2
      where (grid1_corner_lon .gt. pi2)  grid1_corner_lon =
     &                                   grid1_corner_lon - pi2
      where (grid1_corner_lon .lt. zero) grid1_corner_lon =
     &                                   grid1_corner_lon + pi2
      where (grid2_corner_lon .gt. pi2)  grid2_corner_lon =
     &                                   grid2_corner_lon - pi2
      where (grid2_corner_lon .lt. zero) grid2_corner_lon =
     &                                   grid2_corner_lon + pi2

!-----------------------------------------------------------------------
!
!     make sure input latitude range is within the machine values
!     for +/- pi/2 
!
!-----------------------------------------------------------------------

      where (grid1_center_lat >  pih) grid1_center_lat =  pih
      where (grid1_corner_lat >  pih) grid1_corner_lat =  pih
      where (grid1_center_lat < -pih) grid1_center_lat = -pih
      where (grid1_corner_lat < -pih) grid1_corner_lat = -pih

      where (grid2_center_lat >  pih) grid2_center_lat =  pih
      where (grid2_corner_lat >  pih) grid2_corner_lat =  pih
      where (grid2_center_lat < -pih) grid2_center_lat = -pih
      where (grid2_corner_lat < -pih) grid2_corner_lat = -pih

!-----------------------------------------------------------------------
!
!     compute bounding boxes for restricting future grid searches
!
!-----------------------------------------------------------------------

      if (.not. luse_grid_centers) then
        grid1_bound_box(1,:) = minval(grid1_corner_lat, DIM=1)
        grid1_bound_box(2,:) = maxval(grid1_corner_lat, DIM=1)
        grid1_bound_box(3,:) = minval(grid1_corner_lon, DIM=1)
        grid1_bound_box(4,:) = maxval(grid1_corner_lon, DIM=1)

        grid2_bound_box(1,:) = minval(grid2_corner_lat, DIM=1)
        grid2_bound_box(2,:) = maxval(grid2_corner_lat, DIM=1)
        grid2_bound_box(3,:) = minval(grid2_corner_lon, DIM=1)
        grid2_bound_box(4,:) = maxval(grid2_corner_lon, DIM=1)

      else

        nx = grid1_dims(1)
        ny = grid1_dims(2)

        do n=1,grid1_size

          !*** find N,S and NE points to this grid point

          j = (n - 1)/nx +1
          i = n - (j-1)*nx

          if (i < nx) then
            ip1 = i + 1
          else
            !*** assume cyclic
            ip1 = 1
            !*** but if it is not, correct
            e_add = (j - 1)*nx + ip1
            if (abs(grid1_center_lat(e_add) - 
     &              grid1_center_lat(n   )) > pih) then
              ip1 = i
            endif
          endif

          if (j < ny) then
            jp1 = j+1
          else
            !*** assume cyclic
            jp1 = 1
            !*** but if it is not, correct
            n_add = (jp1 - 1)*nx + i
            if (abs(grid1_center_lat(n_add) - 
     &              grid1_center_lat(n   )) > pih) then
              jp1 = j
            endif
          endif

          n_add = (jp1 - 1)*nx + i
          e_add = (j - 1)*nx + ip1
          ne_add = (jp1 - 1)*nx + ip1

          !*** find N,S and NE lat/lon coords and check bounding box

          tmp_lats(1) = grid1_center_lat(n)
          tmp_lats(2) = grid1_center_lat(e_add)
          tmp_lats(3) = grid1_center_lat(ne_add)
          tmp_lats(4) = grid1_center_lat(n_add)

          tmp_lons(1) = grid1_center_lon(n)
          tmp_lons(2) = grid1_center_lon(e_add)
          tmp_lons(3) = grid1_center_lon(ne_add)
          tmp_lons(4) = grid1_center_lon(n_add)

          grid1_bound_box(1,n) = minval(tmp_lats)
          grid1_bound_box(2,n) = maxval(tmp_lats)
          grid1_bound_box(3,n) = minval(tmp_lons)
          grid1_bound_box(4,n) = maxval(tmp_lons)
        end do

        nx = grid2_dims(1)
        ny = grid2_dims(2)

        do n=1,grid2_size

          !*** find N,S and NE points to this grid point

          j = (n - 1)/nx +1
          i = n - (j-1)*nx

          if (i < nx) then
            ip1 = i + 1
          else
            !*** assume cyclic
            ip1 = 1
            !*** but if it is not, correct
            e_add = (j - 1)*nx + ip1
            if (abs(grid2_center_lat(e_add) - 
     &              grid2_center_lat(n   )) > pih) then
              ip1 = i
            endif
          endif

          if (j < ny) then
            jp1 = j+1
          else
            !*** assume cyclic
            jp1 = 1
            !*** but if it is not, correct
            n_add = (jp1 - 1)*nx + i
            if (abs(grid2_center_lat(n_add) - 
     &              grid2_center_lat(n   )) > pih) then
              jp1 = j
            endif
          endif

          n_add = (jp1 - 1)*nx + i
          e_add = (j - 1)*nx + ip1
          ne_add = (jp1 - 1)*nx + ip1

          !*** find N,S and NE lat/lon coords and check bounding box

          tmp_lats(1) = grid2_center_lat(n)
          tmp_lats(2) = grid2_center_lat(e_add)
          tmp_lats(3) = grid2_center_lat(ne_add)
          tmp_lats(4) = grid2_center_lat(n_add)

          tmp_lons(1) = grid2_center_lon(n)
          tmp_lons(2) = grid2_center_lon(e_add)
          tmp_lons(3) = grid2_center_lon(ne_add)
          tmp_lons(4) = grid2_center_lon(n_add)

          grid2_bound_box(1,n) = minval(tmp_lats)
          grid2_bound_box(2,n) = maxval(tmp_lats)
          grid2_bound_box(3,n) = minval(tmp_lons)
          grid2_bound_box(4,n) = maxval(tmp_lons)
        end do

      endif

      where (abs(grid1_bound_box(4,:) - grid1_bound_box(3,:)) > pi)
        grid1_bound_box(3,:) = zero
        grid1_bound_box(4,:) = pi2
      end where

      where (abs(grid2_bound_box(4,:) - grid2_bound_box(3,:)) > pi)
        grid2_bound_box(3,:) = zero
        grid2_bound_box(4,:) = pi2
      end where

      !***
      !*** try to check for cells that overlap poles
      !***

      where (grid1_center_lat > grid1_bound_box(2,:))
     &  grid1_bound_box(2,:) = pih

      where (grid1_center_lat < grid1_bound_box(1,:))
     &  grid1_bound_box(1,:) = -pih

      where (grid2_center_lat > grid2_bound_box(2,:))
     &  grid2_bound_box(2,:) = pih

      where (grid2_center_lat < grid2_bound_box(1,:))
     &  grid2_bound_box(1,:) = -pih

!-----------------------------------------------------------------------
!
!     set up and assign address ranges to search bins in order to 
!     further restrict later searches
!
!-----------------------------------------------------------------------

      select case (restrict_type)

      case ('latitude')
        write(SCRIP_stdout,*) 'Using latitude bins to restrict search.'

        allocate(bin_addr1(2,num_srch_bins))
        allocate(bin_addr2(2,num_srch_bins))
        allocate(bin_lats (2,num_srch_bins))
        allocate(bin_lons (2,num_srch_bins))

        dlat = pi/num_srch_bins

        do n=1,num_srch_bins
          bin_lats(1,n) = (n-1)*dlat - pih
          bin_lats(2,n) =     n*dlat - pih
          bin_lons(1,n) = zero
          bin_lons(2,n) = pi2
          bin_addr1(1,n) = grid1_size + 1
          bin_addr1(2,n) = 0
          bin_addr2(1,n) = grid2_size + 1
          bin_addr2(2,n) = 0
        end do

        do nele=1,grid1_size
          do n=1,num_srch_bins
            if (grid1_bound_box(1,nele) <= bin_lats(2,n) .and.
     &          grid1_bound_box(2,nele) >= bin_lats(1,n)) then
              bin_addr1(1,n) = min(nele,bin_addr1(1,n))
              bin_addr1(2,n) = max(nele,bin_addr1(2,n))
            endif
          end do
        end do

        do nele=1,grid2_size
          do n=1,num_srch_bins
            if (grid2_bound_box(1,nele) <= bin_lats(2,n) .and.
     &          grid2_bound_box(2,nele) >= bin_lats(1,n)) then
              bin_addr2(1,n) = min(nele,bin_addr2(1,n))
              bin_addr2(2,n) = max(nele,bin_addr2(2,n))
            endif
          end do
        end do

      case ('latlon')
        write(SCRIP_stdout,*) 'Using lat/lon boxes to restrict search.'

        dlat = pi /num_srch_bins
        dlon = pi2/num_srch_bins

        allocate(bin_addr1(2,num_srch_bins*num_srch_bins))
        allocate(bin_addr2(2,num_srch_bins*num_srch_bins))
        allocate(bin_lats (2,num_srch_bins*num_srch_bins))
        allocate(bin_lons (2,num_srch_bins*num_srch_bins))

        n = 0
        do j=1,num_srch_bins
        do i=1,num_srch_bins
          n = n + 1

          bin_lats(1,n) = (j-1)*dlat - pih
          bin_lats(2,n) =     j*dlat - pih
          bin_lons(1,n) = (i-1)*dlon
          bin_lons(2,n) =     i*dlon
          bin_addr1(1,n) = grid1_size + 1
          bin_addr1(2,n) = 0
          bin_addr2(1,n) = grid2_size + 1
          bin_addr2(2,n) = 0
        end do
        end do

        num_srch_bins = num_srch_bins**2

        do nele=1,grid1_size
          do n=1,num_srch_bins
            if (grid1_bound_box(1,nele) <= bin_lats(2,n) .and.
     &          grid1_bound_box(2,nele) >= bin_lats(1,n) .and.
     &          grid1_bound_box(3,nele) <= bin_lons(2,n) .and.
     &          grid1_bound_box(4,nele) >= bin_lons(1,n)) then
              bin_addr1(1,n) = min(nele,bin_addr1(1,n))
              bin_addr1(2,n) = max(nele,bin_addr1(2,n))
            endif
          end do
        end do

        do nele=1,grid2_size
          do n=1,num_srch_bins
            if (grid2_bound_box(1,nele) <= bin_lats(2,n) .and.
     &          grid2_bound_box(2,nele) >= bin_lats(1,n) .and.
     &          grid2_bound_box(3,nele) <= bin_lons(2,n) .and.
     &          grid2_bound_box(4,nele) >= bin_lons(1,n)) then
              bin_addr2(1,n) = min(nele,bin_addr2(1,n))
              bin_addr2(2,n) = max(nele,bin_addr2(2,n))
            endif
          end do
        end do

      case default
        stop 'unknown search restriction method'
      end select

!-----------------------------------------------------------------------
!
!     if area not read in, compute an area
!
!-----------------------------------------------------------------------

      if (.not. luse_grid1_area) then
         call SCRIP_GridComputeArea(grid1_area_in, grid1_corner_lat,
     &                              grid1_corner_lon, errorCode)

         if (SCRIP_ErrorCheck(errorCode, rtnName, 
     &                        'error computing grid1 area')) return
      endif

      if (.not. luse_grid2_area) then
         call SCRIP_GridComputeArea(grid2_area_in, grid2_corner_lat,
     &                              grid2_corner_lon, errorCode)

         if (SCRIP_ErrorCheck(errorCode, rtnName, 
     &                        'error computing grid2 area')) return
      endif

!-----------------------------------------------------------------------

      end subroutine grid_init

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_GridComputeArea -- computes grid cell areas
! !INTERFACE:

      subroutine SCRIP_GridComputeArea(area, cornerLat, cornerLon,
     &                                 errorCode)

! !DESCRIPTION:
!  This routine computes a grid cell area based on corner lat/lon
!  coordinates.  It is provided in the case that a user supplied
!  area is not available.
!
! !REVISION HISTORY:
!  same as module

! !OUTPUT PARAMETERS:

      real (SCRIP_r8), dimension(:), intent(out) ::
     &   area              ! computed area for each grid cell

      integer (SCRIP_i4), intent(out) ::
     &   errorCode         ! returned error code

! !INPUT PARAMETERS:

      real (SCRIP_r8), dimension(:,:), intent(in) ::
     &   cornerLat,        ! latitude  of each cell corner
     &   cornerLon         ! longitude of each cell corner

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4) ::
     &   numCells,           ! number of grid cells
     &   numCorners,         ! number of corners in each cell
     &   nCell,              ! loop index for grid cells
     &   nCorner,            ! loop index for corners in each cell
     &   nextCorner          ! next corner around cell perimeter

      real (SCRIP_r8) ::
     &   dphi                ! delta(longitude) for this segment
     
!-----------------------------------------------------------------------
!
!  determine size of grid and initialize
!
!-----------------------------------------------------------------------

      errorCode = SCRIP_Success

      numCells   = size(CornerLat, dim=2)
      numCorners = size(CornerLat, dim=1)

!-----------------------------------------------------------------------
!
!  compute area for each cell by integrating around cell edge
!
!-----------------------------------------------------------------------

      do nCell=1,numCells

         Area(nCell) = 0.0_SCRIP_r8

         do nCorner=1,numCorners
            nextCorner = mod(nCorner,numCorners) + 1

            !*** trapezoid rule - delta(Lon) is -0.5*dx
            dphi = CornerLon(   nCorner,nCell) - 
     &             CornerLon(nextCorner,nCell)
            if (dphi > pi) then
               dphi = dphi - pi2
            else if (dphi < -pi) then
               dphi = dphi + pi2
            endif
            dphi = 0.5_SCRIP_r8*dphi

            Area(nCell) = Area(nCell) + 
     &                    dphi*(sin(CornerLat(   nCorner,nCell)) +
     &                          sin(CornerLat(nextCorner,nCell)))
         end do

      end do

!-----------------------------------------------------------------------
!EOC

      end subroutine SCRIP_GridComputeArea

!***********************************************************************

      end module grids

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

