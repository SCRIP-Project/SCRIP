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
      use timers

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
     &             grid2_frac,        ! participating in remapping
     &             grid1_centroid_lat,! Centroid of grid1 cell
     &             grid1_centroid_lon,!
     &             grid2_centroid_lat,! Centroid of grid2 cell
     &             grid2_centroid_lon !


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

      integer (SCRIP_i4), save ::    ! Cells overlapping the poles (may be 0)
     &     grid1_npole_cell,        
     &     grid1_spole_cell,         
     &     grid2_npole_cell,
     &     grid2_spole_cell

      logical (SCRIP_logical), save ::   ! grid wrap around
     &     grid1_iwrap,
     &     grid1_jwrap,
     &     grid2_iwrap,
     &     grid2_jwrap
      

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

      integer (SCRIP_i4), dimension(:), allocatable, save ::
     &        bin_sort1, ! global indexes sorted into bin friendly
     &        bin_sort2  ! order for grid1 and grid2

      real(SCRIP_r8), dimension(:,:), allocatable, save ::
     &        bin_lats1   ! min,max latitude for each search bin grid1
     &,       bin_lons1   ! min,max longitude for each search bin grid1
     &,       bin_lats2   ! min,max latitude for each search bin grid2
     &,       bin_lons2   ! min,max longitude for each search bin grid2

      integer (SCRIP_i4) ::
     &        bin_max   ! current max size of bin_list
      integer (SCRIP_i4), dimension(:), allocatable ::
     &        bin_cnt   ! number of cells in each bin
      integer (SCRIP_i4), dimension(:,:), allocatable ::
     &        bin_list, ! list of cells for each bin
     &        bin_listhold ! temporary to increase bin_list size

!-----------------------------------------------------------------------
!
!     Parameters for dealing with intersections in the polar region
!
!-----------------------------------------------------------------------

      real (SCRIP_r8), save :: 
     &     north_thresh,  ! threshold for coord transf.
     &     south_thresh   ! threshold for coord transf.


      !*** Number of subsegments used to represents edges near
      !*** the polar regions - choose an odd number to avoid obvious
      !*** degeneracies in intersection

      integer (SCRIP_i4), save ::
     &     npseg                    

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
     &  n,np      ! loop counter
     &, nele   ! element loop counter
     &, i,j,k
     &, ip1,jp1
     &, n_add, e_add, ne_add
     &, nx, ny, n1, n2

      integer (SCRIP_i4) ::
     &     zero_crossing, pi_crossing,
     &     grid1_add, grid2_add,
     &     corner, next_corn, progint
     
      real (SCRIP_r8) ::
     &     beglon, endlon, endlat

      logical (SCRIP_logical) ::
     &     found


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

      real (SCRIP_r8) :: 
     &  minlon,maxlon,minlat,maxlat   ! lat/lon bin extents

      real (SCRIP_r8), dimension(4) ::
     &  tmp_lats, tmp_lons  ! temps for computing bounding boxes

      character (9), parameter ::
     &   rtnName = 'grid_init'

      real (SCRIP_r8), parameter :: 
     &   eps = 1.0e-06

      integer (SCRIP_i4) ::
     &        num_srch_binsx, num_srch_binsy  ! num of bins in lon/lat

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
     &          grid2_bound_box (4            , grid2_size),
     &          grid1_centroid_lat(grid1_size),
     &          grid1_centroid_lon(grid1_size),
     &          grid2_centroid_lat(grid2_size),
     &          grid2_centroid_lon(grid2_size))

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
      grid1_centroid_lat = zero
      grid1_centroid_lon = zero

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
      grid2_centroid_lat = zero
      grid2_centroid_lon = zero

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
!     also, different grids consider the pole to be a slightly different
!     values (1.570796326789 vs 1.5707963267977). Move such points that
!     are practically at the pole to the pole to avoid problems
!
!-----------------------------------------------------------------------

      where (abs(grid1_corner_lat-pih) < 1.e-05) grid1_corner_lat =  pih
      where (abs(grid1_corner_lat+pih) < 1.e-05) grid1_corner_lat = -pih
      where (abs(grid2_corner_lat-pih) < 1.e-05) grid2_corner_lat =  pih
      where (abs(grid2_corner_lat+pih) < 1.e-05) grid2_corner_lat = -pih


!-----------------------------------------------------------------------
!
!    Check for grid wrap-arounds
!    Default is true (for backwards compatability)
!
!-----------------------------------------------------------------------

      grid1_iwrap = .true.
      grid1_jwrap = .true.
      grid2_iwrap = .true.
      grid2_jwrap = .true.

      if (grid1_corners == 4) then
        nx = grid1_dims(1)
        ny = grid1_dims(2)
        do j = 1,ny
          n1 = (j-1)*nx + 1
          n2 = (j-1)*nx + nx
          if (abs(grid1_corner_lat(4,n1)-
     &            grid1_corner_lat(3,n2)) > eps .or.
     &        abs(mod(grid1_corner_lon(4,n1)+pi2,pi2)-
     &            mod(grid1_corner_lon(3,n2)+pi2,pi2)) > eps) then
!            print*, 'grid1_iwrap ',
!     &        grid1_corner_lon(4,n1),grid1_corner_lon(3,n2),
!     &        grid1_corner_lat(4,n1),grid1_corner_lat(3,n2)
            grid1_iwrap = .false.
          endif
        enddo
        do i = 1,nx
          n1 = i
          n2 = (ny-1)*nx + i
          if (abs(grid1_corner_lat(2,n1)-
     &            grid1_corner_lat(3,n2)) > eps .or.
     &        abs(mod(grid1_corner_lon(2,n1)+pi2,pi2)-
     &            mod(grid1_corner_lon(3,n2)+pi2,pi2)) > eps) then
!            print*, 'grid1_jwrap ',
!     &        grid1_corner_lon(2,n1),grid1_corner_lon(3,n2),
!     &        grid1_corner_lat(2,n1),grid1_corner_lat(3,n2)
            grid1_jwrap = .false.
          endif
        enddo

        nx = grid2_dims(1)
        ny = grid2_dims(2)
        do j = 1,ny
          n1 = (j-1)*nx + 1
          n2 = (j-1)*nx + nx
          if (abs(grid2_corner_lat(4,n1)-
     &            grid2_corner_lat(3,n2)) > eps .or.
     &        abs(mod(grid2_corner_lon(4,n1)+pi2,pi2)-
     &            mod(grid2_corner_lon(3,n2)+pi2,pi2)) > eps) then
!            print*, 'grid2_iwrap ',
!     &        grid2_corner_lon(4,n1),grid2_corner_lon(3,n2),
!     &        grid2_corner_lat(4,n1),grid2_corner_lat(3,n2)
            grid2_iwrap = .false.
          endif
        enddo
        do i = 1,nx
          n1 = i
          n2 = (ny-1)*nx + i
          if (abs(grid2_corner_lat(2,n1)-
     &            grid2_corner_lat(3,n2)) > eps .or.
     &        abs(mod(grid2_corner_lon(2,n1)+pi2,pi2)-
     &            mod(grid2_corner_lon(3,n2)+pi2,pi2)) > eps) then
!            print*, 'grid2_jwrap ',
!     &        grid2_corner_lon(2,n1),grid2_corner_lon(3,n2),
!     &        grid2_corner_lat(2,n1),grid2_corner_lat(3,n2)
            grid2_jwrap = .false.
          endif
        enddo
      endif

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
            if (grid1_iwrap) then
              ip1 = 1
            else
              ip1 = i
            endif
          endif

          if (j < ny) then
            jp1 = j+1
          else
            if (grid1_jwrap) then
              jp1 = 1
            else
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
            if (grid2_iwrap) then
              ip1 = 1
            else
              ip1 = i
            endif
          endif

          if (j < ny) then
            jp1 = j+1
          else
            if (grid2_jwrap) then
              jp1 = 1
            else
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

      where (grid1_center_lat > grid1_bound_box(2,:))
     &  grid1_bound_box(2,:) = pih

      where (grid1_center_lat < grid1_bound_box(1,:))
     &  grid1_bound_box(1,:) = -pih

      where (grid2_center_lat > grid2_bound_box(2,:))
     &  grid2_bound_box(2,:) = pih

      where (grid2_center_lat < grid2_bound_box(1,:))
     &  grid2_bound_box(1,:) = -pih


      !***
      !*** Check for cells that overlap poles and explicitly
      !*** store their addresses
      !***

      grid1_npole_cell = 0
      grid1_spole_cell = 0
         
      do grid1_add = 1, grid1_size

         found = .false.
         do corner = 1, grid1_corners
            endlat = grid1_corner_lat(corner,grid1_add)
            if (abs(abs(endlat)-pih) .lt. 1e-5) then
               found = .true.   ! cell has polar pnt; so pole is 
                                ! not in the interior of the cell
               exit
            endif
         enddo

         if (found) cycle


         beglon = grid1_corner_lon(1,grid1_add)
         zero_crossing = 0
         pi_crossing = 0

         do corner = 1, grid1_corners
            next_corn = mod(corner,grid1_corners) + 1
            endlon = grid1_corner_lon(next_corn,grid1_add)

            if (abs(beglon-endlon) .gt. pi) then
               zero_crossing = 1
            else
               if ((beglon .lt. pi .and. endlon .ge. pi) .or.
     &              (endlon .lt. pi .and. beglon .ge. pi)) then
                  pi_crossing = 1
               endif
            endif

            beglon = endlon
         enddo
         
         if (zero_crossing .eq. 1 .and. pi_crossing .eq. 1) then

            !***
            !*** We have a polar cell
            !***

            if (grid1_center_lat(grid1_add) .gt. 0) then
               grid1_npole_cell = grid1_add
            else if (grid1_center_lat(grid1_add) .lt. 0) then
               grid1_spole_cell = grid1_add
            endif

            if (grid1_npole_cell .ne. 0 .and.
     &           grid1_spole_cell .ne. 0) then
               exit
            endif

         endif

      enddo



      grid2_npole_cell = 0
      grid2_spole_cell = 0

      do grid2_add = 1, grid2_size

         found = .false.
         do corner = 1, grid2_corners
            endlat = grid2_corner_lat(corner,grid2_add)
            if (abs(abs(endlat)-pih) .lt. 1e-5) then
               found = .true.   ! cell has polar pnt; so pole is 
                                ! not in the interior of the cell
               exit
            endif
         enddo

         if (found) cycle

         beglon = grid2_corner_lon(1,grid2_add)
         zero_crossing = 0
         pi_crossing = 0

         do corner = 1, grid2_corners
            next_corn = mod(corner,grid2_corners) + 1
            endlon = grid2_corner_lon(next_corn,grid2_add)
            
            if (abs(beglon-endlon) > pi) then
               zero_crossing = 1
            else
               if ((beglon .lt. pi .and. endlon .ge. pi) .or.
     &              (endlon .lt. pi .and. beglon .ge. pi)) then
                  pi_crossing = 1
               endif
            endif

            beglon = endlon
         enddo
         
         if (zero_crossing .eq. 1 .and. pi_crossing .eq. 1) then

            !***
            !*** We have a polar cell
            !***

            if (grid2_center_lat(grid2_add) .gt. 0) then
               grid2_npole_cell = grid2_add
            else if (grid2_center_lat(grid2_add) .lt. 0) then
               grid2_spole_cell = grid2_add
            endif

            if (grid2_npole_cell .ne. 0 .and.
     &           grid2_spole_cell .ne. 0) then
               exit
            endif

         endif

      enddo


      print *, ' '
      print *, 'Grid 1 size', grid1_dims,grid1_size
      print *, 'Grid 1 wrap', grid1_iwrap,grid1_jwrap
      print *, 'Grid 2 size', grid2_dims,grid2_size
      print *, 'Grid 2 wrap', grid2_iwrap,grid2_jwrap


      print *, 'grid1_npole_cell',grid1_npole_cell
      if (grid1_npole_cell .gt. 0) then
         do i = 1, grid1_corners
            print *, grid1_corner_lat(i,grid1_npole_cell),
     &           grid1_corner_lon(i,grid1_npole_cell)
         enddo
      endif
      print *, 'grid1_spole_cell',grid1_spole_cell
      if (grid1_spole_cell .gt. 0) then
         do i = 1, grid1_corners
            print *, grid1_corner_lat(i,grid1_spole_cell), 
     &           grid1_corner_lon(i,grid1_spole_cell)
         enddo
      endif
      print *, 'grid2_npole_cell',grid2_npole_cell
      if (grid2_npole_cell .gt. 0) then
         do i = 1, grid2_corners
            print *, grid2_corner_lat(i,grid2_npole_cell), 
     &           grid2_corner_lon(i,grid2_npole_cell)
         enddo
      endif
      print *, 'grid2_spole_cell',grid2_spole_cell
      if (grid2_spole_cell .gt. 0) then
         do i = 1, grid2_corners
            print *, grid2_corner_lat(i,grid2_spole_cell), 
     &           grid2_corner_lon(i,grid2_spole_cell)
         enddo
      endif
      print *


!-----------------------------------------------------------------------
!
!     set up and assign address ranges to search bins in order to 
!     further restrict later searches
!
!-----------------------------------------------------------------------

      call timer_start(11,'bin_init')

      select case (restrict_type)

      case ('latitude')
        write(SCRIP_stdout,*) 'Using latitude bins to restrict search.'

        num_srch_binsx = 1
        num_srch_binsy = num_srch_bins

      case ('longitude')
        write(SCRIP_stdout,*) 'Using longitude bins to restrict search.'

        num_srch_binsx = num_srch_bins
        num_srch_binsy = 1

      case ('latlon')
        write(SCRIP_stdout,*) 'Using lat/lon boxes to restrict search.'
        num_srch_binsx = num_srch_bins
        num_srch_binsy = num_srch_bins

      case default
        stop 'unknown search restriction method'
      end select


      ! reset num_srch_bins to total
      num_srch_bins = num_srch_binsx*num_srch_binsy

      minlat = min(minval(grid1_bound_box(1,:)),
     &             minval(grid2_bound_box(1,:)))
      maxlat = max(maxval(grid1_bound_box(2,:)),
     &             maxval(grid2_bound_box(2,:)))
      minlon = min(minval(grid1_bound_box(3,:)),
     &             minval(grid2_bound_box(3,:)))
      maxlon = max(maxval(grid1_bound_box(4,:)),
     &             maxval(grid2_bound_box(4,:)))

      minlon = max(zero,minlon-eps)
      maxlon = min(pi2,maxlon+eps)
      minlat = max(-pih,minlat-eps)
      maxlat = min(pih,maxlat+eps)

      dlon = (maxlon-minlon)/num_srch_binsx
      dlat = (maxlat-minlat)/num_srch_binsy

      write(6,*) 'bins min max d lon ',minlon,maxlon,dlon
      write(6,*) 'bins min max d lat ',minlat,maxlat,dlat

      allocate(bin_lats1(2,num_srch_bins))
      allocate(bin_lons1(2,num_srch_bins))
      allocate(bin_lats2(2,num_srch_bins))
      allocate(bin_lons2(2,num_srch_bins))
      allocate(bin_addr1(2,num_srch_bins))
      allocate(bin_addr2(2,num_srch_bins))
      allocate(bin_sort1(grid1_size))
      allocate(bin_sort2(grid2_size))

      n = 0
      do j=1,num_srch_binsy
      do i=1,num_srch_binsx
        n = n + 1
        bin_lats1(1,n) = (j-1)*dlat + minlat
        bin_lats1(2,n) =     j*dlat + minlat
        bin_lons1(1,n) = (i-1)*dlon + minlon
        bin_lons1(2,n) =     i*dlon + minlon
      end do
      end do

      ! for now, initialize bin_lonlat2 = bin_lonlat1
      bin_lats2 = bin_lats1
      bin_lons2 = bin_lons1

!      write(6,*) ' '
!      do n = 1,num_srch_bins
!         write(6,*) 'bin_lonlat1i ',n,bin_lons1(1,n),bin_lons1(2,n),
!     &        bin_lats1(1,n),bin_lats1(2,n)
!      enddo
!      write(6,*) ' '
!      do n = 1,num_srch_bins
!         write(6,*) 'bin_lonlat2i ',n,bin_lons2(1,n),bin_lons2(2,n),
!     &        bin_lats2(1,n),bin_lats2(2,n)
!      enddo

      bin_addr1(1,:) = 1
      bin_addr1(2,:) = 0
      bin_addr2(1,:) = 1
      bin_addr2(2,:) = 0
      bin_sort1 = 0
      bin_sort2 = 0

      if (grid1_size > 1000000) then
        progint = 100000
      else
        progint = 10000
      endif

      bin_max = 1.2*grid1_size/num_srch_bins + 1
      write(6,*) 'bin_max grid1 ',num_srch_bins,grid1_size,bin_max
      allocate(bin_cnt(num_srch_bins))
      allocate(bin_list(bin_max,num_srch_bins))
      bin_cnt = 0
      bin_list = 0

      do nele=1,grid1_size
        if (mod(nele,progint) .eq. 0) then
          print *, nele, ' grid1 cells bin sorted ...'
        endif
        n = 0  ! num_srch_bins
        found = .false.
        do while (.not.found .and. n < num_srch_bins)
          n = n + 1
          if (grid1_center_lat(nele) <= bin_lats1(2,n) .and.
     &        grid1_center_lat(nele) >= bin_lats1(1,n) .and.
     &        grid1_center_lon(nele) <= bin_lons1(2,n) .and.
     &        grid1_center_lon(nele) >= bin_lons1(1,n)) then

            found = .true.

            bin_cnt(n) = bin_cnt(n) + 1

            if (bin_cnt(n) > bin_max) then
               allocate(bin_listhold(bin_max,num_srch_bins))
               do j = 1,num_srch_bins
               do i = 1,bin_cnt(j)
                  bin_listhold(i,j) = bin_list(i,j)
               enddo
               enddo
               deallocate(bin_list)
               bin_max = 1.5*bin_max + 1
               write(6,*) 'increasing bin_max grid1 to ',bin_max
               allocate(bin_list(bin_max,num_srch_bins))
               bin_list = 0
               do j = 1,num_srch_bins
               do i = 1,bin_cnt(j)
                  bin_list(i,j) = bin_listhold(i,j)
               enddo
               enddo
               deallocate(bin_listhold)
            endif

            bin_list(bin_cnt(n),n) = nele

          endif
        end do
        if (.not.found) then
          write(6,*) 'ERROR bin_sort1 ',nele,grid1_center_lon(nele),
     &               grid1_center_lat(nele)
          stop
        endif
      end do   ! nele

      n = 0
      do j = 1,num_srch_bins
         if (j == 1) then
            bin_addr1(1,j) = 1
         else
            bin_addr1(1,j) = bin_addr1(2,j-1) + 1
         endif
         bin_addr1(2,j) = bin_addr1(1,j) + bin_cnt(j) - 1
         do i = 1,bin_cnt(j)
            n = n + 1
            bin_sort1(n) = bin_list(i,j)
         enddo
      enddo
      if (n .ne. grid1_size .or. 
     &   bin_addr1(2,num_srch_bins) .ne. grid1_size) then
         write(6,*) 'ERROR in size1 ',grid1_size,n,
     &      bin_addr1(2,num_srch_bins)
         stop
      endif
      deallocate(bin_list)
      deallocate(bin_cnt)
      
      bin_max = 1.2*grid2_size/num_srch_bins + 1
      write(6,*) 'bin_max grid2 ',num_srch_bins,grid2_size,bin_max
      allocate(bin_cnt(num_srch_bins))
      allocate(bin_list(bin_max,num_srch_bins))
      bin_cnt = 0
      bin_list = 0

      if (grid2_size > 1000000) then
        progint = 100000
      else
        progint = 10000
      endif

      do nele=1,grid2_size
        if (mod(nele,progint) .eq. 0) then
          print *, nele, ' grid2 cells bin sorted ...'
        endif
        n = 0  ! num_srch_bins
        found = .false.
        do while (.not.found .and. n < num_srch_bins)
          n = n + 1
          if (grid2_center_lat(nele) <= bin_lats2(2,n) .and.
     &        grid2_center_lat(nele) >= bin_lats2(1,n) .and.
     &        grid2_center_lon(nele) <= bin_lons2(2,n) .and.
     &        grid2_center_lon(nele) >= bin_lons2(1,n)) then

            found = .true.

            bin_cnt(n) = bin_cnt(n) + 1

            if (bin_cnt(n) > bin_max) then
               allocate(bin_listhold(bin_max,num_srch_bins))
               do j = 1,num_srch_bins
               do i = 1,bin_cnt(j)
                  bin_listhold(i,j) = bin_list(i,j)
               enddo
               enddo
               deallocate(bin_list)
               bin_max = 1.5*bin_max + 1
               write(6,*) 'increasing bin_max grid2 to ',bin_max
               allocate(bin_list(bin_max,num_srch_bins))
               bin_list = 0
               do j = 1,num_srch_bins
               do i = 1,bin_cnt(j)
                  bin_list(i,j) = bin_listhold(i,j)
               enddo
               enddo
               deallocate(bin_listhold)
            endif

            bin_list(bin_cnt(n),n) = nele

          endif
        end do
        if (.not.found) then
          write(6,*) 'ERROR bin_sort2 ',nele,grid2_center_lon(nele),
     &               grid2_center_lat(nele)
          stop
        endif
      end do   ! nele

!----------
!      do nele=1,grid1_size
!        if (bin_sort1(nele) == 0) 
!     &     write(6,*) 'sort1 error ',nele,bin_sort1(nele)
!      enddo

      n = 0
      do j = 1,num_srch_bins
         if (j == 1) then
            bin_addr2(1,j) = 1
         else
            bin_addr2(1,j) = bin_addr2(2,j-1) + 1
         endif
         bin_addr2(2,j) = bin_addr2(1,j) + bin_cnt(j) - 1
         do i = 1,bin_cnt(j)
            n = n + 1
            bin_sort2(n) = bin_list(i,j)
         enddo
      enddo
      if (n .ne. grid2_size .or. 
     &   bin_addr2(2,num_srch_bins) .ne. grid2_size) then
         write(6,*) 'ERROR in size2 ',grid2_size,n,
     &      bin_addr2(2,num_srch_bins)
         stop
      endif
      deallocate(bin_list)
      deallocate(bin_cnt)

!----------
!      do nele=1,grid2_size
!        if (bin_sort2(nele) == 0) 
!     &     write(6,*) 'sort2 error ',nele,bin_sort2(nele)
!      enddo

      ! update bin_lats, bin_lons to take into account full bound 
      ! box of cells associated with the bin
      do n = 1,num_srch_bins
        do np = bin_addr1(1,n),bin_addr1(2,n)
          bin_lats1(1,n) = min(bin_lats1(1,n),
     &                         grid1_bound_box(1,bin_sort1(np)))
          bin_lats1(2,n) = max(bin_lats1(2,n),
     &                         grid1_bound_box(2,bin_sort1(np)))
          bin_lons1(1,n) = min(bin_lons1(1,n),
     &                         grid1_bound_box(3,bin_sort1(np)))
          bin_lons1(2,n) = max(bin_lons1(2,n),
     &                         grid1_bound_box(4,bin_sort1(np)))
        enddo
      enddo

      do n = 1,num_srch_bins
        do np = bin_addr2(1,n),bin_addr2(2,n)
          bin_lats2(1,n) = min(bin_lats2(1,n),
     &                         grid2_bound_box(1,bin_sort2(np)))
          bin_lats2(2,n) = max(bin_lats2(2,n),
     &                         grid2_bound_box(2,bin_sort2(np)))
          bin_lons2(1,n) = min(bin_lons2(1,n),
     &                         grid2_bound_box(3,bin_sort2(np)))
          bin_lons2(2,n) = max(bin_lons2(2,n),
     &                         grid2_bound_box(4,bin_sort2(np)))
        enddo
      enddo

      call timer_stop(11)
      call timer_print(11)
      call timer_clear(11)

!      write(6,*) ' '
!      do n = 1,num_srch_bins
!         write(6,*) 'bin_lonlat1f ',n,bin_lons1(1,n),bin_lons1(2,n),
!     &        bin_lats1(1,n),bin_lats1(2,n)
!      enddo
!      do n = 1,num_srch_bins
!         write(6,*) 'bin_addr1 ',n,bin_addr1(1,n),bin_addr1(2,n),
!     &        bin_sort1(bin_addr1(1,n)),bin_sort1(bin_addr1(2,n))
!      enddo
!      write(6,*) ' '
!      do n = 1,num_srch_bins
!         write(6,*) 'bin_lonlat2f ',n,bin_lons2(1,n),bin_lons2(2,n),
!     &        bin_lats2(1,n),bin_lats2(2,n)
!      enddo
!      do n = 1,num_srch_bins
!         write(6,*) 'bin_addr2 ',n,bin_addr2(1,n),bin_addr2(2,n),
!     &        bin_sort2(bin_addr2(1,n)),bin_sort2(bin_addr2(2,n))
!      enddo
!      write(6,*) ' '

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

