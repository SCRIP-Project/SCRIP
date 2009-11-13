!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This routine reads remapping information from files written
!     by remap_setup.  If remapping in both directions are required,
!     two input files must be specified.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: remap_read.f,v 1.6 2000/04/19 21:56:26 pwjones Exp $
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

      module remap_read

!-----------------------------------------------------------------------
!
!     contains routines for reading a remap file
!
!-----------------------------------------------------------------------

      use SCRIP_KindsMod   ! defines common data types
      use SCRIP_ErrorMod   ! SCRIP error handler
      use SCRIP_NetcdfMod  ! module with netCDF error handler
      use netcdf           ! module for netCDF library

      use constants     ! defines useful constants
      use grids         ! includes all grid information
      use remap_vars    ! module for all required remapping variables

      implicit none

!-----------------------------------------------------------------------
!
!     module variables
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     various netCDF ids for files variables
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4), private :: ! netCDF ids
     &         ncstat, nc_file_id,
     &         nc_srcgrdsize_id, nc_dstgrdsize_id,
     &         nc_srcgrdcorn_id, nc_dstgrdcorn_id,
     &         nc_srcgrdrank_id, nc_dstgrdrank_id,
     &         nc_srcgrddims_id, nc_dstgrddims_id,
     &         nc_numlinks_id, nc_numwgts_id, 
     &         nc_srcgrdimask_id, nc_dstgrdimask_id,
     &         nc_srcgrdcntrlat_id, nc_srcgrdcntrlon_id,
     &         nc_srcgrdcrnrlat_id, nc_srcgrdcrnrlon_id,
     &         nc_srcgrdarea_id, nc_srcgrdfrac_id,
     &         nc_dstgrdcntrlat_id, nc_dstgrdcntrlon_id,
     &         nc_dstgrdcrnrlat_id, nc_dstgrdcrnrlon_id,
     &         nc_dstgrdarea_id, nc_dstgrdfrac_id,
     &         nc_srcgrdadd_id, nc_dstgrdadd_id, nc_rmpmatrix_id

!***********************************************************************

      contains

!***********************************************************************

      subroutine read_remap(map_name, interp_file, errorCode)

!-----------------------------------------------------------------------
!
!     this driver routine reads some global attributes and then
!     calls a specific read routine based on file conventions
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      character(SCRIP_charLength), intent(in) ::
     &  interp_file        ! filename for remap data

!-----------------------------------------------------------------------
!
!     output variables
!
!-----------------------------------------------------------------------

      character(SCRIP_charLength), intent(out) ::
     &  map_name            ! name for mapping

      integer (SCRIP_i4), intent(out) ::
     &   errorCode          ! returned error code

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      character(SCRIP_charLength) :: 
     &   map_method       ! character string for map_type
     &,  normalize_opt    ! character string for normalization option
     &,  convention       ! character string for output convention

      character (10), parameter :: 
     &   rtnName = 'read_remap'

!-----------------------------------------------------------------------
!
!     open file and read some global information
!
!-----------------------------------------------------------------------

      errorCode = SCRIP_Success

      ncstat = nf90_open(interp_file, NF90_NOWRITE, nc_file_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error opening remap file')) return

      !***
      !*** map name
      !***
      map_name = ' '
      ncstat = nf90_get_att(nc_file_id, NF90_GLOBAL, 'title',
     &                      map_name)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading remap name')) return

      print *,'Reading remapping:',trim(map_name)
      print *,'From file:',trim(interp_file)

      !***
      !*** normalization option
      !***
      normalize_opt = ' '
      ncstat = nf90_get_att(nc_file_id, NF90_GLOBAL, 'normalization',
     &                      normalize_opt)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading normalization option')) return

      select case(normalize_opt)
      case ('none')
        norm_opt = norm_opt_none
      case ('fracarea')
        norm_opt = norm_opt_frcarea
      case ('destarea')
        norm_opt = norm_opt_dstarea
      case default
         call SCRIP_ErrorSet(errorCode, rtnName,
     &                       'Invalid normalization option')
         return
      end select

      !***
      !*** map method
      !***
      map_method = ' '
      ncstat = nf90_get_att(nc_file_id, NF90_GLOBAL, 'map_method',
     &                      map_method)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading remap method')) return

      select case(map_method)
      case('Conservative remapping')
        map_type = map_type_conserv
      case('Bilinear remapping')
        map_type = map_type_bilinear
      case('Distance weighted avg of nearest neighbors')
        map_type = map_type_distwgt
      case('Bicubic remapping')
        map_type = map_type_bicubic
      case('Particle remapping')
        map_type = map_type_particle
      case default
         call SCRIP_ErrorSet(errorCode, rtnName, 'Invalid Map Type')
         return
      end select

      !***
      !*** file convention
      !***
      convention = ' '
      ncstat = nf90_get_att(nc_file_id, NF90_GLOBAL, 'conventions',
     &                      convention)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading file convention')) return

!-----------------------------------------------------------------------
!
!     call appropriate read routine based on output convention
!
!-----------------------------------------------------------------------

      select case(convention)

      case ('SCRIP')

         call read_remap_scrip(errorCode)
         if (SCRIP_ErrorCheck(errorCode, rtnName, 
     &                        'error in read_remap_scrip')) return

      case ('NCAR-CSM')

         call read_remap_csm(errorCode)
         if (SCRIP_ErrorCheck(errorCode, rtnName, 
     &                        'error in read_remap_csm')) return

      case default

         call SCRIP_ErrorSet(errorCode, rtnName,
     &                       'unknown output file convention')
         return

      end select

!-----------------------------------------------------------------------

      end subroutine read_remap

!***********************************************************************

      subroutine read_remap_scrip(errorCode)

!-----------------------------------------------------------------------
!
!     the routine reads a netCDF file to extract remapping info
!     in SCRIP format
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4), intent(out) ::
     &   errorCode        ! returned error code

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      character (SCRIP_charLength) ::
     &  grid1_name           ! grid name for source grid
     &, grid2_name           ! grid name for dest   grid

      integer (SCRIP_i4), dimension(:), allocatable ::
     &  grid1_mask_int,      ! integer masks to determine
     &  grid2_mask_int       ! cells that participate in map

      character (16), parameter :: 
     &   rtnName = 'read_remap_scrip'

!-----------------------------------------------------------------------
!
!     read some additional global attributes
!
!-----------------------------------------------------------------------

      errorCode = SCRIP_Success

      !***
      !*** source and destination grid names
      !***

      grid1_name = ' '
      grid2_name = ' '
      ncstat = nf90_get_att(nc_file_id, NF90_GLOBAL, 'source_grid',
     &                      grid1_name)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid name')) return

      ncstat = nf90_get_att(nc_file_id, NF90_GLOBAL, 'dest_grid',
     &                      grid2_name)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid name')) return

      print *,' '
      print *,'Remapping between:',trim(grid1_name)
      print *,'and ',trim(grid2_name)
      print *,' '

!-----------------------------------------------------------------------
!
!     read dimension information
!
!-----------------------------------------------------------------------

      ncstat = nf90_inq_dimid(nc_file_id, 'src_grid_size', 
     &                        nc_srcgrdsize_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid size id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_srcgrdsize_id, 
     &                                len=grid1_size)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid size')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'dst_grid_size', 
     &                        nc_dstgrdsize_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting destination grid size id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_dstgrdsize_id, 
     &                                len=grid2_size)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid size')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'src_grid_corners', 
     &                        nc_srcgrdcorn_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &              'error getting source grid num corner id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_srcgrdcorn_id, 
     &                                len=grid1_corners)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &            'error reading num corners for source grid')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'dst_grid_corners', 
     &                        nc_dstgrdcorn_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid num corner id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_dstgrdcorn_id, 
     &                                len=grid2_corners)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading num corners for destination grid')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'src_grid_rank', 
     &                        nc_srcgrdrank_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid rank id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_srcgrdrank_id, 
     &                                len=grid1_rank)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid rank')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'dst_grid_rank', 
     &                        nc_dstgrdrank_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting destination grid rank id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_dstgrdrank_id, 
     &                                len=grid2_rank)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid rank')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'num_links', 
     &                        nc_numlinks_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting remap size id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_numlinks_id, 
     &                                len=num_links_map1)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading remap size')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'num_wgts', 
     &                        nc_numwgts_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting num weights id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_numwgts_id,
     &                                len=num_wts)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading number of weights')) return

!-----------------------------------------------------------------------
!
!     allocate arrays
!
!-----------------------------------------------------------------------

      allocate( grid1_dims      (grid1_rank),
     &          grid1_center_lat(grid1_size), 
     &          grid1_center_lon(grid1_size),
     &          grid1_area      (grid1_size),
     &          grid1_frac      (grid1_size),
     &          grid1_mask      (grid1_size),
     &          grid1_mask_int  (grid1_size),
     &          grid1_corner_lat(grid1_corners, grid1_size),
     &          grid1_corner_lon(grid1_corners, grid1_size) )

      allocate( grid2_dims      (grid2_rank),
     &          grid2_center_lat(grid2_size), 
     &          grid2_center_lon(grid2_size),
     &          grid2_area      (grid2_size),
     &          grid2_frac      (grid2_size),
     &          grid2_mask      (grid2_size),
     &          grid2_mask_int  (grid2_size),
     &          grid2_corner_lat(grid2_corners, grid2_size),
     &          grid2_corner_lon(grid2_corners, grid2_size) )

      allocate( grid1_add_map1(num_links_map1),
     &          grid2_add_map1(num_links_map1),
     &          wts_map1(num_wts,num_links_map1) )

!-----------------------------------------------------------------------
!
!     get variable ids
!
!-----------------------------------------------------------------------

      ncstat = nf90_inq_varid(nc_file_id, 'src_grid_dims', 
     &                        nc_srcgrddims_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid dims id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'src_grid_imask', 
     &                        nc_srcgrdimask_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid mask id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'src_grid_center_lat', 
     &                        nc_srcgrdcntrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid center lat id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'src_grid_center_lon', 
     &                        nc_srcgrdcntrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid center lon id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'src_grid_corner_lat', 
     &                        nc_srcgrdcrnrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid corner lat id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'src_grid_corner_lon', 
     &                        nc_srcgrdcrnrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid corner lon id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'src_grid_area', 
     &                        nc_srcgrdarea_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid area id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'src_grid_frac', 
     &                        nc_srcgrdfrac_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid frac id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_grid_dims', 
     &                        nc_dstgrddims_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting destination grid dims id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_grid_imask', 
     &                        nc_dstgrdimask_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting destination grid mask id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_grid_center_lat', 
     &                        nc_dstgrdcntrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid center lat id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_grid_center_lon', 
     &                        nc_dstgrdcntrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid center lon id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_grid_corner_lat', 
     &                        nc_dstgrdcrnrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid corner lat id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_grid_corner_lon', 
     &                        nc_dstgrdcrnrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid corner lon id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_grid_area', 
     &                        nc_dstgrdarea_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid area id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_grid_frac', 
     &                        nc_dstgrdfrac_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid frac id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'src_address', 
     &                        nc_srcgrdadd_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting remap source address id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_address', 
     &                        nc_dstgrdadd_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting remap destination address id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'remap_matrix', 
     &                        nc_rmpmatrix_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting remap weights id')) return

!-----------------------------------------------------------------------
!
!     read all variables
!
!-----------------------------------------------------------------------

      ncstat = nf90_get_var(nc_file_id, nc_srcgrddims_id, 
     &                      grid1_dims)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid dims')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdimask_id, 
     &                      grid1_mask_int)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid mask')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdcntrlat_id, 
     &                      grid1_center_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid center lat')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdcntrlon_id, 
     &                      grid1_center_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid center lon')) return

      grid1_units = ' '
      ncstat = nf90_get_att(nc_file_id, nc_srcgrdcntrlat_id, 'units',
     &                      grid1_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid units')) return

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

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdcrnrlat_id, 
     &                      grid1_corner_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid corner lats')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdcrnrlon_id, 
     &                      grid1_corner_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid corner lons')) return

      grid1_units = ' '
      ncstat = nf90_get_att(nc_file_id, nc_srcgrdcrnrlat_id, 'units',
     &                      grid1_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid units')) return

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

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdarea_id, 
     &                      grid1_area)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid area')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdfrac_id, 
     &                      grid1_frac)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid frac')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrddims_id, 
     &                      grid2_dims)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid dims')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdimask_id, 
     &                      grid2_mask_int)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid mask')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdcntrlat_id, 
     &                      grid2_center_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid center lats')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdcntrlon_id, 
     &                      grid2_center_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid center lons')) return

      grid2_units = ' '
      ncstat = nf90_get_att(nc_file_id, nc_dstgrdcntrlat_id, 'units',
     &                      grid2_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid units')) return

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

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdcrnrlat_id, 
     &                      grid2_corner_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid corner lats')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdcrnrlon_id, 
     &                      grid2_corner_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid corner lons')) return

      grid2_units = ' '
      ncstat = nf90_get_att(nc_file_id, nc_dstgrdcrnrlat_id, 'units',
     &                      grid2_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid units')) return

      select case (grid2_units(1:7))
      case ('degrees')
        grid2_corner_lat = grid2_corner_lat*deg2rad
        grid2_corner_lon = grid2_corner_lon*deg2rad
      case ('radians')
        !*** no conversion necessary
      case default
        print *,'unknown units supplied for grid2 corner lat/lon: '
        print *,'proceeding assuming radians'
      end select

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdarea_id, 
     &                      grid2_area)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid area')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdfrac_id, 
     &                      grid2_frac)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid frac')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdadd_id, 
     &                      grid1_add_map1)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source addresses')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdadd_id, 
     &                      grid2_add_map1)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination addresses')) return

      ncstat = nf90_get_var(nc_file_id, nc_rmpmatrix_id, 
     &                      wts_map1)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading weights')) return

!-----------------------------------------------------------------------
!
!     initialize logical mask 
!
!-----------------------------------------------------------------------

      where (grid1_mask_int == 1)
        grid1_mask = .true.
      elsewhere
        grid1_mask = .false.
      endwhere
      where (grid2_mask_int == 1)
        grid2_mask = .true.
      elsewhere
        grid2_mask = .false.
      endwhere
      deallocate(grid1_mask_int, grid2_mask_int)

!-----------------------------------------------------------------------
!
!     close input file
!
!-----------------------------------------------------------------------

      ncstat = nf90_close(nc_file_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error closing remap file')) return

!-----------------------------------------------------------------------

      end subroutine read_remap_scrip

!***********************************************************************

      subroutine read_remap_csm(errorCode)

!-----------------------------------------------------------------------
!
!     the routine reads a netCDF file to extract remapping info
!     in NCAR-CSM format
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4), intent(out) ::
     &   errorCode        ! returned error code

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      character (SCRIP_charLength) ::
     &  grid1_name           ! grid name for source grid
     &, grid2_name           ! grid name for dest   grid

      integer (SCRIP_i4) ::
     &  nc_numwgts1_id    ! extra netCDF id for num_wgts > 1 
     &, nc_rmpmatrix2_id  ! extra netCDF id for high-order remap matrix

      real (SCRIP_r8), dimension(:),allocatable ::
     &  wts1              ! CSM wants single array for 1st-order wts

      real (SCRIP_r8), dimension(:,:),allocatable ::
     &  wts2              ! write remaining weights in different array

      integer (SCRIP_i4), dimension(:), allocatable ::
     &  grid1_mask_int,      ! integer masks to determine
     &  grid2_mask_int       ! cells that participate in map

      character (14), parameter :: 
     &   rtnName = 'read_remap_csm'

!-----------------------------------------------------------------------
!
!     read some additional global attributes
!
!-----------------------------------------------------------------------

      errorCode = SCRIP_Success

      !***
      !*** source and destination grid names
      !***

      grid1_name = ' '
      grid2_name = ' '
      ncstat = nf90_get_att(nc_file_id, NF90_GLOBAL, 'domain_a',
     &                      grid1_name)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid name')) return

      ncstat = nf90_get_att(nc_file_id, NF90_GLOBAL, 'domain_b',
     &                      grid2_name)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid name')) return

      print *,' '
      print *,'Remapping between:',trim(grid1_name)
      print *,'and ',trim(grid2_name)
      print *,' '

!-----------------------------------------------------------------------
!
!     read dimension information
!
!-----------------------------------------------------------------------

      ncstat = nf90_inq_dimid(nc_file_id, 'n_a', nc_srcgrdsize_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid size id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_srcgrdsize_id, 
     &                                len=grid1_size)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid size')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'n_b', nc_dstgrdsize_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting destination grid size id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_dstgrdsize_id, 
     &                                len=grid2_size)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid size')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'nv_a', nc_srcgrdcorn_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting id for source grid num corners')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_srcgrdcorn_id, 
     &                                len=grid1_corners)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading num corners for source grid')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'nv_b', nc_dstgrdcorn_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &   'error getting id for destination grid num corners')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_dstgrdcorn_id, 
     &                                len=grid2_corners)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &   'error reading num corners for destination grid')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'src_grid_rank', 
     &                        nc_srcgrdrank_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid rank id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_srcgrdrank_id, 
     &                                len=grid1_rank)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid rank')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'dst_grid_rank', 
     &                        nc_dstgrdrank_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid rank id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_dstgrdrank_id, 
     &                                len=grid2_rank)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid rank')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'n_s', nc_numlinks_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting remap size id')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_numlinks_id, 
     &                                len=num_links_map1)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading remap size')) return

      ncstat = nf90_inq_dimid(nc_file_id, 'num_wgts', 
     &                        nc_numwgts_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting id for number of weights')) return
      ncstat = nf90_inquire_dimension(nc_file_id, nc_numwgts_id, 
     &                                len=num_wts)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading number of weights')) return

      if (num_wts > 1) then
        ncstat = nf90_inq_dimid(nc_file_id, 'num_wgts1', 
     &                          nc_numwgts1_id)
        if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting num weights id')) return
      endif

!-----------------------------------------------------------------------
!
!     allocate arrays
!
!-----------------------------------------------------------------------

      allocate( grid1_dims      (grid1_rank),
     &          grid1_center_lat(grid1_size), 
     &          grid1_center_lon(grid1_size),
     &          grid1_area      (grid1_size),
     &          grid1_frac      (grid1_size),
     &          grid1_mask      (grid1_size),
     &          grid1_mask_int  (grid1_size),
     &          grid1_corner_lat(grid1_corners, grid1_size),
     &          grid1_corner_lon(grid1_corners, grid1_size) )

      allocate( grid2_dims      (grid2_rank),
     &          grid2_center_lat(grid2_size), 
     &          grid2_center_lon(grid2_size),
     &          grid2_area      (grid2_size),
     &          grid2_frac      (grid2_size),
     &          grid2_mask      (grid2_size),
     &          grid2_mask_int  (grid2_size),
     &          grid2_corner_lat(grid2_corners, grid2_size),
     &          grid2_corner_lon(grid2_corners, grid2_size) )

      allocate( grid1_add_map1(num_links_map1),
     &          grid2_add_map1(num_links_map1),
     &          wts_map1(num_wts,num_links_map1),
     &          wts1(num_links_map1),
     &          wts2(num_wts-1,num_links_map1) )

!-----------------------------------------------------------------------
!
!     get variable ids
!
!-----------------------------------------------------------------------

      ncstat = nf90_inq_varid(nc_file_id, 'src_grid_dims', 
     &                        nc_srcgrddims_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid dims id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'mask_a', 
     &                        nc_srcgrdimask_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid mask id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'yc_a', nc_srcgrdcntrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid center lat id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'xc_a', nc_srcgrdcntrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid center lon id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'yv_a', nc_srcgrdcrnrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid corner lat id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'xv_a', nc_srcgrdcrnrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid corner lon id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'area_a', nc_srcgrdarea_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid area id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'frac_a', nc_srcgrdfrac_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid frac id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'dst_grid_dims', 
     &                      nc_dstgrddims_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting destination grid dims id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'mask_b', 
     &                      nc_dstgrdimask_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting destination grid mask id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'yc_b', nc_dstgrdcntrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid center lat id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'xc_b', nc_dstgrdcntrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid center lon id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'yv_b', nc_dstgrdcrnrlat_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination corner lat id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'xv_b', nc_dstgrdcrnrlon_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination corner lon id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'area_b', nc_dstgrdarea_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting destination grid area id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'frac_b', nc_dstgrdfrac_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting destination grid frac id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'col', nc_srcgrdadd_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting source grid address id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'row', nc_dstgrdadd_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error getting destination grid address id')) return

      ncstat = nf90_inq_varid(nc_file_id, 'S', nc_rmpmatrix_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting remap weights id')) return

      if (num_wts > 1) then
        ncstat = nf90_inq_varid(nc_file_id, 'S2', nc_rmpmatrix2_id)
        if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error getting remap weights id')) return
      endif

!-----------------------------------------------------------------------
!
!     read all variables
!
!-----------------------------------------------------------------------

      ncstat = nf90_get_var(nc_file_id, nc_srcgrddims_id, grid1_dims)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid dims')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdimask_id, 
     &                      grid1_mask_int)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid mask')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdcntrlat_id, 
     &                      grid1_center_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid center lat')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdcntrlon_id, 
     &                      grid1_center_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid center lon')) return

      ncstat = nf90_get_att(nc_file_id, nc_srcgrdcntrlat_id, 'units',
     &                      grid1_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid units')) return

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

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdcrnrlat_id, 
     &                      grid1_corner_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid corner lats')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdcrnrlon_id, 
     &                      grid1_corner_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid corner lons')) return

      ncstat = nf90_get_att(nc_file_id, nc_srcgrdcrnrlat_id, 'units',
     &                      grid1_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid units')) return

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

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdarea_id, 
     &                      grid1_area)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid area')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdfrac_id, 
     &                      grid1_frac)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid frac')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrddims_id, 
     &                      grid2_dims)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid dims')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdimask_id, 
     &                      grid2_mask_int)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid mask')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdcntrlat_id, 
     &                      grid2_center_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid center lats')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdcntrlon_id, 
     &                      grid2_center_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid center lats')) return

      ncstat = nf90_get_att(nc_file_id, nc_dstgrdcntrlat_id, 'units',
     &                      grid2_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid units')) return

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

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdcrnrlat_id, 
     &                      grid2_corner_lat)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid corner lats')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdcrnrlon_id, 
     &                      grid2_corner_lon)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid corner lons')) return


      ncstat = nf90_get_att(nc_file_id, nc_dstgrdcrnrlat_id, 'units',
     &                      grid2_units)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid units')) return

      select case (grid2_units(1:7))
      case ('degrees')
        grid2_corner_lat = grid2_corner_lat*deg2rad
        grid2_corner_lon = grid2_corner_lon*deg2rad
      case ('radians')
        !*** no conversion necessary
      case default
        print *,'unknown units supplied for grid2 corner lat/lon: '
        print *,'proceeding assuming radians'
      end select

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdarea_id, 
     &                      grid2_area)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid area')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdfrac_id, 
     &                      grid2_frac)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading destination grid frac')) return

      ncstat = nf90_get_var(nc_file_id, nc_srcgrdadd_id, 
     &                      grid1_add_map1)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading source grid addresses')) return

      ncstat = nf90_get_var(nc_file_id, nc_dstgrdadd_id, 
     &                      grid2_add_map1)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &         'error reading destination grid addresses')) return

      ncstat = nf90_get_var(nc_file_id, nc_rmpmatrix_id, wts1)
      wts_map1(1,:) = wts1
      deallocate(wts1)

      if (num_wts > 1) then
        ncstat = nf90_get_var(nc_file_id, nc_rmpmatrix2_id, wts2)
        wts_map1(2:,:) = wts2
        deallocate(wts2)
      endif
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error reading remap weights')) return

!-----------------------------------------------------------------------
!
!     initialize logical mask 
!
!-----------------------------------------------------------------------

      where (grid1_mask_int == 1)
        grid1_mask = .true.
      elsewhere
        grid1_mask = .false.
      endwhere
      where (grid2_mask_int == 1)
        grid2_mask = .true.
      elsewhere
        grid2_mask = .false.
      endwhere
      deallocate(grid1_mask_int, grid2_mask_int)

!-----------------------------------------------------------------------
!
!     close input file
!
!-----------------------------------------------------------------------

      ncstat = nf90_close(nc_file_id)
      if (SCRIP_NetcdfErrorCheck(ncstat, errorCode, rtnName,
     &               'error closing remap file')) return

!-----------------------------------------------------------------------

      end subroutine read_remap_csm

!***********************************************************************

      end module remap_read

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
