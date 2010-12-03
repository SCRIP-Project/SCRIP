!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 program SCRIP_driver

!BOP
! !PROGRAM: SCRIP_driver
!
! !DESCRIPTION:
!  This program is a standalone driver for computing SCRIP weights
!  that are written to a file for later use.  It reads in inputs
!  and then calls the appropriate SCRIP routines for the desired
!  weights.  The program reads options from a file named 
!  {\em scrip\_in} with a single namelist-like group in the format
!  below and the inputs shown:
!  \begin{verbatim}
!  &remapInputs
!     gridFile1 = 'filename'   (input file for first grid)
!     gridFile2 = 'filename'   (input file for second grid)
!     interpFile1 = 'filename' (output file for grid1->grid2 map)
!     interpFile2 = 'filename' (output file for grid2->grid1 map)
!     mapName1    = 'name'     (name for grid1->grid2 map)
!     mapName2    = 'name'     (name for grid2->grid1 map)
!     num_maps     = 1 or 2     (1 for a single grid1->grid2 map,
!                               2 for computing map for both directions)
!     luse_grid1_area = .true./.false. (true to use input grid area 
!     luse_grid2_area = .true./.false.  in place of SCRIP-computed area)
!     mapMethod   = 'method'   (regrid method: conservative, bilinear, 
!                               bicubic or nearNbr)
!     normalizeOpt = 'option'  (option for weight normalization:
!                               dstArea, fracArea)
!     outputFormat = 'format'  (output format - scrip or ccsm)
!     restrict_type = 'type' (choice for restricting grid searches:
!                                   latitude, longitude, latlon)
!     num_srch_bins = [1-n] (integer number of search bins to divide
!                                domain and speed grid searches)
!     num_polar_segs = 11 (Number of segments each lat-lon line must be
!                          subdivided into to get an accurate representation
!                          of the equivalent curve in polar space
!     npole_threshold = 1.5 (Latitude above which line segments are transformed
!			    to Lambert Space in the northern hemisphere)
!     spole_threshold = -1.5 (Latitude below with line segments are transformed
!			    to Lambert space in the southern hemisphere
!     num_threads     = 2 (Number of simultaneous threads to run program with)
!  /
!  \end{verbatim}
!
! !REVISION HISTORY:
!  SVN:$Id: $

! !USES:

   use SCRIP_KindsMod             ! module defining data types
   use SCRIP_CommMod              ! for initializing comm environment
   use SCRIP_ErrorMod             ! SCRIP error checking and logging
   use SCRIP_IOUnitsMod           ! manages I/O units
   use SCRIP_ConfigMod            ! SCRIP configuration module
   use SCRIP_InitMod              ! SCRIP initialization
   use constants                  ! module for common constants
   use timers                     ! CPU timers
   use grids                      ! module with grid information
   use remap_vars                 ! common remapping variables
   use remap_conservative         ! routines for conservative remap
   use remap_distance_weight      ! routines for dist-weight remap
   use remap_bilinear             ! routines for bilinear interp
   use remap_bicubic              ! routines for bicubic  interp
   use SCRIP_RemapParticleMod     ! routines for particle remap
   use remap_write                ! routines for remap output

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

   character (12), parameter :: &
      rtnName = 'SCRIP_driver'

!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
      n,            &! dummy counter
      iunit          ! unit number for input configuration file

!-----------------------------------------------------------------------
!
!  initialize communication environment and SCRIP package
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

   call SCRIP_CommInitMessageEnvironment
   call SCRIP_Initialize(errorCode)
   if (SCRIP_errorCheck(errorCode, rtnName, 'error initializing SCRIP')) &
      call SCRIP_driverExit(errorCode)

!-----------------------------------------------------------------------
!
!  initialize timers
!
!-----------------------------------------------------------------------

   call timers_init
   do n=1,max_timers
      call timer_clear(n)
   end do

!-----------------------------------------------------------------------
!
!  read input namelist
!
!-----------------------------------------------------------------------

   call SCRIP_ConfigOpen(iunit, errorCode)
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error opening config file')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs',                         &
                         'gridFile1', gridFile1, 'unknown', errorCode, & 
                         outStringBefore= 'grid1 contained in file: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading grid1 filename')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs',                         &
                         'gridFile2', gridFile2, 'unknown', errorCode, & 
                         outStringBefore= 'grid2 contained in file: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading grid2 filename')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'interpFile1',          &
                         interpFile1, 'unknown', errorCode,            & 
           outStringBefore= 'grid1 to grid2 regrid contained in file: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading interp1 filename')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'interpFile2',          &
                         interpFile2, 'unknown', errorCode,            & 
           outStringBefore= 'grid2 to grid1 regrid contained in file: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading interp2 filename')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs',                       &
                         'mapName1', mapName1, 'unknown', errorCode, & 
                         outStringBefore= 'map1 will be named: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading map1 name')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs',                       &
                         'mapName2', mapName2, 'unknown', errorCode, &
                         outStringBefore= 'map2 will be named: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading map2 name')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs',                     &
                         'num_maps', num_maps, 1, errorCode, & 
                         outStringBefore= 'Computing ',            &
                         outStringAfter= ' remappings')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading num_maps')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'luse_grid1_area', &
                         luse_grid1_area, .false., errorCode,     & 
                         outStringBefore= 'Use grid1 area: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading luse_grid1_area')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'luse_grid2_area', &
                         luse_grid2_area, .false., errorCode,     & 
                         outStringBefore= 'Use grid2 area: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading luse_grid2_area')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'mapMethod',    &
                         mapMethod, 'conservative', errorCode, & 
                         outStringBefore= 'Map method is ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading mapMethod')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'normalizeOpt', &
                         normalizeOpt, 'fracArea', errorCode, & 
                         outStringBefore= 'Normalization option is: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading normalizeOpt')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'outputFormat', &
                         outputFormat, 'scrip', errorCode,     & 
                         outStringBefore= 'Output format is: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading outputFormat')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'restrict_type', &
                         restrict_type, 'latitude', errorCode,  & 
                         outStringBefore= 'Restricting search by: ')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading restrict_type')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'num_srch_bins', &
                         num_srch_bins, 900, errorCode,         & 
                         outStringBefore= 'Using ',                 &
                         outStringAfter = ' search bins')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading num_srch_bins')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs',                     &
                         'num_polar_segs', npseg, 11, errorCode,   & 
                         outStringBefore= 'Using ',            &
                         outStringAfter= ' segments per polar edge')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading num_polar_segs')) call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs',                     &
                         'npole_threshold', north_thresh, &
                         1.5_SCRIP_r8,     &
			 errorCode, & 
                         outStringBefore='North pole threshold is ',&
                         outStringAfter= ' radians')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading npole_threshold')) &
           call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs',                     &
                         'spole_threshold', south_thresh, &
                         -2.0_SCRIP_r8,    &
			 errorCode, & 
                         outStringBefore='South pole threshold is ',&
                         outStringAfter= ' radians')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading spole_threshold')) &
           call SCRIP_driverExit(errorCode)

   call SCRIP_ConfigRead(iunit, 'remapInputs',                     &
                         'num_threads', nthreads, &
                         2, &
			 errorCode, & 
                         outStringBefore='Number of threads ',&
                         outStringAfter= '')
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error reading number of threads')) &
           call SCRIP_driverExit(errorCode)


   call SCRIP_ConfigClose(iunit, errorCode)
   if (SCRIP_ErrorCheck(errorCode, rtnName, &
       'error closing config file')) call SCRIP_driverExit(errorCode)

   select case(mapMethod)
   case ('conservative')
      map_type = map_type_conserv
      luse_grid_centers = .false.
   case ('bilinear')
      map_type = map_type_bilinear
      luse_grid_centers = .true.
   case ('bicubic')
      map_type = map_type_bicubic
      luse_grid_centers = .true.
   case ('distwgt')
      map_type = map_type_distwgt
      luse_grid_centers = .true.
   case ('particle')
      map_type = map_type_particle
      luse_grid_centers = .false.
   case default
      call SCRIP_ErrorSet(errorCode, rtnName, 'unknown mapping method')
      call SCRIP_driverExit(errorCode)
   end select

   select case(trim(normalizeOpt))
   case ('none')
      norm_opt = norm_opt_none
   case ('fracArea')
      norm_opt = norm_opt_frcarea
   case ('destArea')
      norm_opt = norm_opt_dstarea
   case default
      call SCRIP_ErrorSet(errorCode, rtnName, 'unknown normalization option')
      call SCRIP_driverExit(errorCode)
   end select

!-----------------------------------------------------------------------
!
!  initialize grid information for both grids
!
!-----------------------------------------------------------------------

   call grid_init(gridFile1, gridFile2, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, 'Error initializing grids')) &
      call SCRIP_driverExit(errorCode)

   write(SCRIP_stdout, *) 'Computing remappings between: ',grid1_name
   write(SCRIP_stdout, *) '                         and  ',grid2_name

!-----------------------------------------------------------------------
!
!  initialize some remapping variables.
!
!-----------------------------------------------------------------------

   call init_remap_vars

!-----------------------------------------------------------------------
!
!  call appropriate interpolation setup routine based on type of
!  remapping requested.
!
!-----------------------------------------------------------------------

   select case(map_type)
   case(map_type_conserv)
      call remap_conserv
   case(map_type_bilinear)
      call remap_bilin
   case(map_type_distwgt)
      call remap_distwgt
   case(map_type_bicubic)
      call remap_bicub
   case(map_type_particle)
      call SCRIP_RemapParticleCreate(errorCode)
   case default
      call SCRIP_ErrorSet(errorCode, rtnName, 'Invalid Map Type')
      call SCRIP_driverExit(errorCode)
   end select

!-----------------------------------------------------------------------
!
!  reduce size of remapping arrays and write remapping info to file.
!
!-----------------------------------------------------------------------

   if (num_links_map1 /= max_links_map1) then
        call resize_remap_vars(1, num_links_map1-max_links_map1)
   endif
   if ((num_maps > 1) .and. (num_links_map2 /= max_links_map2)) then
        call resize_remap_vars(2, num_links_map2-max_links_map2)
   endif

   call write_remap(mapName1, mapName2, & 
                    interpFile1, interpFile2, outputFormat, errorCode)
   if (SCRIP_ErrorCheck(errorCode, rtnName, 'error in write_remap')) &
       call SCRIP_driverExit(errorCode)

!-----------------------------------------------------------------------
!
!  All done, exit gracefully
!
!-----------------------------------------------------------------------

   call SCRIP_driverExit(errorCode)

!-----------------------------------------------------------------------
!EOC

 end program SCRIP_driver

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_driverExit
! !INTERFACE:

   subroutine SCRIP_driverExit(errorCode)

! !DESCRIPTION:
!  This routine exits the SCRIP driver program. It first calls the 
!  SCRIP error print function to print any errors encountered and then
!  exits the message environment before stopping.
!
! !REVISION HISTORY:
!  SVN:$Id: $

! !USES:

   use SCRIP_KindsMod
   use SCRIP_CommMod
   use SCRIP_ErrorMod

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      errorCode        ! error flag to detect any errors encountered

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  call SCRIP error print function to output any logged errors that
!  were encountered during execution.  Then stop.
!
!-----------------------------------------------------------------------

   call SCRIP_errorPrint(errorCode, SCRIP_masterTask)
   call SCRIP_CommExitMessageEnvironment

   stop

!-----------------------------------------------------------------------
!EOC

   end subroutine SCRIP_driverExit

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
