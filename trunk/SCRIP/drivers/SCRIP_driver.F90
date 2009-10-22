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
   if (SCRIP_errorCheck(errorCode,'error initializing SCRIP')) then
      call SCRIP_errorPrint(errorCode, SCRIP_masterTask)
      call SCRIP_CommExitMessageEnvironment
      stop
   endif

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
   if (SCRIP_ErrorCheck(errorCode, 'error opening config file')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs',                         &
                         'gridFile1', gridFile1, 'unknown', errorCode, & 
                         outStringBefore= 'grid1 contained in file: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading grid1 filename')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs',                         &
                         'gridFile2', gridFile2, 'unknown', errorCode, & 
                         outStringBefore= 'grid2 contained in file: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading grid2 filename')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'interpFile1',          &
                         interpFile1, 'unknown', errorCode,            & 
           outStringBefore= 'grid1 to grid2 regrid contained in file: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading interp1 filename')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'interpFile2',          &
                         interpFile2, 'unknown', errorCode,            & 
           outStringBefore= 'grid2 to grid1 regrid contained in file: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading interp2 filename')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs',                       &
                         'mapName1', mapName1, 'unknown', errorCode, & 
                         outStringBefore= 'map1 will be named: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading map1 name')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs',                       &
                         'mapName2', mapName2, 'unknown', errorCode, &
                         outStringBefore= 'map2 will be named: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading map2 name')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs',                     &
                         'num_maps', num_maps, 1, errorCode, & 
                         outStringBefore= 'Computing ',            &
                         outStringAfter= ' remappings')
   if (SCRIP_ErrorCheck(errorCode, 'error reading num_maps')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'luse_grid1_area', &
                         luse_grid1_area, .false., errorCode,     & 
                         outStringBefore= 'Use grid1 area: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading luse_grid1_area')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'luse_grid2_area', &
                         luse_grid2_area, .false., errorCode,     & 
                         outStringBefore= 'Use grid2 area: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading luse_grid2_area')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'mapMethod',    &
                         mapMethod, 'conservative', errorCode, & 
                         outStringBefore= 'Map method is ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading mapMethod')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'normalizeOpt', &
                         normalizeOpt, 'fracArea', errorCode, & 
                         outStringBefore= 'Normalization option is: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading normalizeOpt')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'outputFormat', &
                         outputFormat, 'scrip', errorCode,     & 
                         outStringBefore= 'Output format is: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading outputFormat')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'restrict_type', &
                         restrict_type, 'latitude', errorCode,  & 
                         outStringBefore= 'Restricting search by: ')
   if (SCRIP_ErrorCheck(errorCode, 'error reading restrict_type')) stop

   call SCRIP_ConfigRead(iunit, 'remapInputs', 'num_srch_bins', &
                         num_srch_bins, 900, errorCode,         & 
                         outStringBefore= 'Using ',                 &
                         outStringAfter = 'search bins')
   if (SCRIP_ErrorCheck(errorCode, 'error reading num_srch_bins')) stop

   call SCRIP_ConfigClose(iunit, errorCode)
   if (SCRIP_ErrorCheck(errorCode, 'error closing config file')) stop

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
      stop 'unknown mapping method'
   end select

   select case(trim(normalizeOpt))
   case ('none')
      norm_opt = norm_opt_none
   case ('fracArea')
      norm_opt = norm_opt_frcarea
   case ('destArea')
      norm_opt = norm_opt_dstarea
   case default
      stop 'unknown normalization option'
   end select

!-----------------------------------------------------------------------
!
!  initialize grid information for both grids
!
!-----------------------------------------------------------------------

   call grid_init(gridFile1, gridFile2, errorCode)

   if (errorCode /= SCRIP_Success) then
      call SCRIP_ErrorSet(errorCode, &
                          'SCRIP_driver: Error initializing grids')
      call SCRIP_errorPrint(errorCode, SCRIP_masterTask)
      call SCRIP_CommExitMessageEnvironment
      stop
   endif

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
      stop 'Invalid Map Type'
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
                    interpFile1, interpFile2, outputFormat)

!-----------------------------------------------------------------------
!
!  All done, exit gracefully
!
!-----------------------------------------------------------------------

   call SCRIP_errorPrint(errorCode, SCRIP_masterTask)
   call SCRIP_CommExitMessageEnvironment

!-----------------------------------------------------------------------
!EOC

 end program SCRIP_driver

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
