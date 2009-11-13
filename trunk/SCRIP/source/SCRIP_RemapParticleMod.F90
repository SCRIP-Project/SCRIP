!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module SCRIP_RemapParticleMod

!BOP
! !MODULE: SCRIP_RemapParticleMod
! !DESCRIPTION:
!  This module contains routines for computing a conservative remapping
!  between two grids using a particle method.  In this method, particles
!  are distributed on one grid carrying a fraction of the grid area.
!  The locations of these particles on the second grid are then 
!  determined and the area assigned to the second grid.
! 
! !REVISION HISTORY:
!  SVN:$Id: $
!
! !USES

   use SCRIP_KindsMod ! defines common data types
   use SCRIP_ErrorMod

   use constants    ! defines common constants
   use grids        ! module containing grid information
   use remap_vars   ! module containing remap information

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS

   public :: SCRIP_RemapParticleCreate

!EOP
!BOC
!-----------------------------------------------------------------------
!
!     module variables
!
!-----------------------------------------------------------------------

!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_RemapParticleCreate
! !INTERFACE:

 subroutine SCRIP_RemapParticleCreate(errorCode)

! !DESCRIPTION
!  This routine computes addresses and weights for a conservative
!  remapping between two grids using a particle method.  In this method
!  particles containing a cell area are distributed from one grid and
!  located/accumulated on a second grid.  The area shared between the
!  two is the area of overlap and is used as the remapping weight.
!
! !REVISION HISTORY
!  same as module

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode   ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
! local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4), parameter :: &
      particlesPerCell = 1000 ! num particles to distribute per cell
                              !   for now - this must be a perfect square

   integer (SCRIP_i4) :: &
      i,j,n,             &! dummy loop indices
      nPart,             &! particle index
      fineGrid,          &! id of finer of two grids
      coarseGrid,        &! id of coarser of two grids
      numFineCells,      &! number of cells on fine grid
      numCoarseCells,    &! number of cells on coarse grid
      fineCell,          &! address of fine cell 
      coarseCell,        &! address of fine cell 
      srcAdd,            &! address of source cell 
      dstAdd,            &! address of destination cell 
      nxParticle,        &! number of particles to distribute in x-dir
      nyParticle,        &! number of particles to distribute in y-dir
      numFound,          &! number of particles found on both grids
      minAdd, maxAdd,    &! address range for restricted search
      allocStatus         ! status flag for allocates

   real (SCRIP_r8) ::    &
      dx, dy,            &! spacing of particles
      normFactor,        &! normalization factor
      partArea            ! area assigned to each particle

   real (SCRIP_r8), dimension(:), allocatable :: &
      partLat,           &! latitude  of each distributed particle
      partLon             ! longitude of each distributed particle

   logical (SCRIP_logical), dimension(:), allocatable :: &
      partFound           ! logical flag for particle found on grid

   logical (SCRIP_logical), dimension(:), pointer :: &
      gridMaskFine,      &! mask on fine grid
      gridMaskCoarse      ! mask on coarse grid

   real (SCRIP_r8), dimension(:), pointer :: &
      gridAreaFine,      &! grid cell area for fine grid cells
      gridAreaCoarse,    &! grid cell area for coarse grid cells
      gridFracFine,      &! fractional grid cell area for fine grid
      gridFracCoarse      ! fractional grid cell area for coarse grid

   real (SCRIP_r8), dimension(:,:), pointer :: &
      gridBoundBoxFine,    &! bounding boxes for fine grid cells
      gridBoundBoxCoarse,  &! bounding boxes for fine grid cells
      gridCornerLatFine,   &! corner lats for fine grid cells
      gridCornerLonFine,   &! corner lons for fine grid cells
      gridCornerLatCoarse, &! corner lats for coarse grid cells
      gridCornerLonCoarse   ! corner lons for coarse grid cells

   real (SCRIP_r8), dimension(2) :: &
      weights               ! local wgt array

   logical (SCRIP_logical) :: &
      boundBoxOverlap       ! true if two bounding boxes overlap
 
   character (25), parameter :: &
      rtnName = 'SCRIP_RemapParticleCreate'

!-----------------------------------------------------------------------
!
!  particles should be distributed on finer grid, so determine which
!  grid is the fine grid
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

   if (grid1_size >= grid2_size) then
      fineGrid = 1
      coarseGrid = 2
      numFineCells = grid1_size
      numCoarseCells = grid2_size
      gridMaskFine => grid1_mask
      gridMaskCoarse => grid2_mask
      gridAreaFine => grid1_area_in
      gridAreaCoarse => grid2_area_in
      gridFracFine => grid1_frac
      gridFracCoarse => grid2_frac
      gridCornerLatFine => grid1_corner_lat
      gridCornerLonFine => grid1_corner_lon
      gridCornerLatCoarse => grid2_corner_lat
      gridCornerLonCoarse => grid2_corner_lon
      gridBoundBoxFine => grid1_bound_box
      gridBoundBoxCoarse => grid2_bound_box
   else
      fineGrid = 2
      coarseGrid = 1
      numFineCells = grid2_size
      numCoarseCells = grid1_size
      gridMaskFine => grid2_mask
      gridMaskCoarse => grid1_mask
      gridAreaFine => grid2_area_in
      gridAreaCoarse => grid1_area_in
      gridFracFine => grid2_frac
      gridFracCoarse => grid1_frac
      gridCornerLatFine => grid2_corner_lat
      gridCornerLonFine => grid2_corner_lon
      gridCornerLatCoarse => grid1_corner_lat
      gridCornerLonCoarse => grid1_corner_lon
      gridBoundBoxFine => grid2_bound_box
      gridBoundBoxCoarse => grid1_bound_box
   endif

   grid1_area = grid1_area_in
   grid2_area = grid2_area_in

!-----------------------------------------------------------------------
!
!  for each cell on fine grid, distribute particles
!
!-----------------------------------------------------------------------

   allocate(partLat  (particlesPerCell), &
            partLon  (particlesPerCell), &
            partFound(particlesPerCell), &
            stat = allocStatus)

   if (allocStatus > 0) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'error allocating particle arrays')
      return
   endif

   nxParticle = nint(sqrt(real(particlesPerCell)))
   nyParticle = nint(sqrt(real(particlesPerCell)))

   gridFracFine   = 0.0_SCRIP_r8
   gridFracCoarse = 0.0_SCRIP_r8

   do fineCell = 1,numFineCells
   if (gridMaskFine(fineCell)) then  ! do only if mask is true

      if (fineGrid == 1) then
         srcAdd = fineCell
      else
         dstAdd = fineCell
      endif

!-----------------------------------------------------------------------
!
!     distribute particles based on bounding box
!
!-----------------------------------------------------------------------

      dy = (gridBoundBoxFine(2,fineCell)-gridBoundBoxFine(1,fineCell))/ &
           (nyParticle+1)
      dx = (gridBoundBoxFine(4,fineCell)-gridBoundBoxFine(3,fineCell))/ &
           (nxParticle+1)

      nPart = 0
      do j=1,nyParticle
      do i=1,nxParticle

         nPart = nPart + 1
         partLon(nPart) = gridBoundBoxFine(3,fineCell) + i*dx
         partLat(nPart) = gridBoundBoxFine(1,fineCell) + j*dy
         partFound(nPart) = .false.

      enddo
      enddo

!-----------------------------------------------------------------------
!
!     keep only those particles that are in fine grid cell and assign
!     area to particles based on fine cell area
!
!-----------------------------------------------------------------------
  
      do nPart = 1,particlesPerCell
         partFound(nPart) = SCRIP_RemapParticleInCell(          &
                               partLat(nPart), partLon(nPart),  &
                               gridCornerLatFine(:,fineCell),   &
                               gridCornerLonFine(:,fineCell),   &
                               errorCode)
      end do

      if (SCRIP_ErrorCheck(errorCode, rtnName, &
                           'error finding part in fine grid')) return

      !***
      !*** determine number of particles in play and assigne equal
      !*** areas to each
      !***

      numFound = count(partFound)

      if (numFound == 0) then
         call SCRIP_ErrorSet(errorCode, rtnName, &
                             'no particles located on fine grid')
         return
      endif

      partArea = gridAreaFine(fineCell)/numFound
      weights(:) = partArea

      !***
      !*** collapse list of particles to only those found
      !***

      do nPart = 1,numFound

         !*** find next found and compress list

         nextFound: do n=nPart,particlesPerCell
            if (partFound(n)) then
               partFound(n) = .false. ! remove from list
               if (nPart < n) then  ! only do copy if necessary
                  partLat(nPart) = partLat(n)
                  partLon(nPart) = partLon(n)
               endif
               exit nextFound
            endif
         end do nextFound
      end do

!-----------------------------------------------------------------------
!
!     locate particle on coarse grid cell and when found, store that
!     particle area as a remapping link
!
!-----------------------------------------------------------------------

      !***
      !*** restrict searches first using search bins
      !***

      minAdd = numCoarseCells 
      maxAdd = 1
      do n=1,num_srch_bins
         if (fineGrid == 1) then
            if (fineCell >= bin_addr1(1,n) .and. &
                fineCell <= bin_addr1(2,n)) then
               minAdd = min(minAdd, bin_addr2(1,n))
               maxAdd = max(maxAdd, bin_addr2(2,n))
            endif
         else
            if (fineCell >= bin_addr2(1,n) .and. &
                fineCell <= bin_addr2(2,n)) then
               minAdd = min(minAdd, bin_addr1(1,n))
               maxAdd = max(maxAdd, bin_addr1(2,n))
            endif
         endif
      end do

      !***
      !*** further restrict searches using bounding boxes
      !***

      do coarseCell = minAdd,maxAdd

         if (coarseGrid == 1) then
            srcAdd = coarseCell
         else
            dstAdd = coarseCell
         endif

         boundBoxOverlap = (gridBoundBoxCoarse(1,coarseCell) <=   &
                            gridBoundBoxFine  (2,fineCell)) .and. &
                           (gridBoundBoxCoarse(2,coarseCell) >=   &
                            gridBoundBoxFine  (1,fineCell)) .and. &
                           (gridBoundBoxCoarse(3,coarseCell) <=   &
                            gridBoundBoxFine  (4,fineCell)) .and. &
                           (gridBoundBoxCoarse(4,coarseCell) >=   &
                            gridBoundBoxFine  (3,fineCell))

         !***
         !*** if the bounding boxes overlap, check this cell
         !***

         if (boundBoxOverlap) then

            !*** check particles that have not already been found

            do nPart = 1,numFound
            if (.not. partFound(nPart)) then
               partFound(nPart) = SCRIP_RemapParticleInCell(         &
                                  partLat(nPart), partLon(nPart),    &
                                  gridCornerLatCoarse(:,coarseCell), &
                                  gridCornerLonCoarse(:,coarseCell), &
                                  errorCode)

               if (SCRIP_ErrorCheck(errorCode, rtnName, &
                                    'error in particle find')) return

               if (partFound(nPart)) then

                  call SCRIP_RemapParticleStoreLink(srcAdd, dstAdd,  &
                                                    weights, errorCode)

                  if (SCRIP_ErrorCheck(errorCode, rtnName, &
                                       'error storing link')) return

                  gridFracFine  (  fineCell) = gridFracFine  (  fineCell) + &
                                               partArea
                  gridFracCoarse(coarseCell) = gridFracCoarse(coarseCell) + &
                                               partArea

               endif
            endif
            end do


         endif ! bounding boxes overlap

      end do ! search over coarse cells

!-----------------------------------------------------------------------
!
!  finished with this cell on fine grid
!
!-----------------------------------------------------------------------

   endif ! fine cell mask
   end do ! loop over fine grid cells

   deallocate(partLat, partLon, partFound, &
              stat = allocStatus)

   if (allocStatus > 0) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'error deallocating particle arrays')
      return
   endif

!-----------------------------------------------------------------------
!
!  renormalize weights based on input choice for normalization
!
!-----------------------------------------------------------------------

   do n=1,num_links_map1

      srcAdd = grid1_add_map1(n)
      dstAdd = grid2_add_map1(n)

      select case (norm_opt)

      case(norm_opt_dstarea)
         if (fineGrid == 1) then
            normFactor = 1.0_SCRIP_r8/gridAreaCoarse(dstAdd)
         else
            normFactor = 1.0_SCRIP_r8/gridAreaFine(dstAdd)
         endif
      case(norm_opt_frcarea)
         if (fineGrid == 1) then
            normFactor = 1.0_SCRIP_r8/gridFracCoarse(dstAdd)
         else
            normFactor = 1.0_SCRIP_r8/gridFracFine(dstAdd)
         endif
      case(norm_opt_none)
         normFactor = 1.0_SCRIP_r8
      end select

      wts_map1(1,n) = wts_map1(1,n)*normFactor

      if (wts_map1(1,n) > 1.5 .or. wts_map1(1,n) < 0.0) then
         print *, 'weight out of range map1: ',n, srcAdd, dstAdd, &
                                               wts_map1(1,n)
      endif

      if (num_maps > 1) then
         srcAdd = grid1_add_map2(n)
         dstAdd = grid2_add_map2(n)

         select case (norm_opt)

         case(norm_opt_dstarea)
            if (fineGrid == 1) then
               normFactor = 1.0_SCRIP_r8/gridAreaFine(dstAdd)
            else
               normFactor = 1.0_SCRIP_r8/gridAreaCoarse(dstAdd)
            endif
         case(norm_opt_frcarea)
            if (fineGrid == 1) then
               normFactor = 1.0_SCRIP_r8/gridFracFine(dstAdd)
            else
               normFactor = 1.0_SCRIP_r8/gridFracCoarse(dstAdd)
            endif
         case(norm_opt_none)
            normFactor = 1.0_SCRIP_r8
         end select

         wts_map2(1,n) = wts_map2(1,n)*normFactor

         if (wts_map2(1,n) > 1.5 .or. wts_map2(1,n) < 0.0) then
            print *, 'weight out of range map2: ',n, srcAdd, dstAdd, &
                                                  wts_map2(1,n)
         endif

      endif
   end do

   where (grid1_area_in /= 0.0_SCRIP_r8) 
      grid1_frac = grid1_frac/grid1_area_in
   elsewhere
      grid1_frac = 0.0_SCRIP_r8
   end where

   where (grid2_area_in /= 0.0_SCRIP_r8) 
      grid2_frac = grid2_frac/grid2_area_in
   elsewhere
      grid2_frac = 0.0_SCRIP_r8
   end where

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_RemapParticleCreate

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_RemapParticleInCell
! !INTERFACE:

 function SCRIP_RemapParticleInCell(partLat, partLon,            &
                                   cellLat, cellLon, errorCode) &
                                   result(inCell)

! !DESCRIPTION
!  This function determines whether a particle resides within a
!  cell.  It uses a cross-product test to determine whether the
!  particle is within a cell.  Note that this test is known to
!  fail for non-convex cells.
!
! !REVISION HISTORY
!  same as module

! !OUTPUT PARAMETERS:

   logical (SCRIP_logical) :: &
      inCell      ! true if particle is located in cell

   integer (SCRIP_i4), intent(out) :: &
      errorCode   ! returned error code

! !INPUT PARAMETERS:

   real (SCRIP_r8), intent(in) :: &
      partLat, partLon   ! lat,lon location of particle

   real (SCRIP_r8), dimension(:), intent(in) :: &
      cellLat, cellLon   ! lat,lon location of cell corners

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
      numCorners,        &! num corners in grid cell
      corner,            &! corner index
      nextCorner          ! index for next corner in cell

   real (SCRIP_r8) ::    &
      vec1Lat, vec1Lon,  &! vectors and cross products used
      vec2Lat, vec2Lon,  &! during grid search
      crossProduct

   character (25), parameter :: &
      rtnName = 'SCRIP_RemapParticleInCell'

!-----------------------------------------------------------------------
!
! initialize defaults
!
!-----------------------------------------------------------------------

  errorCode = SCRIP_Success
  inCell = .false.
  numCorners = size(cellLat)
  
!-----------------------------------------------------------------------
!
!  perform cross product test to determine whether a point is enclosed 
!  by a cell
!
!-----------------------------------------------------------------------

   cornerLoop: do corner = 1,numCorners
      nextCorner = MOD(corner,numCorners) + 1

      !***
      !*** here we take the cross product of the vector making 
      !*** up each cell side with the vector formed by the vertex
      !*** and search point.  if all the cross products are 
      !*** positive, the point is contained in the cell.
      !***

      vec1Lat = cellLat(nextCorner) - cellLat(corner)
      vec1Lon = cellLon(nextCorner) - cellLon(corner)
      vec2Lat = partLat - cellLat(corner)
      vec2Lon = partLon - cellLon(corner)

      !***
      !*** if endpoint coincident with vertex, particle is
      !*** in the cell, so set flag and exit
      !***

      if (vec2Lat == 0 .and. vec2Lon == 0) then
         inCell = .true.
         exit cornerLoop
      endif

      !***
      !*** check for 0,2pi crossings
      !***

      if (vec1Lon >  pi) then
        vec1Lon = vec1Lon - pi2
      else if (vec1Lon < -pi) then
        vec1Lon = vec1Lon + pi2
      endif
      if (vec2Lon >  pi) then
        vec2Lon = vec2Lon - pi2
      else if (vec2Lon < -pi) then
        vec2Lon = vec2Lon + pi2
      endif

      crossProduct = vec1Lon*vec2Lat - vec2Lon*vec1Lat

      !***
      !*** if cross product is less than zero, this cell
      !*** doesn't work
      !***

      if (crossProduct < 0.0_SCRIP_r8) exit cornerLoop

   end do cornerLoop

   !***
   !*** if cross products all positive, we found the location
   !***

   if (corner > numCorners) inCell = .true.

!-----------------------------------------------------------------------
!EOC

 end function SCRIP_RemapParticleInCell

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_RemapParticleStoreLink
! !INTERFACE:

 subroutine SCRIP_RemapParticleStoreLink(srcAdd, dstAdd, weights, &
                                         errorCode)

! !DESCRIPTION
!  This routine stores the addresses and weights for this link in
!  the appropriate address and weight arrays and resizes those
!  arrays if necessary.
!
! !REVISION HISTORY
!  same as module

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode   ! returned error code

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcAdd, dstAdd     ! source and destination addresses for
                         ! this link

   real (SCRIP_r8), dimension(:), intent(in) :: &
      weights            ! weights for this link

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
      nlink,             &! link index
      allocStatus,       &! status flag for allocate
      minLink, maxLink    ! restricted search for existing links

   integer (SCRIP_i4), dimension(:,:), allocatable, save :: &
      linkAdd1,          &! min,max link add to restrict search
      linkAdd2            ! min,max link add to restrict search

   logical (SCRIP_logical), save :: &
      firstCall = .true.  ! flag to determine first call to this routine
                          ! for allocation purposes

   character (28), parameter :: &
      rtnName = 'SCRIP_RemapParticleStoreLink'

!-----------------------------------------------------------------------
!
!  if all weights are zero, do not bother storing the link
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

   if (all(weights == 0.0_SCRIP_r8)) return

!-----------------------------------------------------------------------
!
!  restrict the range of links to search for existing links
!
!-----------------------------------------------------------------------

   if (firstCall) then
      allocate(linkAdd1(2,grid1_size), &
               linkAdd2(2,grid2_size), stat=allocStatus)

      if (allocStatus > 0) then
         call SCRIP_ErrorSet(errorCode, rtnName, &
                             'error allocating address arrays')
         return
      endif

      linkAdd1 = 0
      linkAdd2 = 0
      minLink  = 1
      maxLink  = 0
      firstCall = .false.
   else
      minLink = min(linkAdd1(1,srcAdd),linkAdd2(1,dstAdd))
      maxLink = max(linkAdd1(2,srcAdd),linkAdd2(2,dstAdd))
      if (minLink == 0) then
         minLink = 1
         maxLink = 0
      endif
   endif

!-----------------------------------------------------------------------
!
!  if the link already exists, add the weight to the current weight
!  arrays
!
!-----------------------------------------------------------------------

   do nlink=minLink,maxLink
      if (srcAdd == grid1_add_map1(nlink)) then
      if (dstAdd == grid2_add_map1(nlink)) then

         wts_map1(:,nlink) = wts_map1(:,nlink) + weights(1:num_wts)
         if (num_maps == 2) then
            wts_map2(:,nlink) = wts_map2(:,nlink) + & 
                                weights(num_wts+1:2*num_wts)
         endif
         return

      endif
      endif
   end do

!-----------------------------------------------------------------------
!
!  if the link does not yet exist, increment number of links and 
!  check to see if remap arrays need to be increased to accomodate 
!  the new link.  then store the link.
!
!-----------------------------------------------------------------------

   num_links_map1  = num_links_map1 + 1
   if (num_links_map1 > max_links_map1) & 
      call resize_remap_vars(1,resize_increment)

   grid1_add_map1(num_links_map1) = srcAdd
   grid2_add_map1(num_links_map1) = dstAdd
   wts_map1    (:,num_links_map1) = weights(1:num_wts)

   if (num_maps > 1) then
      num_links_map2  = num_links_map2 + 1
      if (num_links_map2 > max_links_map2) & 
         call resize_remap_vars(2,resize_increment)

      grid1_add_map2(num_links_map2) = dstAdd
      grid2_add_map2(num_links_map2) = srcAdd
      wts_map2    (:,num_links_map2) = weights(num_wts+1:2*num_wts)
   endif

   if (linkAdd1(1,srcAdd) == 0) linkAdd1(1,srcAdd) = num_links_map1
   if (linkAdd2(1,dstAdd) == 0) linkAdd2(1,dstAdd) = num_links_map1
   linkAdd1(2,srcAdd) = num_links_map1
   linkAdd2(2,dstAdd) = num_links_map1

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_RemapParticleStoreLink

!***********************************************************************

 end module SCRIP_RemapParticleMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
