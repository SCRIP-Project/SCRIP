!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module SCRIP_BroadcastMod

!BOP
! !MODULE: SCRIP_BroadcastMod
! !DESCRIPTION:
!  This module contains routines for broadcasting scalar and array
!  data from one processor or task to the rest of the tasks. This
!  particular version contains MPI implementations of these routines.
!
! !REVISION HISTORY:
!  SVN:$Id: $
!
! !USES:

   use SCRIP_KindsMod
   use SCRIP_CommMod
   use SCRIP_ErrorMod

   implicit none
   private
   save

   include 'mpif.h'

! !PUBLIC MEMBER FUNCTIONS:

   public  :: SCRIP_Broadcast

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  generic interfaces for module procedures
!
!-----------------------------------------------------------------------

   interface SCRIP_Broadcast
     module procedure SCRIP_BroadcastScalarR8,    &
                      SCRIP_BroadcastScalarR4,    &
                      SCRIP_BroadcastScalarI4,    &
                      SCRIP_BroadcastScalarLog,   &
                      SCRIP_BroadcastScalarChar,  &
                      SCRIP_BroadcastArrayR8D1,   &
                      SCRIP_BroadcastArrayR4D1,   &
                      SCRIP_BroadcastArrayI4D1,   &
                      SCRIP_BroadcastArrayLogD1,  &
                      SCRIP_BroadcastArrayCharD1, &
                      SCRIP_BroadcastArrayR8D2,   &
                      SCRIP_BroadcastArrayR4D2,   &
                      SCRIP_BroadcastArrayI4D2,   &
                      SCRIP_BroadcastArrayLogD2,  &
                      SCRIP_BroadcastArrayR8D3,   &
                      SCRIP_BroadcastArrayR4D3,   &
                      SCRIP_BroadcastArrayI4D3,   &
                      SCRIP_BroadcastArrayLogD3,  &
                      SCRIP_BroadcastArrayR8D4,   &
                      SCRIP_BroadcastArrayR4D4,   &
                      SCRIP_BroadcastArrayI4D4,   &
                      SCRIP_BroadcastArrayLogD4,  &
                      SCRIP_BroadcastArrayR8D5,   &
                      SCRIP_BroadcastArrayR4D5,   &
                      SCRIP_BroadcastArrayI4D5,   &
                      SCRIP_BroadcastArrayLogD5,  &
                      SCRIP_BroadcastArrayR8D6,   &
                      SCRIP_BroadcastArrayR4D6,   &
                      SCRIP_BroadcastArrayI4D6,   &
                      SCRIP_BroadcastArrayLogD6
   end interface

!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastScalarR8
! !INTERFACE:

 subroutine SCRIP_BroadcastScalarR8(scalar, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a scalar real8 variable from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), intent(inout) :: &
      scalar               ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: ierr  ! local MPI error flag
   character (23), parameter :: &
      rtnName = 'SCRIP_BroadcastScalarR8'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   call MPI_BCAST(scalar, 1, SCRIP_mpiR8, srcTask, &
                             SCRIP_communicator, ierr)
   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)
   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

end subroutine SCRIP_BroadcastScalarR8

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastScalarR4
! !INTERFACE:

subroutine SCRIP_BroadcastScalarR4(scalar, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a scalar real variable from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module
!
! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), intent(inout) :: &
      scalar               ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: ierr  ! local MPI error flag
   character (23), parameter :: &
      rtnName = 'SCRIP_BroadcastScalarR4'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   call MPI_BCAST(scalar, 1, SCRIP_mpiR4, srcTask, &
                             SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastScalarR4

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastScalarI4
! !INTERFACE:

subroutine SCRIP_BroadcastScalarI4(scalar, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a scalar integer variable from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(inout) :: &
      scalar                ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: ierr  ! local MPI error flag
   character (23), parameter :: &
      rtnName = 'SCRIP_BroadcastScalarI4'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   call MPI_BCAST(scalar, 1, MPI_INTEGER, srcTask, &
                             SCRIP_communicator,ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastScalarI4

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastScalarLog
! !INTERFACE:

subroutine SCRIP_BroadcastScalarLog(scalar, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a scalar logical variable from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), intent(inout) :: &
     scalar               ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     itmp,               &! local temporary
     ierr                 ! MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastScalarLog'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   if (scalar) then
     itmp = 1
   else
     itmp = 0
   endif

   call MPI_BCAST(itmp, 1, MPI_INTEGER, srcTask, &
                           SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

   if (itmp == 1) then
     scalar = .true.
   else
     scalar = .false.
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastScalarLog

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastScalarChar
! !INTERFACE:

subroutine SCRIP_BroadcastScalarChar(scalar, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a scalar character variable from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   character (*), intent(inout) :: &
     scalar               ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     clength,            &! length of character
     ierr                 ! MPI error flag
   character (25), parameter :: &
      rtnName = 'SCRIP_BroadcastScalarChar'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   clength = len(scalar)

   call MPI_BCAST(scalar, clength, MPI_CHARACTER, srcTask, &
                                   SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                       'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                     'error in MPI Barrier')
      return
   endif

!--------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastScalarChar

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR8D1
! !INTERFACE:

subroutine SCRIP_BroadcastArrayR8D1(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a vector real8 variable from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:), intent(inout) :: &
      array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
      nelements,       &! size of array
      ierr              ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR8D1'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR8, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR8D1

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR4D1
! !INTERFACE:

subroutine SCRIP_BroadcastArrayR4D1(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a real vector from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
      nelements,       &! size of array to be broadcast
      ierr              ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR4D1'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR4, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR4D1

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayI4D1
! !INTERFACE:

subroutine SCRIP_BroadcastArrayI4D1(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts an integer vector from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:), intent(inout) :: &
       array              ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayI4D1'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, MPI_INTEGER, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayI4D1

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayLogD1
! !INTERFACE:

subroutine SCRIP_BroadcastArrayLogD1(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a logical vector from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4), dimension(:), allocatable :: &
      tmpArray             ! temporary array for MPI bcast

   integer (SCRIP_i4) :: &
      nelements,          &! size of array to be broadcast
      ierr                 ! local MPI error flag
   character (25), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayLogD1'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)
   allocate(tmpArray(nelements))

   where (array)
     tmpArray = 1
   elsewhere
     tmpArray = 0
   end where

   call MPI_BCAST(tmpArray, nelements, MPI_INTEGER, srcTask, &
                                       SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                     'error in MPI Barrier')
      return
   endif

   where (tmpArray == 1)
     array = .true.
   elsewhere
     array = .false.
   end where

   deallocate(tmpArray)

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayLogD1

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayCharD1
! !INTERFACE:

subroutine SCRIP_BroadcastArrayCharD1(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a character vector from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   character (SCRIP_charLength), dimension(:), intent(inout) :: &
       array              ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (26), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayCharD1'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements*SCRIP_charLength, MPI_CHARACTER, &
                         srcTask, SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                       'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                     'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayCharD1

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR8D2
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR8D2(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a real8 2d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
      nelements,         &! size of array
      ierr                ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR8D2'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR8, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR8D2

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR4D2
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR4D2(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a real 2d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR4D2'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR4, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR4D2

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayI4D2
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayI4D2(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a 2d integer array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:), intent(inout) :: &
       array              ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayI4D2'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, MPI_INTEGER, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)
   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayI4D2

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayLogD2
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayLogD2(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a logical 2d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4), dimension(:,:), allocatable :: &
     tmpArray             ! temporary array for MPI bcast

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (25), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayLogD2'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)
   allocate(tmpArray(size(array,dim=1),size(array,dim=2)))

   where (array)
     tmpArray = 1
   elsewhere
     tmpArray = 0
   end where

   call MPI_BCAST(tmpArray, nelements, MPI_INTEGER, srcTask, &
                                       SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

   where (tmpArray == 1)
     array = .true.
   elsewhere
     array = .false.
   end where

   deallocate(tmpArray)

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayLogD2

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR8D3
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR8D3(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a double 3d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,       &! size of array
     ierr              ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR8D3'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR8, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR8D3

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR4D3
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR4D3(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a real 3d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR4D3'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR4, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR4D3

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayI4D3
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayI4D3(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts an integer 3d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask             ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:,:), intent(inout) :: &
      array               ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayI4D3'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, MPI_INTEGER, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayI4D3

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayLogD3
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayLogD3(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a logical 3d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask             ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:,:), intent(inout) :: &
      array               ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4), dimension(:,:,:), allocatable :: &
     tmpArray            ! temporary array for MPI bcast

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (25), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayLogD3'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)
   allocate(tmpArray(size(array,dim=1), &
                     size(array,dim=2), &
                     size(array,dim=3)))

   where (array)
     tmpArray = 1
   elsewhere
     tmpArray = 0
   end where

   call MPI_BCAST(tmpArray, nelements, MPI_INTEGER, srcTask, &
                                       SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

   where (tmpArray == 1)
     array = .true.
   elsewhere
     array = .false.
   end where

   deallocate(tmpArray)

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayLogD3

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR8D4
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR8D4(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a double 4d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:,:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,       &! size of array
     ierr              ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR8D4'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR8, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR8D4

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR4D4
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR4D4(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a real 4d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR4D4'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR4, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR4D4

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayI4D4
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayI4D4(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts an integer 4d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask             ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:,:,:), intent(inout) :: &
      array               ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayI4D4'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, MPI_INTEGER, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayI4D4

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayLogD4
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayLogD4(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a logical 4d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask             ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:,:,:), intent(inout) :: &
      array               ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4), dimension(:,:,:,:), allocatable :: &
     tmpArray            ! temporary array for MPI bcast

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (25), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayLogD4'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)
   allocate(tmpArray(size(array,dim=1), &
                     size(array,dim=2), &
                     size(array,dim=3), &
                     size(array,dim=4)))

   where (array)
     tmpArray = 1
   elsewhere
     tmpArray = 0
   end where

   call MPI_BCAST(tmpArray, nelements, MPI_INTEGER, srcTask, &
                                       SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

   where (tmpArray == 1)
     array = .true.
   elsewhere
     array = .false.
   end where

   deallocate(tmpArray)

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayLogD4

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR8D5
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR8D5(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a double 5d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:,:,:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,       &! size of array
     ierr              ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR8D5'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR8, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR8D5

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR4D5
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR4D5(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a real 5d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:,:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR4D5'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR4, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR4D5

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayI4D5
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayI4D5(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts an integer 5d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask             ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:,:,:,:), intent(inout) :: &
      array               ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayI4D5'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, MPI_INTEGER, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayI4D5

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayLogD5
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayLogD5(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a logical 5d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask             ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:,:,:,:), intent(inout) :: &
      array               ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4), dimension(:,:,:,:,:), allocatable :: &
     tmpArray            ! temporary array for MPI bcast

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (25), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayLogD5'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)
   allocate(tmpArray(size(array,dim=1), &
                     size(array,dim=2), &
                     size(array,dim=3), &
                     size(array,dim=4), &
                     size(array,dim=5)))

   where (array)
     tmpArray = 1
   elsewhere
     tmpArray = 0
   end where

   call MPI_BCAST(tmpArray, nelements, MPI_INTEGER, srcTask, &
                                       SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

   where (tmpArray == 1)
     array = .true.
   elsewhere
     array = .false.
   end where

   deallocate(tmpArray)

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayLogD5

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR8D6
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR8D6(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a double 6d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:,:,:,:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,       &! size of array
     ierr              ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR8D6'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR8, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR8D6

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR4D6
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR4D6(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a real 6d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:,:,:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayR4D6'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, SCRIP_mpiR4, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayR4D6

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayI4D6
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayI4D6(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts an integer 6d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask             ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:,:,:,:,:), intent(inout) :: &
      array               ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (24), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayI4D6'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)

   call MPI_BCAST(array, nelements, MPI_INTEGER, srcTask, &
                                    SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                      'error in MPI Barrier')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayI4D6

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayLogD6
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayLogD6(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a logical 6d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask             ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:,:,:,:,:), intent(inout) :: &
      array               ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode           ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4), dimension(:,:,:,:,:,:), allocatable :: &
     tmpArray            ! temporary array for MPI bcast

   integer (SCRIP_i4) :: &
     nelements,          &! size of array to be broadcast
     ierr                 ! local MPI error flag
   character (25), parameter :: &
      rtnName = 'SCRIP_BroadcastArrayLogD6'

!-----------------------------------------------------------------------

   errorCode = SCRIP_success

   nelements = size(array)
   allocate(tmpArray(size(array,dim=1), &
                     size(array,dim=2), &
                     size(array,dim=3), &
                     size(array,dim=4), &
                     size(array,dim=5), &
                     size(array,dim=6)))

   where (array)
     tmpArray = 1
   elsewhere
     tmpArray = 0
   end where

   call MPI_BCAST(tmpArray, nelements, MPI_INTEGER, srcTask, &
                                       SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Bcast')
      return
   endif

   call MPI_BARRIER(SCRIP_communicator, ierr)

   if (ierr /= MPI_SUCCESS) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                        'error in MPI Barrier')
      return
   endif

   where (tmpArray == 1)
     array = .true.
   elsewhere
     array = .false.
   end where

   deallocate(tmpArray)

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayLogD6

!***********************************************************************

 end module SCRIP_BroadcastMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
