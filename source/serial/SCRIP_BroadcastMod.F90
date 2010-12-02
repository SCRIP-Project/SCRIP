!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module SCRIP_BroadcastMod

!BOP
! !MODULE: SCRIP_BroadcastMod
! !DESCRIPTION:
!  This module contains a set of broadcast routines for scalars
!  and arrays.  It supports broadcasting a value from one task
!  to all other tasks.  This particular module contains serial
!  implementations of these routines and typically perform no
!  operations since there is no need to broadcast what is already
!  known.
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
!  Broadcasts a scalar r8 variable from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), intent(inout) :: &
      scalar               ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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
! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), intent(inout) :: &
      scalar               ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(inout) :: &
      scalar                ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), intent(inout) :: &
     scalar               ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   character (*), intent(inout) :: &
     scalar               ! scalar to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastScalarChar

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR8D1
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR8D1(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a vector r8 variable from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:), intent(inout) :: &
       array              ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   character (SCRIP_charLength), dimension(:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayCharD1

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_BroadcastArrayR8D2
! !INTERFACE:

 subroutine SCRIP_BroadcastArrayR8D2(array, srcTask, errorCode)

! !DESCRIPTION:
!  Broadcasts a r8 2d array from one processor (srcTask)
!  to all other processors. This is a specific instance of the generic
!  SCRIP\_Broadcast interface.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:), intent(inout) :: &
       array              ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:,:), intent(inout) :: &
       array              ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:,:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:,:,:), intent(inout) :: &
       array              ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:,:,:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:,:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:,:,:,:), intent(inout) :: &
       array              ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:,:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask           ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r8), dimension(:,:,:,:,:,:), intent(inout) :: &
     array             ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   real (SCRIP_r4), dimension(:,:,:,:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   integer (SCRIP_i4), dimension(:,:,:,:,:,:), intent(inout) :: &
       array              ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

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

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
     srcTask              ! processor number to broadcast from

! !INPUT/OUTPUT PARAMETERS:

   logical (SCRIP_logical), dimension(:,:,:,:,:,:), intent(inout) :: &
     array                ! array to be broadcast

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode            ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  for serial codes, nothing is required
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_BroadcastArrayLogD6

!***********************************************************************

 end module SCRIP_BroadcastMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
