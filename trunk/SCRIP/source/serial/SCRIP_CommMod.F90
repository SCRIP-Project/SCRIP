!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!BOP

 module SCRIP_CommMod

! !MODULE: SCRIP_CommMod
! !DESCRIPTION:
!  This module contains necessary routines and variables to support
!  parallel communication modules in SCRIP.  A typical SCRIP user will
!  not call any of these routines - they are only for internal SCRIP
!  use.
!
!  The module contains communicators, tags, task ids and other 
!  information and the routines to set them up.  In addition, several
!  utility routines for setting up the communication environment are
!  included.  For this serial version, most of these routines simply
!  set dummy values and perform no operations.
!
! !REVISION HISTORY:
!  SVN:$Id: $
!
! !USES:

   use SCRIP_KindsMod

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public  :: SCRIP_CommInit,                    &
              SCRIP_CommInitMessageEnvironment,  &
              SCRIP_CommExitMessageEnvironment,  &
              SCRIP_CommAbortMessageEnvironment, &
              SCRIP_CommGetNumProcs,             &
              SCRIP_CommCreateCommunicator

! !PUBLIC DATA MEMBERS:

   integer (SCRIP_i4), public :: &
      SCRIP_communicator,         &! MPI communicator for ocn comms
      SCRIP_mpiR8,                &! MPI type for r8
      SCRIP_mpiR4,                &! MPI type for r4
      SCRIP_myTask,               &! MPI task number for this task
      SCRIP_masterTask             ! MPI task number for master task

   integer (SCRIP_i4), parameter, public :: &
      SCRIP_mpitagBndy2d         = 1,       &! MPI tags for various
      SCRIP_mpitagGS             = 1000      ! communication patterns

!EOP
!BOC
!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_CommInit
! !INTERFACE:

 subroutine SCRIP_CommInit

! !DESCRIPTION:
!  This routine sets up communication environment and defines the SCRIP
!  communicator.
!
! !REVISION HISTORY:
!  same as module
!

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!  Create communicator for internal SCRIP communications
!  serial execution - set a dummy value
!
!-----------------------------------------------------------------------

   SCRIP_communicator = 0

!-----------------------------------------------------------------------
!
!  initialize other variables with dummy value for the serial case
!
!-----------------------------------------------------------------------

   SCRIP_masterTask = 0
   SCRIP_myTask     = 0
   SCRIP_mpiR8      = 0
   SCRIP_mpiR4      = 0

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_CommInit

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_CommGetNumProcs
! !INTERFACE:

 function SCRIP_CommGetNumProcs(communicator)

! !DESCRIPTION:
!  This function returns the number of processor assigned to
!  a given communicator.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      communicator         ! communicator to query for num processors

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4) :: &
      SCRIP_CommGetNumProcs  ! number of processors in communicator

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  always return one for serial case
!
!-----------------------------------------------------------------------

   SCRIP_CommGetNumProcs = 1

!-----------------------------------------------------------------------
!EOC

 end function SCRIP_CommGetNumProcs

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_CommInitMessageEnvironment
! !INTERFACE:

 subroutine SCRIP_CommInitMessageEnvironment

! !DESCRIPTION:
!  This routine initializes the message environment.
!
! !REVISION HISTORY:
!  same as module

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  this routine does nothing in serial case
!
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_CommInitMessageEnvironment

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_CommExitMessageEnvironment
! !INTERFACE:

 subroutine SCRIP_CommExitMessageEnvironment

! !DESCRIPTION:
!  This routine exits the message environment properly when model
!  stops.
!
! !REVISION HISTORY:
!  same as module

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  serial case does nothing
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_CommExitMessageEnvironment

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_CommAbortMessageEnvironment
! !INTERFACE:

 subroutine SCRIP_CommAbortMessageEnvironment

! !DESCRIPTION:
!  This routine aborts the message environment when model stops.
!  It will attempt to abort the entire MPI COMM WORLD.
!
! !REVISION HISTORY:
!  same as module

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!  serial case does nothing
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_CommAbortMessageEnvironment

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_CommCreateCommunicator
! !INTERFACE:

 subroutine SCRIP_CommCreateCommunicator(newCommunicator, numProcs)

! !DESCRIPTION:
!  This routine enables creation of a separate communicator for a 
!  subset of processors under default communicator.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      numProcs          ! num of procs in new distribution

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      newCommunicator   ! new communicator for this distribution

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  set dummy value for serial case
!
!-----------------------------------------------------------------------

   newCommunicator = 0

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_CommCreateCommunicator

!***********************************************************************

 end module SCRIP_CommMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
