!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!BOP

 module SCRIP_CommMod

! !MODULE: SCRIP_CommMod
! !DESCRIPTION:
!  This module contains necessary routines and variables to support
!  other parallel communication modules in SCRIP.  In particular, this
!  module contains communicators, tags, task ids and other necessary
!  information and the routines to set them up.  In addition, several
!  utility routines for setting up the communication environment are
!  included.
!
! !REVISION HISTORY:
!  SVN:$Id: $
!
! !USES:

   use SCRIP_KindsMod

   implicit none
   private
   save

   include 'mpif.h'

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
      SCRIP_mpiR16,               &! MPI type for r16
      SCRIP_mpiR8,                &! MPI type for r8
      SCRIP_mpiR4,                &! MPI type for r4
      SCRIP_myTask,               &! MPI task number for this task
      SCRIP_masterTask             ! MPI task number for master task

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
!  This routine sets up default SCRIP communicator and data types.
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

   integer (SCRIP_i4) :: ierr  ! MPI error flag

!-----------------------------------------------------------------------
!
!  create communicator for internal SCRIP communications
!  Generally, it inherits the default MPI communicator from the
!  calling routine.  It is assumed that the MPI initialization occurs 
!  within the calling application or that 
!  SCRIP\_CommInitMessageEnvironment is called from the driver routine.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!  when not coupled, simply duplicate global communicator
!
!-----------------------------------------------------------------------

   call MPI_COMM_DUP(MPI_COMM_WORLD, SCRIP_communicator, ierr)

!-----------------------------------------------------------------------
!
!  determine task ids
!
!-----------------------------------------------------------------------

   SCRIP_masterTask = 0
   call MPI_COMM_RANK  (SCRIP_communicator, SCRIP_myTask, ierr)

!-----------------------------------------------------------------------
!
!  On some machines the MPI implementation makes some assumptions about
!  these data types, so these are chosen to try and choose the
!  appropriate kind.
!
!-----------------------------------------------------------------------

   SCRIP_mpiR16 = MPI_REAL16
   SCRIP_mpiR8  = MPI_REAL8
   SCRIP_mpiR4  = MPI_REAL4

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
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: ierr

!-----------------------------------------------------------------------

   call MPI_COMM_SIZE(communicator, SCRIP_CommGetNumProcs, ierr)

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

! !INCLUDES:

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: ierr  ! MPI error flag

!-----------------------------------------------------------------------

   call MPI_INIT(ierr)
   call MPI_BARRIER(MPI_COMM_WORLD,ierr)

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

! !INCLUDES:

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: ierr  ! MPI error flag

!-----------------------------------------------------------------------

   call MPI_FINALIZE(ierr)

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

! !INCLUDES:

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: errorCode, ierr  !MPI error flag

!-----------------------------------------------------------------------

   call MPI_BARRIER(SCRIP_Communicator, ierr)
   call MPI_ABORT(MPI_COMM_WORLD, errorCode, ierr)
   call MPI_FINALIZE(ierr)

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_CommAbortMessageEnvironment

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_CommCreateCommunicator
! !INTERFACE:

 subroutine SCRIP_CommCreateCommunicator(newCommunicator, numProcs)

! !DESCRIPTION:
!  This routine creates a separate communicator for a subset of
!  processors under default SCRIP communicator.
!
! !REVISION HISTORY:
!  same as module

! !INCLUDES:

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
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: &
     MPI_GROUP_OCN,         &! group of processors assigned to ocn
     MPI_GROUP_NEW           ! group of processors assigned to new dist

   integer (SCRIP_i4) :: &
     ierr                    ! error flag for MPI comms

   integer (SCRIP_i4), dimension(3) :: &
     range                   ! range of tasks assigned to new dist
                             !  (assumed 0,num_procs-1)

!-----------------------------------------------------------------------
!
!  determine group of processes assigned to distribution
!
!-----------------------------------------------------------------------

   call MPI_COMM_GROUP (SCRIP_Communicator, MPI_GROUP_OCN, ierr)

   range(1) = 0
   range(2) = numProcs-1
   range(3) = 1

!-----------------------------------------------------------------------
!
!  create subroup and communicator for new distribution
!  note: MPI_COMM_CREATE must be called by all procs in SCRIP_Communicator
!
!-----------------------------------------------------------------------

   call MPI_GROUP_RANGE_INCL(MPI_GROUP_OCN, 1, range, &
                             MPI_GROUP_NEW, ierr)

   call MPI_COMM_CREATE (SCRIP_Communicator, MPI_GROUP_NEW,  &
                         newCommunicator, ierr)

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_CommCreateCommunicator

!***********************************************************************

 end module SCRIP_CommMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
