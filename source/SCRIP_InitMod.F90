!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module SCRIP_InitMod

!BOP
! !MODULE: SCRIP_InitMod
! !DESCRIPTION:
!  This module initializes various modules and variables needed by
!  the SCRIP package.  It should be call once before any other SCRIP
!  routines.  
!
!  Users should not need to change any values in this module.
!  The module performs initialization primarily by calling other
!  initialization routines for each individual module in SCRIP.
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

   public :: SCRIP_Initialize

!EOP
!BOC
!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_Initialize -- initializes all SCRIP modules
! !INTERFACE:

 subroutine SCRIP_Initialize(errorCode)

! !DESCRIPTION:
!  This routine initializes all modules and variables in the SCRIP
!  package by calling all necessary initialization routines.
!
! !REVISION HISTORY:
!  same as module

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode              ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   character (16), parameter :: rtnName = 'SCRIP_Initialize'

!-----------------------------------------------------------------------
!
!  initialize by calling any required initialization routines
!  
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

   call SCRIP_CommInit

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_Initialize

!***********************************************************************

 end module SCRIP_InitMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
