!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module SCRIP_ConfigMod

!BOP
! !MODULE:  SCRIP_ConfigMod
!
! !DESCRIPTION:
!  This module contains routines for reading input configuration
!  data from a configuration file.  Variables for a specified module 
!  are read from an input file and broadcast to all processors.
!  A default value for a variable can be specified and will be
!  used if the variable is not found in the input file.
!
! !REVISION HISTORY:
!  SVN:$Id: SCRIP_ConfigMod.F90 15 2006-08-21 20:04:13Z  $
!  2007-12-17: Phil Jones
!     initial version
!
! !USES:

   use SCRIP_KindsMod
   use SCRIP_ErrorMod
   use SCRIP_IOUnitsMod
   use SCRIP_CommMod
   use SCRIP_BroadcastMod

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: SCRIP_ConfigOpen,               &
             SCRIP_ConfigClose,              &
             SCRIP_ConfigRead

! !PUBLIC DATA MEMBERS:

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  private config module variables
!
!-----------------------------------------------------------------------

   character (SCRIP_charLength), parameter :: &
      configFileDefault = 'scrip_in'

   interface SCRIP_ConfigRead
      module procedure SCRIP_ConfigReadI4,      &
                       SCRIP_ConfigReadR4,      &
                       SCRIP_ConfigReadR8,      &
                       SCRIP_ConfigReadLogical, &
                       SCRIP_ConfigReadCharacter
   end interface

!EOC
!***********************************************************************

contains

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_ConfigOpen
! !INTERFACE:

 subroutine SCRIP_ConfigOpen(iunit, errorCode, configFileName)

! !DESCRIPTION:
!  This routine opens a configuration input file for reading.
!  If no filename is supplied, the default filename is used.
!  The unit number for the input file is returned for use by
!  the configuration read routines.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   character (*), intent(in), optional :: &
      configFileName

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      iunit,                   &! I/O unit for config file
      errorCode                 ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (SCRIP_i4) :: ierr  ! status flag from open

   character (16), parameter :: rtnName = 'SCRIP_ConfigOpen'

!-----------------------------------------------------------------------
!
!  get unit number for input file to be read
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

   call SCRIP_IOUnitsGet(iunit)

!-----------------------------------------------------------------------
!
!  if this is master task, open the file for reading and check for 
!  errors
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then
      if (present(configFileName)) then
         open(unit=iunit, file=configFileName, form='formatted', &
              status='old', action='read', position='rewind',    &
              iostat=ierr)
      else
         open(unit=iunit, file=configFileDefault, form='formatted', &
              status='old', action='read', position='rewind',       &
              iostat=ierr)
      endif
   endif

   call SCRIP_Broadcast(ierr, SCRIP_masterTask, errorCode)

   if (ierr > 0 .or. errorCode /= SCRIP_Success) then
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'error opening config file')
      return
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_ConfigOpen

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_ConfigClose
! !INTERFACE:

 subroutine SCRIP_ConfigClose(iunit, errorCode)

! !DESCRIPTION:
!  This routine closes an open configuration file and releases
!  the assigned unit.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      iunit                     ! I/O unit for config file

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      errorCode                 ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  if this is master task, open the file for reading and check for 
!  errors
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success

   if (SCRIP_myTask == SCRIP_masterTask) close(iunit)

!-----------------------------------------------------------------------
!
!  release unit number
!
!-----------------------------------------------------------------------

   call SCRIP_IOUnitsRelease(iunit)

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_ConfigClose

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_ConfigRead
! !INTERFACE:

 subroutine SCRIP_ConfigReadI4(iunit, moduleName, variableName,   &
                             variable, defaultValue, errorCode, &
                             outStringBefore, outStringAfter)

! !DESCRIPTION:
!  This routine reads a variable from a configuration input file
!  that has already been opened with a ConfigOpen call.  Each variable
!  in the input file is associated with a module, so the module name
!  must also be supplied.  If the variable is not present in the
!  input file, the defaultValue is assigned.  After a successful read, 
!  the value for the variable is broadcasted to other processors.
!  Finally, the value is printing to stdout using either a generic
!  output string or user-specified output defined by outStringBefore
!  and outStringAfter.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETER:

   integer (SCRIP_i4), intent(in) :: &
      iunit                    ! i/o unit of config file

   character (*), intent(in) :: &
      moduleName,               &! name of module where this var resides
      variableName               ! name of variable to be read

   integer (SCRIP_i4), intent(in) :: &
      defaultValue               ! default value to assign to variable

   character (*), intent(in), optional :: &
      outStringBefore,   &! optional output string to precede variable value
      outStringAfter      ! optional output string to follow  variable value

! !OUTPUT PARAMETERS:

   integer (SCRIP_i4), intent(out) :: &
      variable                  ! variable to assing input value

   integer (SCRIP_i4), intent(out) :: &
      errorCode                 ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   logical (SCRIP_logical) :: &
      moduleFound,          &! logical flag for module   search
      variableFound,        &! logical flag for variable search
      isOpen                 ! logical flag for file inquiry

   integer (SCRIP_i4) :: &
      istat,                &! I/O status flag
      indx,                 &! index for manipulating string
      errVal                 ! internal error flag

   character (SCRIP_charLength) :: &
      inputString,          &! temp for reading each record
      tmpString              ! temp for manipulating input string

   character (16), parameter :: &
      rtnName = 'SCRIP_ConfigRead'

!-----------------------------------------------------------------------
!
!  check to see if unit is open and rewind unit
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success
   errVal = 0

   if (SCRIP_myTask == SCRIP_masterTask) then
      inquire(unit=iunit, opened=isOpen)
      if (isOpen) then
         rewind(iunit)
      else
         errVal = -1
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for module name
!
!-----------------------------------------------------------------------

   moduleFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. isOpen) then
      moduleSearch: do 

         ! read line from input file
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit moduleSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit moduleSearch
         endif

         ! look for ampersand, signifying a module name
         ! then check module name for a match
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '&') then
            if (trim(tmpString(2:)) == trim(moduleName)) then
               moduleFound = .true.
               exit moduleSearch
            endif
         else
            cycle moduleSearch
         endif
         
      end do moduleSearch

      if (.not. moduleFound) then
         errVal = -3
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for variable name
!
!-----------------------------------------------------------------------

   variableFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. moduleFound) then
      varSearch: do 

         ! read line from input file: should be name = value
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit varSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit varSearch
         endif

         ! check for end of module block
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '/') exit varSearch

         ! then check for a variable name match
         indx = index(tmpString,'=')
         if (trim(adjustl(tmpString(1:indx-1))) == &
             trim(variableName)) then
            variableFound = .true.
            exit varSearch
         endif
      end do varSearch
   endif

!-----------------------------------------------------------------------
!
!  check for errors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(errVal, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting error value')) return

   select case(errVal)
   case (-1)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'config file not opened for reading')
      return
   case (-2)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'error reading record from config file')
      return
   case (-3)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'module name not found in config file')
      return
   case default
   end select

!-----------------------------------------------------------------------
!
!  extract value from input string or set value to default
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then

      if (variableFound) then
         read(tmpString(indx+1:),*) variable
      else
         variable = defaultValue
         write(SCRIP_stdout, '(a37,a)') &
            '   Using default value for variable: ', variableName
      endif
   endif

!-----------------------------------------------------------------------
!
!  broadcast value to all processors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(variable, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting variable')) return

!-----------------------------------------------------------------------
!
!  output value to stdout
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then
      if (present(outStringBefore)) then
         tmpString = outStringBefore
      else
         tmpString(:) = ' '
         write(tmpString,'(a,a3)') variableName,' = '
      endif

      if (present(outStringAfter)) then
         write(SCRIP_stdout,'(a,a1,i10,a)') trim(tmpString), ' ', &
                                          variable, outStringAfter
      else
         write(SCRIP_stdout,'(a,a1,i10)'  ) trim(tmpString), ' ', &
                                          variable
      endif
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_ConfigReadI4

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_ConfigRead
! !INTERFACE:

 subroutine SCRIP_ConfigReadR4(iunit, moduleName, variableName,   &
                             variable, defaultValue, errorCode, &
                             outStringBefore, outStringAfter)

! !DESCRIPTION:
!  This routine reads a variable from a configuration input file
!  that has already been opened with a ConfigOpen call.  Each variable
!  in the input file is associated with a module, so the module name
!  must also be supplied.  If the variable is not present in the
!  input file, the defaultValue is assigned.  After a successful read, 
!  the value for the variable is broadcasted to other processors.
!  Finally, the value is printing to stdout using either a generic
!  output string or user-specified output defined by outStringBefore
!  and outStringAfter.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETER:

   integer (SCRIP_i4), intent(in) :: &
      iunit                    ! i/o unit of config file

   character (*), intent(in) :: &
      moduleName,               &! name of module where this var resides
      variableName               ! name of variable to be read

   real (SCRIP_r4), intent(in) :: &
      defaultValue               ! default value to assign to variable

   character (*), intent(in), optional :: &
      outStringBefore,   &! optional output string to precede variable value
      outStringAfter      ! optional output string to follow  variable value

! !OUTPUT PARAMETERS:

   real (SCRIP_r4), intent(out) :: &
      variable                  ! variable to assing input value

   integer (SCRIP_i4), intent(out) :: &
      errorCode                 ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   logical (SCRIP_logical) :: &
      moduleFound,          &! logical flag for module   search
      variableFound,        &! logical flag for variable search
      isOpen                 ! logical flag for file inquiry

   integer (SCRIP_i4) :: &
      istat,                &! I/O status flag
      indx,                 &! index for manipulating string
      errVal                 ! internal error flag

   character (SCRIP_charLength) :: &
      inputString,          &! temp for reading each record
      tmpString              ! temp for manipulating input string

   character (16), parameter :: &
      rtnName = 'SCRIP_ConfigRead'

!-----------------------------------------------------------------------
!
!  check to see if unit is open and rewind unit
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success
   errVal = 0

   if (SCRIP_myTask == SCRIP_masterTask) then
      inquire(unit=iunit, opened=isOpen)
      if (isOpen) then
         rewind(iunit)
      else
         errVal = -1
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for module name
!
!-----------------------------------------------------------------------

   moduleFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. isOpen) then
      moduleSearch: do 

         ! read line from input file
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit moduleSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit moduleSearch
         endif

         ! look for ampersand, signifying a module name
         ! then check module name for a match
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '&') then
            if (trim(tmpString(2:)) == trim(moduleName)) then
               moduleFound = .true.
               exit moduleSearch
            endif
         else
            cycle moduleSearch
         endif
         
      end do moduleSearch

      if (.not. moduleFound) then
         errVal = -3
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for variable name
!
!-----------------------------------------------------------------------

   variableFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. moduleFound) then
      varSearch: do 

         ! read line from input file: should be name = value
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit varSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit varSearch
         endif

         ! check for end of module block
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '/') exit varSearch

         ! then check for a variable name match
         indx = index(tmpString,'=')
         if (trim(adjustl(tmpString(1:indx-1))) == &
             trim(variableName)) then
            variableFound = .true.
            exit varSearch
         endif
      end do varSearch
   endif

!-----------------------------------------------------------------------
!
!  check for errors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(errVal, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting error value')) return

   select case(errVal)
   case (-1)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'config file not opened for reading')
      return
   case (-2)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'error reading record from config file')
      return
   case (-3)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'module name not found in config file')
      return
   case default
   end select

!-----------------------------------------------------------------------
!
!  extract value from input string or set value to default
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then

      if (variableFound) then
         read(tmpString(indx+1:),*) variable
      else
         variable = defaultValue
         write(SCRIP_stdout, '(a37,a)') &
            '   Using default value for variable: ', variableName
      endif
   endif

!-----------------------------------------------------------------------
!
!  broadcast value to all processors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(variable, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting variable')) return

!-----------------------------------------------------------------------
!
!  output value to stdout
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then
      if (present(outStringBefore)) then
         tmpString = outStringBefore
      else
         write(tmpString,'(a,a3)') variableName,' = '
      endif

      if (present(outStringAfter)) then
         write(SCRIP_stdout,'(a,a1,1pe12.5,a)') trim(tmpString), ' ', &
                                              variable, outStringAfter
      else
         write(SCRIP_stdout,'(a,a1,1pe12.5)'  ) trim(tmpString), ' ', &
                                              variable
      endif
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_ConfigReadR4

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_ConfigRead
! !INTERFACE:

 subroutine SCRIP_ConfigReadR8(iunit, moduleName, variableName,   &
                             variable, defaultValue, errorCode, &
                             outStringBefore, outStringAfter)

! !DESCRIPTION:
!  This routine reads a variable from a configuration input file
!  that has already been opened with a ConfigOpen call.  Each variable
!  in the input file is associated with a module, so the module name
!  must also be supplied.  If the variable is not present in the
!  input file, the defaultValue is assigned.  After a successful read, 
!  the value for the variable is broadcasted to other processors.
!  Finally, the value is printing to stdout using either a generic
!  output string or user-specified output defined by outStringBefore
!  and outStringAfter.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETER:

   integer (SCRIP_i4), intent(in) :: &
      iunit                    ! i/o unit of config file

   character (*), intent(in) :: &
      moduleName,               &! name of module where this var resides
      variableName               ! name of variable to be read

   real (SCRIP_r8), intent(in) :: &
      defaultValue               ! default value to assign to variable

   character (*), intent(in), optional :: &
      outStringBefore,   &! optional output string to precede variable value
      outStringAfter      ! optional output string to follow  variable value

! !OUTPUT PARAMETERS:

   real (SCRIP_r8), intent(out) :: &
      variable                  ! variable to assign input value

   integer (SCRIP_i4), intent(out) :: &
      errorCode                 ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   logical (SCRIP_logical) :: &
      moduleFound,          &! logical flag for module   search
      variableFound,        &! logical flag for variable search
      isOpen                 ! logical flag for file inquiry

   integer (SCRIP_i4) :: &
      istat,                &! I/O status flag
      indx,                 &! index for manipulating string
      errVal                 ! internal error flag

   character (SCRIP_charLength) :: &
      inputString,          &! temp for reading each record
      tmpString              ! temp for manipulating input string

   character (16), parameter :: &
      rtnName = 'SCRIP_ConfigRead'

!-----------------------------------------------------------------------
!
!  check to see if unit is open and rewind unit
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success
   errVal = 0

   if (SCRIP_myTask == SCRIP_masterTask) then
      inquire(unit=iunit, opened=isOpen)
      if (isOpen) then
         rewind(iunit)
      else
         errVal = -1
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for module name
!
!-----------------------------------------------------------------------

   moduleFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. isOpen) then
      moduleSearch: do 

         ! read line from input file
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit moduleSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit moduleSearch
         endif

         ! look for ampersand, signifying a module name
         ! then check module name for a match
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '&') then
            if (trim(tmpString(2:)) == trim(moduleName)) then
               moduleFound = .true.
               exit moduleSearch
            endif
         else
            cycle moduleSearch
         endif
         
      end do moduleSearch

      if (.not. moduleFound) then
         errVal = -3
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for variable name
!
!-----------------------------------------------------------------------

   variableFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. moduleFound) then
      varSearch: do 

         ! read line from input file: should be name = value
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit varSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit varSearch
         endif

         ! check for end of module block
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '/') exit varSearch

         ! then check for a variable name match
         indx = index(tmpString,'=')
         if (trim(adjustl(tmpString(1:indx-1))) == &
             trim(variableName)) then
            variableFound = .true.
            exit varSearch
         endif
      end do varSearch
   endif

!-----------------------------------------------------------------------
!
!  check for errors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(errVal, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting error value')) return

   select case(errVal)
   case (-1)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'config file not opened for reading')
      return
   case (-2)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'error reading record from config file')
      return
   case (-3)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'module name not found in config file')
      return
   case default
   end select

!-----------------------------------------------------------------------
!
!  extract value from input string or set value to default
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then

      if (variableFound) then
         read(tmpString(indx+1:),*) variable
      else
         variable = defaultValue
         write(SCRIP_stdout, '(a37,a)') &
            '   Using default value for variable: ', variableName
      endif
   endif

!-----------------------------------------------------------------------
!
!  broadcast value to all processors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(variable, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting variable')) return

!-----------------------------------------------------------------------
!
!  output value to stdout
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then
      if (present(outStringBefore)) then
         tmpString = outStringBefore
      else
         write(tmpString,'(a,a3)') variableName,' = '
      endif

      if (present(outStringAfter)) then
         write(SCRIP_stdout,'(a,a1,1pe22.15,a)') trim(tmpString), ' ', &
                                               variable, outStringAfter
      else
         write(SCRIP_stdout,'(a,a1,1pe22.15)'  ) trim(tmpString), ' ', &
                                               variable
      endif
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_ConfigReadR8

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_ConfigRead
! !INTERFACE:

 subroutine SCRIP_ConfigReadLogical(iunit, moduleName, variableName,  &
                             variable, defaultValue, errorCode,     &
                             outStringBefore, outStringAfter)

! !DESCRIPTION:
!  This routine reads a variable from a configuration input file
!  that has already been opened with a ConfigOpen call.  Each variable
!  in the input file is associated with a module, so the module name
!  must also be supplied.  If the variable is not present in the
!  input file, the defaultValue is assigned.  After a successful read, 
!  the value for the variable is broadcasted to other processors.
!  Finally, the value is printing to stdout using either a generic
!  output string or user-specified output defined by outStringBefore
!  and outStringAfter.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETER:

   integer (SCRIP_i4), intent(in) :: &
      iunit                    ! i/o unit of config file

   character (*), intent(in) :: &
      moduleName,               &! name of module where this var resides
      variableName               ! name of variable to be read

   logical (SCRIP_logical), intent(in) :: &
      defaultValue               ! default value to assign to variable

   character (*), intent(in), optional :: &
      outStringBefore,   &! optional output string to precede variable value
      outStringAfter      ! optional output string to follow  variable value

! !OUTPUT PARAMETERS:

   logical (SCRIP_logical), intent(out) :: &
      variable                  ! variable to assign input value

   integer (SCRIP_i4), intent(out) :: &
      errorCode                 ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   logical (SCRIP_logical) :: &
      moduleFound,          &! logical flag for module   search
      variableFound,        &! logical flag for variable search
      isOpen                 ! logical flag for file inquiry

   integer (SCRIP_i4) :: &
      istat,                &! I/O status flag
      indx,                 &! index for manipulating string
      errVal                 ! internal error flag

   character (SCRIP_charLength) :: &
      inputString,          &! temp for reading each record
      tmpString              ! temp for manipulating input string

   character (16), parameter :: &
      rtnName = 'SCRIP_ConfigRead'

!-----------------------------------------------------------------------
!
!  check to see if unit is open and rewind unit
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success
   errVal = 0

   if (SCRIP_myTask == SCRIP_masterTask) then
      inquire(unit=iunit, opened=isOpen)
      if (isOpen) then
         rewind(iunit)
      else
         errVal = -1
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for module name
!
!-----------------------------------------------------------------------

   moduleFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. isOpen) then
      moduleSearch: do 

         ! read line from input file
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit moduleSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit moduleSearch
         endif

         ! look for ampersand, signifying a module name
         ! then check module name for a match
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '&') then
            if (trim(tmpString(2:)) == trim(moduleName)) then
               moduleFound = .true.
               exit moduleSearch
            endif
         else
            cycle moduleSearch
         endif
         
      end do moduleSearch

      if (.not. moduleFound) then
         errVal = -3
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for variable name
!
!-----------------------------------------------------------------------

   variableFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. moduleFound) then
      varSearch: do 

         ! read line from input file: should be name = value
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit varSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit varSearch
         endif

         ! check for end of module block
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '/') exit varSearch

         ! then check for a variable name match
         indx = index(tmpString,'=')
         if (trim(adjustl(tmpString(1:indx-1))) == &
             trim(variableName)) then
            variableFound = .true.
            exit varSearch
         endif
      end do varSearch
   endif

!-----------------------------------------------------------------------
!
!  check for errors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(errVal, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting error value')) return

   select case(errVal)
   case (-1)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'config file not opened for reading')
      return
   case (-2)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'error reading record from config file')
      return
   case (-3)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'module name not found in config file')
      return
   case default
   end select

!-----------------------------------------------------------------------
!
!  extract value from input string or set value to default
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then

      if (variableFound) then
         read(tmpString(indx+1:),*) variable
      else
         variable = defaultValue
         write(SCRIP_stdout, '(a37,a)') &
            '   Using default value for variable: ', variableName
      endif
   endif

!-----------------------------------------------------------------------
!
!  broadcast value to all processors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(variable, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting variable')) return

!-----------------------------------------------------------------------
!
!  output value to stdout
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then
      if (present(outStringBefore)) then
         tmpString = outStringBefore
      else
         write(tmpString,'(a,a3)') variableName,' = '
      endif

      if (present(outStringAfter)) then
         if (variable) then
            write(SCRIP_stdout,'(a,a5,a)') trim(tmpString), ' true', &
                                         outStringAfter
         else
            write(SCRIP_stdout,'(a,a6,a)') trim(tmpString), ' false', &
                                         outStringAfter
         endif
      else
         if (variable) then
            write(SCRIP_stdout,'(a,a5)') trim(tmpString), ' true'
         else
            write(SCRIP_stdout,'(a,a6)') trim(tmpString), ' false'
         endif
      endif
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_ConfigReadLogical

!***********************************************************************
!BOP
! !IROUTINE: SCRIP_ConfigRead
! !INTERFACE:

 subroutine SCRIP_ConfigReadCharacter(iunit, moduleName, variableName, &
                             variable, defaultValue, errorCode,      &
                             outStringBefore, outStringAfter)

! !DESCRIPTION:
!  This routine reads a variable from a configuration input file
!  that has already been opened with a ConfigOpen call.  Each variable
!  in the input file is associated with a module, so the module name
!  must also be supplied.  If the variable is not present in the
!  input file, the defaultValue is assigned.  After a successful read, 
!  the value for the variable is broadcasted to other processors.
!  Finally, the value is printing to stdout using either a generic
!  output string or user-specified output defined by outStringBefore
!  and outStringAfter.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETER:

   integer (SCRIP_i4), intent(in) :: &
      iunit                    ! i/o unit of config file

   character (*), intent(in) :: &
      moduleName,               &! name of module where this var resides
      variableName               ! name of variable to be read

   character (*), intent(in) :: &
      defaultValue               ! default value to assign to variable

   character (*), intent(in), optional :: &
      outStringBefore,   &! optional output string to precede variable value
      outStringAfter      ! optional output string to follow  variable value

! !OUTPUT PARAMETERS:

   character (SCRIP_charLength), intent(out) :: &
      variable                  ! variable to assign input value

   integer (SCRIP_i4), intent(out) :: &
      errorCode                 ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   logical (SCRIP_logical) :: &
      moduleFound,          &! logical flag for module   search
      variableFound,        &! logical flag for variable search
      isOpen                 ! logical flag for file inquiry

   integer (SCRIP_i4) :: &
      istat,                &! I/O status flag
      indx,                 &! index for manipulating string
      errVal                 ! internal error flag

   character (SCRIP_charLength) :: &
      inputString,          &! temp for reading each record
      tmpString              ! temp for manipulating input string

   character (16), parameter :: &
      rtnName = 'SCRIP_ConfigRead'

!-----------------------------------------------------------------------
!
!  check to see if unit is open and rewind unit
!
!-----------------------------------------------------------------------

   errorCode = SCRIP_Success
   errVal = 0

   if (SCRIP_myTask == SCRIP_masterTask) then
      inquire(unit=iunit, opened=isOpen)
      if (isOpen) then
         rewind(iunit)
      else
         errVal = -1
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for module name
!
!-----------------------------------------------------------------------

   moduleFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. isOpen) then
      moduleSearch: do 

         ! read line from input file
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit moduleSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit moduleSearch
         endif

         ! look for ampersand, signifying a module name
         ! then check module name for a match
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '&') then
            if (trim(tmpString(2:)) == trim(moduleName)) then
               moduleFound = .true.
               exit moduleSearch
            endif
         else
            cycle moduleSearch
         endif
         
      end do moduleSearch

      if (.not. moduleFound) then
         errVal = -3
      endif
   endif

!-----------------------------------------------------------------------
!
!  look for variable name
!
!-----------------------------------------------------------------------

   variableFound = .false.
   if (SCRIP_myTask == SCRIP_masterTask .and. moduleFound) then
      varSearch: do 

         ! read line from input file: should be name = value
         read(iunit, '(a100)', iostat=istat) inputString

         ! check for read errors
         if (istat < 0) then ! end of file
            exit varSearch
         else if (istat > 0) then ! error reading from file
            errVal = -2
            exit varSearch
         endif

         ! check for end of module block
         tmpString = adjustl(inputString)
         if (tmpString(1:1) == '/') exit varSearch

         ! then check for a variable name match
         indx = index(tmpString,'=')
         if (trim(adjustl(tmpString(1:indx-1))) == &
             trim(variableName)) then
            variableFound = .true.
            exit varSearch
         endif
      end do varSearch
   endif

!-----------------------------------------------------------------------
!
!  check for errors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(errVal, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting error value')) return

   select case(errVal)
   case (-1)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'config file not opened for reading')
      return
   case (-2)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'error reading record from config file')
      return
   case (-3)
      call SCRIP_ErrorSet(errorCode, rtnName, &
                          'module name not found in config file')
      return
   case default
   end select

!-----------------------------------------------------------------------
!
!  extract value from input string or set value to default
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then

      if (variableFound) then
         read(tmpString(indx+1:),*) variable
      else
         variable = defaultValue
         write(SCRIP_stdout, '(a37,a)') &
            '   Using default value for variable: ', variableName
      endif
   endif

!-----------------------------------------------------------------------
!
!  broadcast value to all processors
!
!-----------------------------------------------------------------------

   call SCRIP_Broadcast(variable, SCRIP_masterTask, errorCode)

   if (SCRIP_ErrorCheck(errorCode, rtnName, &
                        'error broadcasting variable')) return

!-----------------------------------------------------------------------
!
!  output value to stdout
!
!-----------------------------------------------------------------------

   if (SCRIP_myTask == SCRIP_masterTask) then
      if (present(outStringBefore)) then
         tmpString = outStringBefore
      else
         write(tmpString,'(a,a3)') variableName,' = '
      endif

      if (present(outStringAfter)) then
         write(SCRIP_stdout,'(a,a1,a,a1,a)') trim(tmpString), ' ', &
                                           trim(variable),  ' ', &
                                           outStringAfter
      else
         write(SCRIP_stdout,'(a,a1,a)'  ) trim(tmpString), ' ', &
                                        trim(variable)
      endif
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine SCRIP_ConfigReadCharacter

!***********************************************************************

 end module SCRIP_ConfigMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
