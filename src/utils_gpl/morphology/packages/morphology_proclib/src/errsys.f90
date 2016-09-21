subroutine errsys(string,ierr)
!**********************************************************************
!     +------------------------+
!     |    D E L T A R E S     |
!     +------------------------+
!
!***********************************************************************
!
!     Project : Open Processes Library
!     Author  : Jan van Beek, Arjen Markus
!     Date    : 091019             Version : 1.00
!
!     History :
!
!     Date    Author          Description
!     ------  --------------  -----------------------------------
!     091019  Arjen Markus    Adapted original for use in OPL
!***********************************************************************
!
!     Description of the module :
!
!        Print an error message and stop the program
!        This is a simplified version of the original to make sure
!        that the message is always visible.
!
!        Reason:
!        On Windows files opened to a unit number in the main program
!        seem to be independent of the files opened to the same unit
!        number in a DLL. As the original writes to the monitoring
!        file, the message would be hidden in a file like "fort.88"
!
    implicit none

    character*(*) :: string
    integer       :: ierr

    write(*,*) string
    write(*,*) 'The program stopped because of this'

    stop
end subroutine errsys
