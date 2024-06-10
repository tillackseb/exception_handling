!> TODO add description
module exception_handling_string_utils
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'exception_handling_string_utils'
  
  public :: indent, truncate_width, upper

contains

  !> Indent string.
  pure function indent( string, n ) result( indented_string )
    !> description
    character(len=*), intent(in) :: string
    !> number of columns to indent
    integer, intent(in) :: n
    !> indented string
    character(len=:), allocatable :: indented_string
  
    character(len=*), parameter :: PROCEDURE_NAME = 'indent'

    integer :: i, j

    if (n <= 0) then
      indented_string = string
      return
    end if

    indented_string = ''
    j = 1
    do i = 1, len( string )
      if (string(i:i) /= new_line(string)) cycle
      indented_string = indented_string // repeat( ' ', n ) // string(j:i)
      j = i + 1
    end do
    if (j <= len(string)) indented_string = indented_string // repeat( ' ', n ) // string(j:)
  end function indent

  !> Truncates a string to maximum number of columns and inserts line breaks if necessary.
  pure function truncate_width( string, n ) result( truncated_string )
    !> description
    character(len=*), intent(in) :: string
    !> max number of columns in truncated string
    integer, intent(in) :: n
    !> truncated string
    character(len=:), allocatable :: truncated_string
  
    character(len=*), parameter :: PROCEDURE_NAME = 'truncate_width'

    integer :: i, j

    if (n <= 0) then
      truncated_string = string
      return
    end if

    truncated_string = ''
    j = 1
    do while (j <= len( string ) - n)
      i = index( string(j:j+n), new_line(string) )
      if (i == 0) i = index( string(j:j+n), ' ', back=.true. )
      if (i == 0) i = n
      truncated_string = truncated_string // string(j:j+i-1)
      j = j + i 
      if (string(j-1:j-1) /= new_line(string)) truncated_string = truncated_string // new_line(truncated_string)
    end do
    truncated_string = truncated_string // string(j:) 
  end function truncate_width

  !> Convert string to all upper case.
  pure function upper( lstring ) result( ustring )
    !> lower case string
    character(len=*), intent(in) :: lstring
    !> upper case string
    character(len=len(lstring)) :: ustring
  
    character(len=*), parameter :: PROCEDURE_NAME = 'upper'

    integer :: i, j

    do i = 1, len( lstring )
      j = iachar( lstring(i:i) )
      if (j >= iachar('a') .and. j <= iachar('z')) then
        ustring(i:i) = achar( j - 32 )
      else
        ustring(i:i) = achar( j )
      end if
    end do
  end function upper

end module exception_handling_string_utils
