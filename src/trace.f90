!> Utilities to trace back path to origin of exception.
module exception_handling_trace
  use exception_handling_configuration
  !uses
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'exception_handling_trace'

  !> Procedure trace type.
  type :: ProcedureTrace
    private
    !> comma separated list of levels of trace
    character(len=TRACE_MAX_LENGTH) :: trace = repeat( TRACE_SPECIAL, TRACE_MAX_LENGTH )
  contains
    !> add level to trace
    procedure :: add
    !> return and remove last level from trace
    procedure :: pop
    !> convert comma separated list to array of strings
    procedure :: to_array
  end type ProcedureTrace

  !> Constructor.
  interface ProcedureTrace
    module procedure :: new_procedure_trace
  end interface ProcedureTrace

  ! module ProcedureTrace type constants
  !> constant empty procedure trace
  type(ProcedureTrace), public, parameter :: EMPTY_TRACE &
    = ProcedureTrace( trace=repeat( TRACE_SPECIAL, TRACE_MAX_LENGTH ) )
  !> constant unknown procedure trace
  type(ProcedureTrace), public, parameter :: UNKNOWN_TRACE &
    = ProcedureTrace( trace=TRACE_UNKNOWN )

  public :: ProcedureTrace

contains

  !> Procedure trace constructor.
  pure function new_procedure_trace( trace_string, delimiter ) result( self )
    !> comma separated list of levels of trace   
    character(len=*), intent(in) :: trace_string
    !> delimiter (only used to parse initialization string)   
    character, intent(in) :: delimiter
    !> procedure trace
    type(ProcedureTrace) :: self
  
    character(len=*), parameter :: PROCEDURE_NAME = 'new_procedure_trace'

    integer :: i, j, n
    character(len=TRACE_MAX_LENGTH) :: add

    if (trim( adjustl( trace_string ) ) == '') then
      self = EMPTY_TRACE
      return
    end if

    n = len( trace_string )
    i = 1; j = 1
    do while (i > 0 .and. j <= n)
      i = index( trace_string(j:), delimiter )
      if (i == 0) then
        add = trim( adjustl( trace_string(j:n) ) )
      else if (i == 1) then
        add = TRACE_UNKNOWN
      else
        add = trim( adjustl( trace_string(j:j+i-2) ) )
      end if
      if (add == '') add = TRACE_UNKNOWN
      call self%add( add )
      j = j + i
    end do
    if (trace_string(n:n) == delimiter) call self%add( TRACE_UNKNOWN )
  end function new_procedure_trace

  !> Add level to trace.
  pure subroutine add( self, level )
    !> trace
    class(ProcedureTrace), intent(inout) :: self
    !> level do add
    character(len=*), intent(in) :: level
  
    character(len=*), parameter :: PROCEDURE_NAME = 'add'

    integer :: last, n

    n = len( trim( adjustl( level ) ) )
    last = index( self%trace, TRACE_SPECIAL )
    if (last + n >= TRACE_MAX_LENGTH) &
      error stop &
        MODULE_NAME // ' % ' // PROCEDURE_NAME // ': &
        Maximum trace length exeeded. Consider increasing TRACE_MAX_LENGTH.'
    if (last > 1) then
      self%trace(last:last) = TRACE_DELIMITER
      last = last + 1
    end if
    if (n > 0) then
      self%trace(last:last+n-1) = trim( adjustl( level ) )
    else
      n = len( TRACE_UNKNOWN )
      self%trace(last:last+n-1) = TRACE_UNKNOWN
    end if
  end subroutine add

  !> Return and remove last level from trace.
  pure subroutine pop( self, level )
    !> trace
    class(ProcedureTrace), intent(inout) :: self
    !> removed level
    character(len=:), allocatable, intent(out) :: level
  
    character(len=*), parameter :: PROCEDURE_NAME = 'pop'

    integer :: first, last

    first = index( self%trace, TRACE_DELIMITER, back=.true. ) + 1
    last = index( self%trace, TRACE_SPECIAL ) - 1
    if (last > 0 .and. first <= last) then
      level = self%trace(first:last)
      self%trace(first:last) = repeat( TRACE_SPECIAL, last-first+1 )
      if (first > 1) self%trace(first-1:first-1) = TRACE_SPECIAL
    else
      level = ''
      self%trace = repeat( TRACE_SPECIAL, len(self%trace) )
    end if
  end subroutine pop

  !> Convert comma separated list to array of strings.
  pure function to_array( self ) result( array )
    !> trace
    class(ProcedureTrace), intent(in) :: self
    !> array of strings
    character(len=:), allocatable :: array(:)
  
    character(len=*), parameter :: PROCEDURE_NAME = 'to_array'

    integer :: n, last, i, j, k, width

    ! find maximum length and number of elements
    n = count( [(self%trace(i:i) == TRACE_DELIMITER, i=1, len(self%trace))] ) + 1
    last = index( self%trace, TRACE_SPECIAL ) - 1
    width = 0; i = 1; j = 1
    do while (i > 0 .and. j <= last)
      i = index( self%trace(j:), TRACE_DELIMITER )
      width = max( width, i-1 )
      j = j + i
    end do
    width = max( width, last+1-j )

    ! create output
    allocate( character(len=width) :: array(n) )
    j = 1
    do k = 1, n-1
      i = index( self%trace(j:), TRACE_DELIMITER )
      array(k) = self%trace(j:j+i-2)
      j = j + i 
    end do
    array(k) = self%trace(j:last)
  end function to_array

end module exception_handling_trace
