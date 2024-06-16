!> title:   Fortran exceptions
!> author:  Sebastian Tillack
!> version: v0.1
!> date:    June 2024
!> license: GPLv3
!
!> TODO add description
module exception_handling_exception
  use exception_handling_configuration
  use exception_handling_exception_class, only : ExceptionClass, ERROR_CLASS, NO_EXCEPTION_CLASS
  use exception_handling_trace, only : ProcedureTrace, EMPTY_TRACE, UNKNOWN_TRACE
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'exception_handling_exception'

  !> Generic exception object.
  type :: Exception
    private
    !> exception class code
    type(ExceptionClass), public :: class = ERROR_CLASS
    !> custom integer code
    integer, public :: code = EXCEPTION_DEFAULT_CODE
    !> exception message
    character(len=EXCEPTION_MESSAGE_MAX_LENGTH), public :: message = ''
    !> trace leading to place exception was thrown
    type(ProcedureTrace) :: throw_trace = EMPTY_TRACE
    !> trace leading to place exception was caught
    type(ProcedureTrace) :: catch_trace = EMPTY_TRACE
    !> name of originating exception handler
    character(len=HANDLER_NAME_MAX_LENGTH) :: handler_name = ''
  contains
    !> tell where exception is thrown
    procedure :: throw
    !> tell where exception is caught
    procedure :: catch
    !> write exception to all report units
    procedure :: report
  end type Exception

  !> Constructor
  interface Exception
    module procedure :: new_exception
  end interface Exception

  ! module Exception type constants
  !> no exception
  type(Exception), public, parameter :: NO_EXCEPTION &
    = Exception( class=NO_EXCEPTION_CLASS, code=0, message='', throw_trace=EMPTY_TRACE, catch_trace=EMPTY_TRACE )

  public :: Exception

contains

  !> Exception constructor.
  pure function new_exception( exception_class, code, message, config ) result( self )
    !> exception class  
    type(ExceptionClass), intent(in) :: exception_class
    !> custom integer code  
    !> default: [[exception_handling_configuration(module):EXCEPTION_DEFAULT_CODE]]
    integer, optional, intent(in) :: code
    !> exception message  
    !> default: [[exception_handling_configuration(module):EXCEPTION_DEFAULT_MESSAGE]]
    character(len=*), optional, intent(in) :: message
    !> output configuration
    type(ExceptionOutputConfiguration), optional, intent(in) :: config
    !> exception
    type(Exception) :: self
  
    character(len=*), parameter :: PROCEDURE_NAME = 'new_exception'

    self%class = exception_class

    self%message = EXCEPTION_DEFAULT_MESSAGE
    if (present(message)) self%message = trim( adjustl( message ) )
    if (present(code)) self%code = code

    self%throw_trace = UNKNOWN_TRACE
    self%catch_trace = UNKNOWN_TRACE
  end function new_exception

  !> Tell where exception is thrown.
  pure subroutine throw( self, handler, trace )
    !> exception
    class(Exception), intent(inout) :: self
    !> name of exception handler that threw this exception
    character(len=*), intent(in) :: handler
    !> trace leading to place where exception is thrown  
    !> default: unknown trace
    type(ProcedureTrace), optional, intent(in) :: trace
  
    character(len=*), parameter :: PROCEDURE_NAME = 'throw'

    self%handler_name = handler
    if (present(trace)) self%throw_trace = trace
  end subroutine throw

  !> Tell where exception is caught.
  pure subroutine catch( self, handler, trace )
    !> exception
    class(Exception), intent(inout) :: self
    !> name of exception handler that threw this exception
    character(len=*), intent(in) :: handler
    !> trace leading to place where exception is caught  
    !> default: unknown trace
    type(ProcedureTrace), optional, intent(in) :: trace
  
    character(len=*), parameter :: PROCEDURE_NAME = 'catch'

    self%handler_name = handler
    if (present(trace)) self%catch_trace = trace
  end subroutine catch

  !> Write exception to all report units.
  subroutine report( self )
    use exception_handling_string_utils, only : indent, truncate_width
    !> exception
    class(Exception), intent(in) :: self
  
    character(len=*), parameter :: PROCEDURE_NAME = 'report'

    integer :: i
    integer, allocatable :: units(:)
    character(len=64) :: codestring
    character(len=:), allocatable :: string
    type(ExceptionOutputConfiguration) :: config

    config = self%class%get_output_config()
    write( codestring, '(i64)' ) self%code
    string = &
      self%class%get_name() // ' [' // trim( adjustl( self%handler_name ) ) // ']' &
        // new_line(string) // &
      indent( 'trace:    ', 2 ) // &
        adjustl( indent( truncate_width( trace_string( self%throw_trace, self%catch_trace, config%max_trace_lines ), &
          config%max_width-2-10 ), 2+10 ) ) // new_line(string) // &
      indent( 'code:     ', 2 ) // &
        trim( adjustl( codestring ) ) &
        // new_line(string) // &
      indent( 'message:  ', 2 ) // &
        adjustl( indent( truncate_width( trim( adjustl( self%message ) ), config%max_width-2-10 ), 2+10 ) ) &
        // new_line(string) 

    units = self%class%get_report_units()
    do i = 1, size( units )
      write( units(i), '(a)' ) string
    end do
  end subroutine report

  !> Create output string from catch and throw trace.
  pure function trace_string( throw, catch, max_num_lines ) result( string )
    !> trace leading to place exception was thrown
    type(ProcedureTrace), intent(in) :: throw
    !> trace leading to place exception was caught
    type(ProcedureTrace), intent(in) :: catch
    !> maximum number of lines
    integer, intent(in) :: max_num_lines
    !> output string
    character(len=:), allocatable :: string
  
    character(len=*), parameter :: PROCEDURE_NAME = 'trace_string'

    ! box drawing characters
    ! ╴ ╶ ─ ╵ ╷ │ ╌ ╎ ┄ ┆ ┈ ┊ ┌ ┐ └ ┘ ├ ┤ ┬ ┴ ┼

    integer :: i, nthrow, ncatch, ncommon, nstart, nend
    character(len=6) :: level
    character(len=:), allocatable :: athrow(:), acatch(:)

    ! find common trace
    athrow = throw%to_array(); nthrow = size( athrow )
    acatch = catch%to_array(); ncatch = size( acatch )
    ncommon = 0
    do while (ncommon < min(nthrow, ncatch))
      if (trim( adjustl( athrow(ncommon+1) ) ) /= trim( adjustl( acatch(ncommon+1) ) )) exit
      ncommon = ncommon + 1
    end do

    ! write trace string
    select case (max_num_lines)
    case (0)
      string = ''
    case (1)
      write( level, '(i4,": ")' ) nthrow
      string = level // trim( athrow(nthrow) )
    case (2)
      string = 'lv. 1 ' // trim( athrow(1) )
      write( level, '(i5,x)' ) nthrow
      string = string // new_line(string) // level // trim( athrow(nthrow) )
    case default
      if (nthrow <= max_num_lines .or. max_num_lines < 0) then
        ! full trace
        nstart = nthrow - 1
        nend = 0
      else
        ! split in middle
        nstart = 1 + (max_num_lines - 3) / 2
        nend = max_num_lines - 1 - nstart
      end if
      if (nthrow == 1) then
        string = 'lv. 1 ' // trim( athrow(1) )
      else
        string = 'lv. 1 ┬ ' // trim( athrow(1) )
      end if
      do i = 2, nstart
        write( level, '(i5,x)' ) i
        string = string // new_line(string) // level // repeat( ' ', i-2 ) // '└┬ ' // trim( athrow(i) )
      end do
      level = repeat( ' ', len(level) )
      if (nend > 0) string = string // new_line(string) // level // &
        repeat( ' ', nstart-1 ) // '╵' // repeat( '-', nthrow-nend-1-nstart ) // '╷'
      do i = nthrow - nend + 1, nthrow - 1
        write( level, '(i5,x)' ) i
        string = string // new_line(string) // level // repeat( ' ', i-2 ) // '└┬ ' // trim( athrow(i) )
      end do
      write( level, '(i5,x)' ) nthrow
      if (nthrow > 1) string = string // new_line(string) // level // repeat( ' ', nthrow-2 ) // '└─ ' // trim( athrow(nthrow) )
    end select

    ! add catch info
    if (ncatch > 0) then
      if (string /= '') string = string // new_line(string) 
      write( level, '(i6)' ) ncatch
      string = string // 'caught on level ' // trim( adjustl( level ) ) // ' in "' // trim( acatch(ncatch) ) // '"'
    end if
    if (ncommon > 0 .and. ncommon < ncatch) then
      write( level, '(i6)' ) ncommon
      string = string // ' on a branch that left the branch above on level ' // trim( adjustl( level ) ) &
        // ' in "' // trim( athrow(ncommon) ) // '"'
    end if
  end function trace_string

end module exception_handling_exception
