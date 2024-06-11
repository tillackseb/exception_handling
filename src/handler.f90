!> title:   Fortran exception handler
!> author:  Sebastian Tillack
!> version: v0.1
!> date:    June 2024
!> license: GPLv3
!
!> TODO add description
module exception_handling_exception_handler
  use, intrinsic :: iso_fortran_env, only : output_unit, error_unit
  use exception_handling_configuration
  use exception_handling_exception_class, only : ExceptionClass, ERROR_CLASS, WARNING_CLASS
  use exception_handling_exception, only : Exception
  use exception_handling_trace, only : ProcedureTrace
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'exception_handling_exception_handler'

  !> Exception handler.
  type :: ExceptionHandler
    private
    !> name of handler
    character(len=HANDLER_NAME_MAX_LENGTH) :: name = HANDLER_NAME_DEFAULT
    !> exception classes this handler can handle
    type(ExceptionClass), allocatable :: classes(:)
    !> number of exceptions in exception stack
    integer :: exception_count = 0
    !> stack of uncaught exceptions
    type(Exception), allocatable :: exceptions(:)
    !> trace to current state of exception handler
    type(ProcedureTrace) :: trace
    !> index of thread in (shared memory) threaded environment
    integer :: thread = -1
    !> index of rank in (distributed memory) parallel environment
    integer :: rank = -1
    !> output configuration
    type(ExceptionOutputConfiguration) :: config
  contains
    !> throw exception
    procedure :: throw
    !> catch last exception in stack
    procedure :: catch
    !> generic assertion (raises exception if failed)
    !procedure :: assert
    !> procedure argument assertion (raises exception if failed)
    !procedure, public :: assert_argument
    !> raise warning
    !procedure, public :: warn
    !> add level to trace
    procedure :: add_to_trace
    !> remove last level from trace
    procedure :: remove_from_trace
    !> handler has uncaught exceptions
    procedure :: has_uncaught_exceptions
    !> add new exception class
    procedure :: add_exception_class
    !> add report units to given exception class
    procedure :: add_report_units
    !> remove report units from given exception class
    procedure :: remove_report_units
    !> create copy with empty exception stack
    procedure :: empty_copy
    !> merge exceptions from another exception handler
    procedure :: merge_handler
  end type ExceptionHandler

  !> Constructor.
  interface ExceptionHandler
    module procedure :: new_exception_handler
  end interface ExceptionHandler

  public :: Exception, ExceptionClass, ExceptionHandler

contains

  !> Exception handler constructor.
  pure function new_exception_handler( name, thread, rank, max_width, max_trace_lines, default_units ) result( self )
    !> custom name for exception handler
    character(len=*), intent(in) :: name
    !> index of thread in (shared memory) threaded environment   
    !> default: -1 (no thread information)
    integer, optional, intent(in) :: thread
    !> index of rank in (distributed memory) parallel environment   
    !> default: -1 (no rank information)
    integer, optional, intent(in) :: rank
    !> maximum column width of output   
    !> default: -1 (no maximum width)
    integer, optional, intent(in) :: max_width
    !> maximum number of lines in trace output  
    !> default: -1 (full trace)
    integer, optional, intent(in) :: max_trace_lines
    !> initialize with default report units
    !> [[exception_handling_configuration(module):DEFAULT_ERROR_UNIT]] and 
    !> [[exception_handling_configuration(module):DEFAULT_WARNING_UNIT]]   
    !> default: True
    logical, optional, intent(in) :: default_units
    !> exception handler
    type(ExceptionHandler) :: self
  
    character(len=*), parameter :: PROCEDURE_NAME = 'new_exception_handler'

    logical :: defun

    defun = .true.
    if (present(default_units)) defun = default_units

    ! assign values
    self%name = name
    if (present(thread)) self%thread = thread
    if (present(rank)) self%rank = rank
    if (present(max_width)) self%config%max_width = max_width
    if (present(max_trace_lines)) self%config%max_trace_lines = max_trace_lines
    ! add default exception classes
    call self%add_exception_class( ERROR_CLASS )
    call self%add_exception_class( WARNING_CLASS )
    ! remove default units
    if (.not. defun) then
      call self%remove_report_units( ERROR_CLASS, [DEFAULT_ERROR_UNIT] )
      call self%remove_report_units( WARNING_CLASS, [DEFAULT_WARNING_UNIT] )
    end if
    ! initialize exception stack
    call append_entries( self%exceptions )
    ! initialize trace
    self%trace = ProcedureTrace()
  end function new_exception_handler

  !> Throw an exception, i.e., add new exception to stack of uncaught exceptions.
  pure subroutine throw( self, class, code, message, info, trace, add_class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> exception class
    type(ExceptionClass), intent(in) :: class
    !> optional custom exception code   
    !> default: [[exception_handling_configuration(module):EXCEPTION_DEFAULT_CODE]]
    integer, optional, intent(in) :: code
    !> custom message   
    !> default: [[exception_handling_configuration(module):EXCEPTION_DEFAULT_MESSAGE]]
    character(len=*), optional, intent(in) :: message
    !> optional additional information   
    !> default: none
    character(len=*), optional, intent(in) :: info
    !> trace leading to place exception is thrown  
    !> default: empty trace
    type(ProcedureTrace), optional, intent(in) :: trace
    !> add exception class to exception handler if not yet contained   
    !> default: True
    logical, optional, intent(in) :: add_class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'throw'

    integer :: i
    logical :: add
    character(len=:), allocatable :: string

    add = .true.
    if (present(add_class)) add = add_class

    ! add class
    if (add .and. all( self%classes /= class )) call self%add_exception_class( class )
    ! find class in handler's class list
    i = findloc( self%classes == class, .true., dim=1 )
    if (i == 0) return
    ! compose message
    string = ''
    if (present(message)) string = message
    if (present(info)) string = string // new_line('a') // '(' // trim( adjustl( info ) ) // ')' 
    ! add exception
    self%exception_count = self%exception_count + 1
    if (self%exception_count > size(self%exceptions)) call append_entries( self%exceptions )
    if (string /= '') then
      self%exceptions(self%exception_count) = &
        Exception( self%classes(i), code=code, message=string, config=self%config )
    else
      self%exceptions(self%exception_count) = &
        Exception( self%classes(i), code=code, config=self%config )
    end if
    call self%exceptions(self%exception_count)%throw( trim(self%name), trace=trace )
  end subroutine throw

  !> Catch first exception and remove it from stack.
  pure subroutine catch( self, e, trace )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> cought exception
    type(Exception), intent(out) :: e
    !> trace leading to place exception is cought  
    !> default: empty trace
    type(ProcedureTrace), optional, intent(in) :: trace
  
    character(len=*), parameter :: PROCEDURE_NAME = 'catch'

    e = self%exceptions(1)
    self%exception_count = self%exception_count - 1
    self%exceptions = cshift( self%exceptions, 1 )
    call e%catch( trim(self%name), trace=trace )
  end subroutine catch

  !> Add level to handler's trace.
  pure subroutine add_to_trace( self, module, procedure, trace )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> name of module
    character(len=*), intent(in) :: module
    !> name or procedure
    character(len=*), intent(in) :: procedure
    !> copy of handler's current trace
    type(ProcedureTrace), intent(out) :: trace
  
    character(len=*), parameter :: PROCEDURE_NAME = 'add_to_trace'

    call self%trace%add( location_string( module, procedure, self%thread ))
    trace = self%trace
  end subroutine add_to_trace

  !> Remove last level from handler's trace.
  pure subroutine remove_from_trace( self, module, procedure, trace )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> name of module
    character(len=*), intent(in) :: module
    !> name or procedure
    character(len=*), intent(in) :: procedure
    !> new state of handler's trace
    type(ProcedureTrace), intent(out) :: trace
  
    character(len=*), parameter :: PROCEDURE_NAME = 'remove_from_trace'

    character(len=:), allocatable :: level

    call self%trace%pop( level )
    if (level /= location_string( module, procedure, self%thread )) then
      self%trace = ProcedureTrace()
    end if
    trace = self%trace
  end subroutine remove_from_trace
  
  !> Check if exception handler has uncaught exceptions in stack.
  pure function has_uncaught_exceptions( self ) result( l )
    !> exception handler
    class(ExceptionHandler), intent(in) :: self
    !> check result
    logical :: l
  
    character(len=*), parameter :: PROCEDURE_NAME = 'has_uncaught_exceptions'

    l = (self%exception_count > 0)
  end function has_uncaught_exceptions

  !> Add new exception class to exception handler.
  pure subroutine add_exception_class( self, class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> exception class to add
    type(ExceptionClass), intent(in) :: class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'add_exception_class'

    if (allocated(self%classes)) then
      if (all( self%classes /= class )) self%classes = [self%classes, class]
    else
      self%classes = [class]
    end if
  end subroutine add_exception_class

  !> Add report units to given exception class.
  pure subroutine add_report_units( self, class, units, add_class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> exception class to which to add report units
    type(ExceptionClass), intent(in) :: class
    !> report units to add
    integer, intent(in) :: units(:)
    !> add class if not yet in exception handler   
    !> default: True
    logical, optional, intent(in) :: add_class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'add_report_units'

    logical :: add
    integer :: i, j

    add = .true.
    if (present(add_class)) add = add_class

    ! add units to existing class
    do i = 1, size( self%classes )
      if (self%classes(i) /= class) cycle
      call self%classes(i)%add_report_units( units )
      exit
    end do
    ! add class and units if not existent
    if (i > size(self%classes) .and. add) then
      call self%add_exception_class( class )
      call self%classes(i)%add_report_units( units )
    end if
  end subroutine add_report_units

  !> Remove report units from given exception class.
  pure subroutine remove_report_units( self, class, units, add_class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> exception class from which to remove report units
    type(ExceptionClass), intent(in) :: class
    !> report units to remove
    integer, intent(in) :: units(:)
    !> add class if not yet in exception handler   
    !> default: True
    logical, optional, intent(in) :: add_class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'remove_report_units'

    logical :: add
    integer :: i, j

    add = .true.
    if (present(add_class)) add = add_class

    ! add units to existing class
    do i = 1, size( self%classes )
      if (self%classes(i) /= class) cycle
      call self%classes(i)%remove_report_units( units )
      exit
    end do
    ! add class and units if not existent
    if (i > size(self%classes) .and. add) then
      call self%add_exception_class( class )
      call self%classes(i)%remove_report_units( units )
    end if
  end subroutine remove_report_units

  !> Make copy of exception handler with empty exception stack.
  pure function empty_copy( self, thread ) result( copy )
    !> exception handler
    class(ExceptionHandler), intent(in) :: self
    !> index of thread in (shared memory) threaded environment   
    !> default: -1 (no thread information)
    integer, optional, intent(in) :: thread
    !> copy
    type(ExceptionHandler) :: copy
  
    character(len=*), parameter :: PROCEDURE_NAME = 'empty_copy'

    integer :: i

    copy = ExceptionHandler( self%name, default_units=.false., thread=thread, &
      max_width=self%config%max_width, &
      max_trace_lines=self%config%max_trace_lines )
    copy%rank = self%rank
    copy%trace = self%trace
    do i = 1, size( self%classes )
      call copy%add_exception_class( self%classes(i) )
      call copy%add_report_units( self%classes(i), self%classes(i)%get_report_units() )
    end do
  end function empty_copy

  !> Merge exceptions from another exception handler into this handler's exception stack.  
  !> Classes from other handler are added to this handler is necessary.
  pure subroutine merge_handler( self, other )
    !> this exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> other exception handler
    type(ExceptionHandler), intent(in) :: other
  
    character(len=*), parameter :: PROCEDURE_NAME = 'merge_handler'

    integer :: i

    call append_entries( self%exceptions, n=other%exception_count )
    do i = 1, other%exception_count
      self%exception_count = self%exception_count + 1
      if (all( self%classes /= other%exceptions(i)%class )) call self%add_exception_class( other%exceptions(i)%class )
      self%exceptions(self%exception_count) = other%exceptions(i)
    end do
  end subroutine merge_handler

  !-----------------------------------------------------------------------------
  ! helpers

  !> Append given number of [[Exception(type)]] entries to stack.
  pure subroutine append_entries( stack, n )
    !> description
    type(Exception), allocatable, intent(inout) :: stack(:)
    !> number of entries to append   
    !> default: [[exception_handling_configuration(module):HANDLER_NUM_APPEND]]
    integer, optional, intent(in) :: n
  
    character(len=*), parameter :: PROCEDURE_NAME = 'append_entries'

    integer :: nadd, old_size
    type(Exception), allocatable :: copy(:)

    nadd = HANDLER_NUM_APPEND
    if (present(n)) nadd = max(0, n)

    if (.not. allocated(stack)) allocate( stack(0) )
    old_size = size( stack )
    allocate( copy(old_size+nadd) )
    if (old_size > 0) then
      copy(:old_size) = stack
    end if
    call move_alloc( copy, stack )
  end subroutine append_entries

  !> Join module and procedure name to location string.
  pure function location_string( module, procedure, thread ) result( location )
    use exception_handling_string_utils, only : upper
    !> module name
    character(len=*), intent(in) :: module
    !> procedure name
    character(len=*), intent(in) :: procedure
    !> thread index
    integer, intent(in) :: thread
    !> location string
    character(len=:), allocatable :: location
  
    character(len=*), parameter :: PROCEDURE_NAME = 'location_string'

    character(len=6) :: string

    write( string, '(i6)' ) thread
    location = upper( trim( adjustl( module ) ) ) // ' % ' // upper( trim( adjustl( procedure ) ) )
    if (thread >= 0) location = location // ' (thread ' // trim( adjustl( string ) ) // ')' 
  end function location_string

end module exception_handling_exception_handler
