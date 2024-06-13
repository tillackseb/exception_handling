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
  use exception_handling_exception, only : Exception, NO_EXCEPTION
  use exception_handling_trace, only : ProcedureTrace, UNKNOWN_TRACE
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
    type(ExceptionOutputConfiguration) :: output_config
  contains
    !> throw exception
    procedure :: throw
    !> catch first exception in stack
    procedure :: catch
    !> catch and report first exception in stack
    procedure :: catch_and_report
    !> catch all exceptions in stack
    procedure :: catch_all
    !> catch and report all exceptions in stack
    procedure :: catch_and_report_all
    !> generic assertion (raises exception if failed)
    !procedure :: assert
    !> procedure argument assertion (raises exception if failed)
    !procedure, public :: assert_argument
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
    !> configure output format
    procedure :: configure_output
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
    if (present(max_width)) self%output_config%max_width = max_width
    if (present(max_trace_lines)) self%output_config%max_trace_lines = max_trace_lines
    ! add default exception classes
    call self%add_exception_class( ERROR_CLASS )
    call self%add_exception_class( WARNING_CLASS )
    ! remove default units
    if (.not. defun) then
      call self%remove_report_units( [DEFAULT_ERROR_UNIT], class=ERROR_CLASS )
      call self%remove_report_units(  [DEFAULT_WARNING_UNIT], class=WARNING_CLASS )
    end if
    ! initialize exception stack
    call append_entries( self%exceptions )
    ! initialize trace
    self%trace = ProcedureTrace()
  end function new_exception_handler

  !> Throw an exception, i.e., add new exception to stack of uncaught exceptions.
  pure subroutine throw( self, class, code, message, info, add_class )
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
    if (add) call self%add_exception_class( class )
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
        Exception( self%classes(i), code=code, message=string, config=self%output_config )
    else
      self%exceptions(self%exception_count) = &
        Exception( self%classes(i), code=code, config=self%output_config )
    end if
    call self%exceptions(self%exception_count)%throw( trim(self%name), trace=self%trace )
  end subroutine throw

  !> Catch first exception and remove it from stack.
  pure subroutine catch( self, e, class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> cought exception
    !> ([[exception_handling_exception(module):NO_EXCEPTION]] if handler has no exceptions)
    type(Exception), intent(out) :: e
    !> if present, first exception of this class will be caught
    type(ExceptionClass), optional, intent(in) :: class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'catch'

    integer :: i
    logical :: found

    found = .false.
    if (present(class)) then
      do i = 1, self%exception_count
        found = (self%exceptions(i)%class == class)
        if (.not. found) cycle
        e = self%exceptions(i)
        exit
      end do
    else
      i = 1
      found = self%has_uncaught_exceptions()
      if (found) e = self%exceptions(1)
    end if
    if (found) then
      self%exception_count = self%exception_count - 1
      self%exceptions(i:) = cshift( self%exceptions(i:), 1 )
      call e%catch( trim(self%name), trace=self%trace )
    else
      e = NO_EXCEPTION
    end if
  end subroutine catch

  !> Catch and report first exception and remove it from stack.
  subroutine catch_and_report( self, e, class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> cought exception
    !> ([[exception_handling_exception(module):NO_EXCEPTION]] if handler has no exceptions)
    type(Exception), optional, intent(out) :: e
    !> if present, first exception of this class will be caught
    type(ExceptionClass), optional, intent(in) :: class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'catch_and_report'

    type(Exception) :: e_local
    call self%catch( e_local, class=class )
    call e_local%report
    if (present(e)) e = e_local
  end subroutine catch_and_report

  !> Catch all exceptions and remove them from stack.
  pure subroutine catch_all( self, es, class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> cought exceptions
    !> (empty array if handler has no exceptions)
    type(Exception), allocatable, intent(out) :: es(:)
    !> if present, only exceptions of this class will be caught
    type(ExceptionClass), optional, intent(in) :: class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'catch_all'

    type(Exception) :: e
    integer :: i, n

    if (allocated(es)) deallocate( es )
    if (present(class)) then
      n = 0
      do i = 1, self%exception_count
        if (self%exceptions(i)%class /= class) cycle
        n = n + 1
      end do
    else
      n = self%exception_count
    end if
    allocate( es(n) )
    do i = 1, n
      call self%catch( e, class=class )
      es(i) = e
    end do
  end subroutine catch_all

  !> Catch all exceptions and remove them from stack.
  ! Note: This also exists, to keep `catch_all` pure.
  subroutine catch_and_report_all( self, es, class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> cought exceptions
    !> (empty array if handler has no exceptions)
    type(Exception), optional, allocatable, intent(out) :: es(:)
    !> if present, only exceptions of this class will be caught
    type(ExceptionClass), optional, intent(in) :: class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'catch_and_report_all'

    integer :: i
    type(Exception), allocatable :: es_local(:)
    call self%catch_all( es_local, class=class )
    do i = 1, size( es_local )
      call es_local(i)%report
    end do
    if (present(es)) es = es_local
  end subroutine catch_and_report_all

  !> Add level to handler's trace.
  pure subroutine add_to_trace( self, module, procedure )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> name of module
    character(len=*), intent(in) :: module
    !> name or procedure
    character(len=*), intent(in) :: procedure
  
    character(len=*), parameter :: PROCEDURE_NAME = 'add_to_trace'

    call self%trace%add( location_string( module, procedure, self%thread ))
  end subroutine add_to_trace

  !> Remove last level from handler's trace.
  pure subroutine remove_from_trace( self, module, procedure )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> name of module
    character(len=*), intent(in) :: module
    !> name or procedure
    character(len=*), intent(in) :: procedure
  
    character(len=*), parameter :: PROCEDURE_NAME = 'remove_from_trace'

    character(len=:), allocatable :: level, location

    call self%trace%pop( level )
    location = location_string( module, procedure, self%thread )
    if (level /= location) then
      call self%throw( WARNING_CLASS, &
        code=SELF_EXCEPTION_CODE, &
        message='Trace corruption deteced. Attempt to remove "'//location//'" from trace reported above failed. &
          Trace is unknown from now on.' )
      self%trace = UNKNOWN_TRACE
    end if
  end subroutine remove_from_trace
  
  !> Check if exception handler has uncaught exceptions in stack.
  pure function has_uncaught_exceptions( self, class ) result( l )
    !> exception handler
    class(ExceptionHandler), intent(in) :: self
    !> if present, only exceptions of this class are considered
    type(ExceptionClass), optional, intent(in) :: class
    !> check result
    logical :: l
  
    character(len=*), parameter :: PROCEDURE_NAME = 'has_uncaught_exceptions'

    integer :: i

    l = (self%exception_count > 0)
    if (present(class)) then
      do i = 1, self%exception_count
        l = (self%exceptions(i)%class == class)
        if (l) exit
      end do
    end if
  end function has_uncaught_exceptions

  !> Add new exception class to exception handler.
  pure subroutine add_exception_class( self, class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> exception class to add
    type(ExceptionClass), intent(in) :: class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'add_exception_class'

    if (get_class_index( self, class=class ) > 0) return
    if (allocated(self%classes)) then
      if (all( self%classes /= class )) self%classes = [self%classes, class]
    else
      self%classes = [class]
    end if
  end subroutine add_exception_class

  !> Add report units to all exception classes.
  pure subroutine add_report_units( self, units, class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> report units to add
    integer, intent(in) :: units(:)
    !> if present, units will be added to this class only
    type(ExceptionClass), optional, intent(in) :: class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'add_report_units'

    integer :: i, j, n

    if (present(class)) then
      n = 1
      j = get_class_index( self, class )
    else
      j = 1
      n = size( self%classes )
    end if

    do i = max(1, j), j+n-1
      call self%classes(i)%add_report_units( units )
    end do
  end subroutine add_report_units

  !> Remove report units from all exception class.
  pure subroutine remove_report_units( self, units, class )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> report units to remove
    integer, intent(in) :: units(:)
    !> if present, units will be removed from this class only
    type(ExceptionClass), optional, intent(in) :: class
  
    character(len=*), parameter :: PROCEDURE_NAME = 'remove_report_units'

    integer :: i, j, n

    if (present(class)) then
      n = 1
      j = get_class_index( self, class )
    else
      j = 1
      n = size( self%classes )
    end if

    do i = max(1, j), j+n-1
      call self%classes(i)%remove_report_units( units )
    end do
  end subroutine remove_report_units

  !> Configure output format.
  pure subroutine configure_output( self, class, max_width, max_trace_lines )
    !> exception handler
    class(ExceptionHandler), intent(inout) :: self
    !> if present, configuration will apply to this class only
    type(ExceptionClass), optional, intent(in) :: class
    !> see [[exception_handling_configuration(module):ExceptionOutputConfiguration(type)]]
    integer, optional, intent(in) :: max_width
    !> see [[exception_handling_configuration(module):ExceptionOutputConfiguration(type)]]
    integer, optional, intent(in) :: max_trace_lines
  
    character(len=*), parameter :: PROCEDURE_NAME = 'configure_output'

    integer :: i, j, n

    if (present(class)) then
      n = 1
      j = get_class_index( self, class )
    else
      j = 1
      n = size( self%classes )
    end if

    do i = max(1, j), j+n-1
      call self%classes(i)%configure_output( max_width=max_width, max_trace_lines=max_trace_lines )
    end do
  end subroutine configure_output

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
      max_width=self%output_config%max_width, &
      max_trace_lines=self%output_config%max_trace_lines )
    copy%rank = self%rank
    copy%trace = self%trace
    do i = 1, size( self%classes )
      call copy%add_exception_class( self%classes(i) )
      call copy%add_report_units( self%classes(i)%get_report_units(), class=self%classes(i) )
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

  !> Get index of class in exception handler's class list.
  pure function get_class_index( eh, class ) result( idx )
    !> exception handler
    type(ExceptionHandler), intent(in) :: eh
    !> exception class
    type(ExceptionClass), optional, intent(in) :: class
    !> class index
    integer :: idx
  
    character(len=*), parameter :: PROCEDURE_NAME = 'get_class_index'

    idx = 0
    if (.not. present(class) .or. .not. allocated(eh%classes)) return

    idx = findloc( eh%classes == class, .true., dim=1 )
  end function get_class_index

end module exception_handling_exception_handler
