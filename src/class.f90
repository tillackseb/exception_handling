!> title:   Fortran exception classes
!> author:  Sebastian Tillack
!> version: v0.1
!> date:    June 2024
!> license: GPLv3
!
!> TODO add description
module exception_handling_exception_class
  use exception_handling_configuration
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'exception_handling_exception_class'

  !> Exception class object.
  type :: ExceptionClass
    private
    !> name
    character(len=CLASS_NAME_MAX_LENGTH) :: name = CLASS_NAME_DEFAULT
    !> units to which report exceptions of this class
    integer :: report_units(CLASS_MAX_NUM_UNITS) = CLASS_INVALID_UNIT
    !> output configuration
    type(ExceptionOutputConfiguration) :: output_config = DEFAULT_OUTPUT_CONFIG
  contains
    !> get exception class name
    procedure :: get_name
    !> get all report units
    procedure :: get_report_units
    !> get output configuration
    procedure :: get_output_config
    !> add report units
    procedure :: add_report_units
    !> remove report units
    procedure :: remove_report_units
    !> configure output format
    procedure :: configure_output
    ! comparison
    procedure, private, pass(lhs) :: is_equal_to
    procedure, private, pass(lhs) :: is_not_equal_to
    generic :: operator(==) => is_equal_to
    generic :: operator(/=) => is_not_equal_to
  end type ExceptionClass

  !> Constructor.
  interface ExceptionClass
    module procedure :: new_exception_class
  end interface ExceptionClass

  ! module ExceptionClass type constants
  !> constant exception class representing no exceptions
  type(ExceptionClass), public, parameter :: NO_EXCEPTION_CLASS &
    = ExceptionClass( name='NO EXCEPTION' )
  !> constant exception class for errors
  type(ExceptionClass), public, parameter :: ERROR_CLASS &
    = ExceptionClass( name='ERROR', report_units=[DEFAULT_ERROR_UNIT, spread(CLASS_INVALID_UNIT, 1, CLASS_MAX_NUM_UNITS-1)] )
  !> constant exception class for warnings
  type(ExceptionClass), public, parameter :: WARNING_CLASS &
    = ExceptionClass( name='WARNING', report_units=[DEFAULT_WARNING_UNIT, spread(CLASS_INVALID_UNIT, 1, CLASS_MAX_NUM_UNITS-1)] )
  
  public :: ExceptionClass

contains

  !> Exception class constructor.
  pure function new_exception_class( class_name, report_units ) result( self )
    !> exception class name
    character(len=*), intent(in) :: class_name
    !> report units
    integer, optional, intent(in) :: report_units(:)
    !> exception class
    type(ExceptionClass) :: self
  
    character(len=*), parameter :: PROCEDURE_NAME = 'new_exception_class'

    self%name = trim( adjustl( class_name ) )
    if (present(report_units)) call self%add_report_units( report_units )
  end function new_exception_class

  !> Get name of exception class
  pure function get_name( self ) result( name )
    !> exception class
    class(ExceptionClass), intent(in) :: self
    !> name
    character(len=:), allocatable :: name
  
    character(len=*), parameter :: PROCEDURE_NAME = 'get_name'
    
    name = trim( adjustl( self%name ) )
  end function get_name

  !> Get all report units of exception class.
  pure function get_report_units( self ) result( units )
    !> exception class
    class(ExceptionClass), intent(in) :: self
    !> units
    integer, allocatable :: units(:)
  
    character(len=*), parameter :: PROCEDURE_NAME = 'get_report_units'

    integer :: n

    n = count( self%report_units /= CLASS_INVALID_UNIT )
    if (n > 0) then
      units = self%report_units(:n)
    else
      allocate( units(0) )
    end if
  end function get_report_units

  !> Get output configuration.
  pure function get_output_config( self ) result( config )
    !> exception class
    class(ExceptionClass), intent(in) :: self
    !> output configuration
    type(ExceptionOutputConfiguration) :: config
  
    character(len=*), parameter :: PROCEDURE_NAME = 'get_output_config'
    
    config = self%output_config
  end function get_output_config

  !> Add report units to exception class.
  pure subroutine add_report_units( self, units )
    !> exception class
    class(ExceptionClass), intent(inout) :: self
    !> report units
    integer, intent(in) :: units(:)
  
    character(len=*), parameter :: PROCEDURE_NAME = 'add_report_units'

    integer :: i, n_old, n_add

    n_old = count( self%report_units /= CLASS_INVALID_UNIT )
    n_add = count( [(all( self%report_units /= units(i) ), i=1, size(units))] )
    if (n_old + n_add > CLASS_MAX_NUM_UNITS) &
      error stop &
        MODULE_NAME // ' % ' // PROCEDURE_NAME // ': &
        Maximum number of report units exeeded. Consider increasing CLASS_MAX_NUM_UNITS.'
    do i = 1, size( units )
      if (any( self%report_units == units(i) )) cycle
      n_old = n_old + 1
      self%report_units(n_old) = units(i)
    end do
  end subroutine add_report_units

  !> Remove report units from exception class.
  pure subroutine remove_report_units( self, units )
    !> exception class
    class(ExceptionClass), intent(inout) :: self
    !> report units
    integer, intent(in) :: units(:)
  
    character(len=*), parameter :: PROCEDURE_NAME = 'remove_report_units'

    integer :: i, j, n

    n = size( self%report_units )
    do i = 1, size( units )
      j = findloc( self%report_units, units(i), dim=1 )
      if (j == 0) cycle
      if (j < n) self%report_units(j:n-1) = self%report_units(j+1:n)
      self%report_units(n) = CLASS_INVALID_UNIT 
    end do
  end subroutine remove_report_units

  !> Configure output format.
  pure subroutine configure_output( self, max_width, max_trace_lines )
    !> exception class
    class(ExceptionClass), intent(inout) :: self
    !> see [[exception_handling_configuration(module):ExceptionOutputConfiguration(type)]]
    integer, optional, intent(in) :: max_width
    !> see [[exception_handling_configuration(module):ExceptionOutputConfiguration(type)]]
    integer, optional, intent(in) :: max_trace_lines
  
    character(len=*), parameter :: PROCEDURE_NAME = 'configure_output'

    if (present(max_width)) self%output_config%max_width = max_width
    if (present(max_trace_lines)) self%output_config%max_trace_lines = max_trace_lines
  end subroutine configure_output

  !> Check if two [[ExceptionClass(type)]] objects are equal.
  elemental pure function is_equal_to( lhs, rhs ) result( is_equal )
    !> exception classes to compare
    class(ExceptionClass), intent(in) :: lhs, rhs
    !> comparison result
    logical :: is_equal
  
    character(len=*), parameter :: PROCEDURE_NAME = 'is_equal_to'

    is_equal = (lhs%name == rhs%name)
  end function is_equal_to

  !> Check if two [[ExceptionClass(type)]] objects are not equal.
  elemental pure function is_not_equal_to( lhs, rhs ) result( is_not_equal )
    !> exception classes to compare
    class(ExceptionClass), intent(in) :: lhs, rhs
    !> comparison result
    logical :: is_not_equal
  
    character(len=*), parameter :: PROCEDURE_NAME = 'is_not_equal_to'

    is_not_equal = .not. lhs%is_equal_to( rhs )
  end function is_not_equal_to

end module exception_handling_exception_class
