!> TODO add description
module exception_handling_constants
  use, intrinsic :: iso_fortran_env, only : output_unit, error_unit
  implicit none
  private

  character(*), parameter :: MODULE_NAME = 'exception_handling_constants'

  ! general
  integer, public, parameter :: DEFAULT_ERROR_UNIT = error_unit ! default unit for error roport
  integer, public, parameter :: DEFAULT_WARNING_UNIT = output_unit ! default unit for warning report
  
  ! ProcedureTrace
  integer, public, parameter :: TRACE_MAX_LENGTH = 2048 ! maximum length trace list string
  character, public, parameter :: TRACE_DELIMITER = ',' ! delimiter in trace list string (forbidden in valid Fortran names)
  character, public, parameter :: TRACE_SPECIAL = '~' ! special character (forbidden in valid Fortran names)
  character(*), public, parameter :: TRACE_UNKNOWN = 'unknown_procedure' ! placeholder for unspecified trace entries

  ! ExceptionClass
  integer, public, parameter :: CLASS_NAME_MAX_LENGTH = 32 ! maximum length for exception class name
  character(*), public, parameter :: CLASS_NAME_DEFAULT = 'EXCEPTION' ! default name for exception class
  integer, public, parameter :: CLASS_MAX_NUM_UNITS = 10 ! maximum number of report units for exception classes
  integer, public, parameter :: CLASS_INVALID_UNIT = -1 ! value for invalid report units

  ! Exception
  integer, public, parameter :: EXCEPTION_DEFAULT_CODE = 1 ! default exception code
  integer, public, parameter :: EXCEPTION_MESSAGE_MAX_LENGTH = 2048 ! maximum length of exception message
  character(*), public, parameter :: EXCEPTION_DEFAULT_MESSAGE = & ! default exception message
    'Something unspecified happened. This is the default message.'

  ! ExceptionHandler
  integer, public, parameter :: HANDLER_NAME_MAX_LENGTH = 64 ! maximum length for exception handler name
  character(*), public, parameter :: HANDLER_NAME_DEFAULT = 'default exception handler' ! default name for exception class
  integer, public, parameter :: HANDLER_NUM_APPEND = 10 ! number of elements to add to stack if stack is full

  !> Container for output settings
  type :: ExceptionOutputConfiguration
    !> maximum column width of report output
    ! (negative for no limit)
    integer :: max_width = -1
    !> maximum number of lines in trace output
    ! (negative for no limit)
    integer :: max_trace_lines = -1
  end type ExceptionOutputConfiguration

  public :: ExceptionOutputConfiguration

contains

end module exception_handling_constants
