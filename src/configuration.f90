!> TODO add description
module exception_handling_configuration
  use, intrinsic :: iso_fortran_env, only : output_unit, error_unit
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'exception_handling_configuration'

  ! general
  integer, public, parameter :: DEFAULT_ERROR_UNIT = error_unit ! default unit for error roport
  integer, public, parameter :: DEFAULT_WARNING_UNIT = output_unit ! default unit for warning report
  integer, public, parameter :: SELF_EXCEPTION_CODE = 999 ! code of exceptions thrown by exception handling lib itself
  
  ! ProcedureTrace
  integer, public, parameter :: TRACE_MAX_LENGTH = 2048 ! maximum length trace list string
  character, public, parameter :: TRACE_DELIMITER = ',' ! delimiter in trace list string (forbidden in valid Fortran names)
  character, public, parameter :: TRACE_SPECIAL = '~' ! special character (forbidden in valid Fortran names)
  character(len=*), public, parameter :: TRACE_UNKNOWN = 'unknown_procedure' ! placeholder for unspecified trace entries

  ! ExceptionClass
  integer, public, parameter :: CLASS_NAME_MAX_LENGTH = 32 ! maximum length for exception class name
  character(len=*), public, parameter :: CLASS_NAME_DEFAULT = 'EXCEPTION' ! default name for exception class
  integer, public, parameter :: CLASS_MAX_NUM_UNITS = 10 ! maximum number of report units for exception classes
  integer, public, parameter :: CLASS_INVALID_UNIT = -1 ! value for invalid report units

  ! Exception
  integer, public, parameter :: EXCEPTION_DEFAULT_CODE = 1 ! default exception code
  integer, public, parameter :: EXCEPTION_MESSAGE_MAX_LENGTH = 2048 ! maximum length of exception message
  character(len=*), public, parameter :: EXCEPTION_MESSAGE_DEFAULT = & ! default exception message
    'Something unspecified happened. This is the default message.'
  integer, public, parameter :: EXCEPTION_FILENAME_MAX_LENGTH = 256 ! maximum length of file name
  character(len=*), public, parameter :: EXCEPTION_FILENAME_DEFAULT = 'unknwon' ! default name of file in which exception occurs
  character(len=*), public, parameter :: EXCEPTION_LINESTRING_DEFAULT = 'unknwon' ! default string to report in line number not known

  ! ExceptionHandler
  integer, public, parameter :: HANDLER_NAME_MAX_LENGTH = 64 ! maximum length for exception handler name
  character(len=*), public, parameter :: HANDLER_NAME_DEFAULT = 'default exception handler' ! default name for exception class
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

  ! module ExceptionOutputConfiguration type constants
  !> constant edefault output configuration
  type(ExceptionOutputConfiguration), public, parameter :: DEFAULT_OUTPUT_CONFIG &
    = ExceptionOutputConfiguration( max_width=-1, max_trace_lines=-1 )

  public :: ExceptionOutputConfiguration

contains

end module exception_handling_configuration
