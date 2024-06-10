!> description
module minimal
  !uses
  implicit none
  private

  character(*), parameter :: MODULE_NAME = 'minimal'

  !> description
  type :: MinimalTrace
    private
    character(:), allocatable :: trace
  end type MinimalTrace
  !> description
  interface MinimalTrace
    module procedure :: new_minimal_trace
  end interface MinimalTrace

  !> description
  type :: MinimalClass
    private
    character(32) :: name
    integer, allocatable :: report_units(:)
  end type MinimalClass

  !> description
  type :: MinimalException
    private
    type(MinimalClass) :: class
    integer :: code
    character(:), allocatable :: message
    type(MinimalTrace) :: trace
  end type MinimalException

  !> description
  type :: MinimalHandler
    private
    character(64) :: name
    type(MinimalException), allocatable :: exceptions(:)
    !type(MinimalTrace) :: trace
    !integer :: thread = -1
  end type MinimalHandler
  !> description
  interface MinimalHandler
    module procedure :: new_minimal_handler
  end interface MinimalHandler
  
  public :: MinimalHandler

contains

  !> description
  pure function new_minimal_handler( name ) result( self )
    character(*), intent(in) :: name
    type(MinimalHandler) :: self
  
    character(*), parameter :: PROCEDURE_NAME = 'new_minimal_handler'

    self%name = name
    !self%trace = MinimalTrace( 'test trace' )
  end function new_minimal_handler

  !> description
  pure function new_minimal_trace( trace ) result( self )
    character(*), intent(in) :: trace
    type(MinimalTrace) :: self
  
    character(*), parameter :: PROCEDURE_NAME = 'new_minimal_trace'

    self%trace = trace
  end function new_minimal_trace

end module minimal
