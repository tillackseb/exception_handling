module example_recursive_module
  use exception_handling
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'example_recursive_module'
  
  public :: recursive_subroutine

contains

  recursive subroutine recursive_subroutine( i, eh )
    integer, intent(inout) :: i
    type(ExceptionHandler), optional, intent(inout) :: eh
  
    character(len=*), parameter :: PROCEDURE_NAME = 'recursive_subroutine'
    integer, parameter :: depth = 10

    ! local exception handling variables
    type(Exception) :: e
    ! other local variables
    character(len=3) :: level

    write( level, '(i3)' ) i

    if (present(eh)) &
      call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME//'('//trim(adjustl(level))//')' )

    if (i == depth .and. .not. eh%has_uncaught_exceptions()) then
      call eh%throw( ERROR_CLASS, code=i, &
        message='Maximum value for `i` reached.' )
      call eh%throw( WARNING_CLASS, code=i, &
        message='Reached 1st turning point. Counting down again.' )
    else if (i == 2*depth) then
      call eh%catch( e, class=ERROR_CLASS )
      call e%report
      call eh%throw( WARNING_CLASS, code=i, &
        message='Reached 3rd turning point. Counting down again.' )
    else
      i = i + 1 
      call recursive_subroutine( i, eh )
      i = i - 1 
    end if
    if (i == depth/2 .and. eh%has_uncaught_exceptions( class=ERROR_CLASS )) then
      call eh%throw( WARNING_CLASS, code=i, &
        message='Reached 2nd turning point. Counting up again.' )
      call recursive_subroutine( i, eh )
    end if

    if (present(eh)) &
      call eh%remove_from_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME//'('//trim(adjustl(level))//')' )
  end subroutine recursive_subroutine

end module example_recursive_module

program example_recursive
  use example_recursive_module
  use exception_handling
  use exception_handling_string_utils, only : truncate_width
  implicit none

  character(len=*), parameter :: PROGRAM_NAME = 'example_recursive'

  character(len=*), parameter :: DESCRIPTION = &
    'This examples uses a recursive subroutine and an exception handler to count from 1 to 10 and throws an error. &
    Then, it counts down to 5, turns and counts up again until it reaches 20 where the error is cought. It turns again &
    counting down back to 1. At each turning point in the counting direction a warning is thrown.' &
    // repeat( new_line('a'), 2 ) // &
    'The traces in this example are quite long. Vary the parameters `max_width` and `max_trace_lines` to see their &
    effect on the report output.'

  ! local exception handling variables
  type(ExceptionHandler) :: eh
  type(Exception) :: e
  ! input
  integer :: max_width, max_trace_lines
  ! other local variables
  integer :: i

  write( *, '(a)' ) repeat( '=', 80 )
  write( *, '(a)' ) truncate_width( DESCRIPTION, 80 )
  write( *, '(a)' ) repeat( '=', 80 )

  write( *, '(a)', advance='no' ) 'max_width = '
  read( *, * ) max_width
  write( *, '(a)', advance='no' ) 'max_trace_lines = '
  read( *, * ) max_trace_lines
  write( *, '(a)' ) repeat( '-', 80 )

  eh = ExceptionHandler( 'my test handler' )
  call eh%configure_output( max_width=max_width, max_trace_lines=max_trace_lines )
  call eh%add_to_trace( module=PROGRAM_NAME, procedure='(main program)' )

  i = 1
  call recursive_subroutine( i, eh=eh )

  call eh%catch_and_report_all( class=WARNING_CLASS )
  
  print *, 'Still uncaught errors?', eh%has_uncaught_exceptions( ERROR_CLASS )

  call eh%remove_from_trace( module=PROGRAM_NAME, procedure='(main program)' )
end program example_recursive
