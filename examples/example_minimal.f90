module cool_library
  implicit none
  
contains

  subroutine real_sqrt( a, b, eh )
    use exception_handling
    real, intent(in) :: a
    real, intent(out) :: b
    type(ExceptionHandler), intent(inout) :: eh

    call eh%add_to_trace( module='cool_library', procedure='real_sqrt' )

    if (a < 0.0) then
      call eh%throw( ERROR_CLASS, code=42, &
        message='Input parameter `a` must not be negative.', &
        info='Result `b` may be undefined.' )
    else
      b = sqrt( a )
    end if

    call eh%remove_from_trace( module='cool_library', procedure='real_sqrt' )
  end subroutine real_sqrt

end module cool_library

program example_minimal
  use cool_library, only : real_sqrt
  use exception_handling
  use exception_handling_string_utils, only : truncate_width
  implicit none

  character(len=*), parameter :: PROGRAM_NAME = 'example_minimal'

  character(len=*), parameter :: DESCRIPTION = &
    'This minimal example demonstrates how to create an exception handler, how to throw and catch exceptions and &
    how to keep track of traces.'

  type(ExceptionHandler) :: eh
  type(Exception) :: e

  real :: a, b

  write( *, '(a)' ) repeat( '=', 80 )
  write( *, '(a)' ) truncate_width( DESCRIPTION, 80 )
  write( *, '(a)' ) repeat( '=', 80 )

  eh = ExceptionHandler( 'my test handler' )
  call eh%add_to_trace( module=PROGRAM_NAME, procedure='(main program)' )

  write( *, '(a)', advance='no' ) 'Enter a real number: '
  read( *, * ) a
  call real_sqrt( a, b, eh )

  do while (eh%has_uncaught_exceptions())
    call eh%catch( e )
    call e%report
    error stop
  end do

  write( *, '(a,f10.6,a,f10.6)' ) 'The square root of', a, ' is', b

  call eh%remove_from_trace( module=PROGRAM_NAME, procedure='(main program)' )
end program example_minimal
