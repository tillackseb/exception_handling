module cool_library
  implicit none
  
contains

  subroutine real_sqrt( a, b, eh )
    use exception_handling
    real, intent(in) :: a
    real, intent(out) :: b
    type(ExceptionHandler), intent(inout) :: eh

    type(ProcedureTrace) :: tr

    call eh%add_to_trace( module='cool_library', procedure='real_sqrt', trace=tr )

    if (a < 0.0) then
      call eh%throw( ERROR_CLASS, trace=tr, &
        code=42, &
        message='Input parameter `a` must not be negative.', &
        info='Result may be undefined.' )
    else
      b = sqrt( a )
    end if

    call eh%remove_from_trace( module='cool_library', procedure='real_sqrt', trace=tr )
  end subroutine real_sqrt

end module cool_library

program minimal_example
  use exception_handling
  use cool_library, only : real_sqrt
  implicit none

  type(ExceptionHandler) :: eh
  type(ProcedureTrace) :: tr
  type(Exception) :: e

  real :: a, b

  eh = ExceptionHandler( 'my exception handler' )
  call eh%add_to_trace( module='minimal_handling', procedure='(main program)', trace=tr )

  write( *, '(a)', advance='no' ) 'Enter a real number: '
  read( *, * ) a
  call real_sqrt( a, b, eh )

  do while (eh%has_uncaught_exceptions())
    call eh%catch( e, trace=tr )
    call e%report
    error stop
  end do

  write( *, '(a,f10.6,a,f10.6)' ) 'The square root of', a, ' is', b

  call eh%remove_from_trace( module='minimal_handling', procedure='(main program)', trace=tr )
end program minimal_example
