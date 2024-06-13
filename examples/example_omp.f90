module example_omp_module
  use exception_handling
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'example_omp_module'
  
  public :: serial_subroutine

contains

  subroutine serial_subroutine( i, eh )
    integer, intent(in) :: i
    type(ExceptionHandler), optional, intent(inout) :: eh
  
    character(len=*), parameter :: PROCEDURE_NAME = 'serial_subroutine'

    if (present(eh)) &
      call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME )

    if (mod(i, 2) /= 0) then
      if (present(eh)) then
        call eh%throw( ERROR_CLASS, code=i, &
          message='I do not like odd numbers!' )
      else
        error stop 'Sorry, but I have to stop here, because you do not like to handle exceptions.'
      end if
    end if

    if (present(eh)) &
      call eh%remove_from_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME )
  end subroutine serial_subroutine

end module example_omp_module

program example_omp
  use omp_lib, only : omp_get_thread_num
  use example_omp_module
  use exception_handling
  use exception_handling_string_utils, only : truncate_width
  implicit none

  character(len=*), parameter :: PROGRAM_NAME = 'example_omp'

  character(len=*), parameter :: DESCRIPTION = &
    'This example shows how to use exception handling in an OMP parallel region. Local copies of a global exception &
    handler are created on each thread. A serial subroutine with exception handling is called in a parallel do loop &
    and finally exceptions from all threads are gathered in the global exception handler and caught.' &
    // repeat( new_line('a'), 2 ) // &
    'The order in which exceptions are thrown depends on the order in which the individual threads merge into the global &
    handler and is not deterministic in this example.'

  ! local exception handling variables
  type(ExceptionHandler) :: eh, eh_local
  ! other local variables
  integer :: i

  write( *, '(a)' ) repeat( '=', 80 )
  write( *, '(a)' ) truncate_width( DESCRIPTION, 80 )
  write( *, '(a)' ) repeat( '=', 80 )

  eh = ExceptionHandler( 'my test handler' )
  call eh%add_to_trace( module=PROGRAM_NAME, procedure='(main program)' )

  !$omp parallel default(shared) private(eh_local)
  eh_local = eh%empty_copy( thread=omp_get_thread_num() )

  !$omp do
  do i = 1, 10
    call serial_subroutine( i, eh=eh_local )
  end do
  !$omp end do

  !$omp critical
  call eh%merge_handler( eh_local )
  !$omp end critical
  !$omp end parallel

  call eh%catch_and_report_all

  call eh%remove_from_trace( module=PROGRAM_NAME, procedure='(main program)' )
end program example_omp
