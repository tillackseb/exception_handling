!> Example use case for exception handling.
module example
  use exception_handling
  use another_module
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'example'

  public :: example_omp, example_recursive
  
contains

  !> Example for recommended usage of exception handling in OMP threaded environment.
  subroutine example_omp
    use omp_lib, only : omp_get_thread_num
    character(len=*), parameter :: PROCEDURE_NAME = 'example_omp'

    ! local exception handling variables
    type(ExceptionHandler) :: eh, eh_thread
    type(Exception) :: e
    type(ProcedureTrace) :: tr, tr_thread
    ! other local variables
    integer :: i

    ! create exception handler
    eh = ExceptionHandler( 'my test handler', max_width=100 )

    ! ALLWAYS add procedure to trace at the beginning!
    call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )

    ! call a subroutine that allows for exception handling
    call any_subroutine( eh=eh )

    ! open a threaded region
    !$omp parallel default(shared) private(eh_thread, tr_thread)
    
    ! create thread private copy of exception handler with empty stack
    eh_thread = eh%empty_copy( thread=omp_get_thread_num() )

    ! perform some parallel execution with exception handling
    !$omp do
    do i = 1, 5
      ! remember to pass thread private exception handler
      call another_subroutine( i, eh=eh_thread )
    end do
    !$omp end do

    ! merge thread private copies with shared exception handler
    ! do this in a thread save environment
    !$omp critical
    call eh%merge_handler( eh_thread )
    !$omp end critical

    !$omp end parallel

    ! catch exceptions
    do while (eh%has_uncaught_exceptions())
      ! don't forget to give a trace for more information
      call eh%catch( e, trace=tr )

      ! do things depending on the exception class or code
      ! (e.g. finalization, deallocation, file writes, ...)

      ! report exception to respective report units
      call e%report
    end do

    ! ALLWAYS remove procedure from trace at the end and before return!
    call eh%remove_from_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
  end subroutine example_omp

  !> description
  subroutine example_recursive
    character(len=*), parameter :: PROCEDURE_NAME = 'example_recursive'

    ! local exception handling variables
    type(ExceptionHandler) :: eh
    type(Exception) :: e
    type(ProcedureTrace) :: tr
    ! other local variables
    integer :: i

    eh = ExceptionHandler( 'my test handler', max_width=100, max_trace_lines=5 )
    call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )

    i = 1
    call recursive_subroutine( i, eh=eh )

    do while (eh%has_uncaught_exceptions())
      call eh%catch( e, trace=tr )
      call e%report
    end do

    call eh%remove_from_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
  end subroutine example_recursive

  !> description
  pure subroutine any_subroutine( eh )
    !> exception handler
    type(ExceptionHandler), optional, intent(inout) :: eh    
  
    character(len=*), parameter :: PROCEDURE_NAME = 'any_subroutine'

    type(ProcedureTrace) :: tr

    ! ALLWAYS add procedure to trace at the beginning!
    if (present(eh)) &
      call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )

    ! do stuff here

    ! check for errors
    if (present(eh)) then
      ! A mild exception occured.
      ! A warning is sufficient.
      call eh%throw( WARNING_CLASS, trace=tr, &
        code=42, &
        message='No worries! Not optimal but could have been worse. ;-)' )
    else
      ! do alternative handling here
    end if
      
    ! ALLWAYS remove procedure from trace at the end end before return!
    if (present(eh)) &
      call eh%remove_from_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
  end subroutine any_subroutine

  !> Create deep and branching trace with recursion.
  recursive subroutine recursive_subroutine( i, eh )
    !> some input parameter
    integer, intent(inout) :: i
    !> exception handler
    type(ExceptionHandler), optional, intent(inout) :: eh
  
    character(len=*), parameter :: PROCEDURE_NAME = 'recursive_subroutine'

    type(ProcedureTrace) :: tr
    type(Exception) :: e
    character(len=1) :: level

    write( level, '(i1)' ) i

    if (present(eh)) &
      call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME//'('//level//')', trace=tr )

    if (i >= 8) then
      if (eh%has_uncaught_exceptions()) then
        call eh%catch( e, trace=tr )
        call e%report
      else
        call eh%throw( ERROR_CLASS, trace=tr, &
          code=i, message='i is too large' )
      end if
    else
      i = i + 1 
      call recursive_subroutine( i, eh )
      i = i - 1 
    end if
    if (i == 4 .and. eh%has_uncaught_exceptions()) then
      call recursive_subroutine( i, eh )
    end if

    if (present(eh)) &
      call eh%remove_from_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME//'('//level//')', trace=tr )
  end subroutine recursive_subroutine

end module example
