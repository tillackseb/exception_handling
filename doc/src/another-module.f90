!> Dummy module.
module another_module
  use exception_handling
  use omp_lib
  implicit none
  private

  character(len=*), parameter :: MODULE_NAME = 'another_module'
  
  public :: another_subroutine
  
contains

  !> Another dummy subroutine.
  subroutine another_subroutine( i, eh )
    !> some input parameter
    integer, intent(in) :: i
    !> exception handler
    type(ExceptionHandler), optional, intent(inout) :: eh
  
    character(len=*), parameter :: PROCEDURE_NAME = 'another_subroutine'

    ! exception handling
    type(ProcedureTrace) :: tr
    type(Exception) :: e
    ! local variables
    logical :: check

    ! ALLWAYS add procedure to trace at the beginning!
    if (present(eh)) &
      call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
      
    ! do stuff here

    ! check for errors
    check = (i < 3)

    ! handle errors
    if (.not. check) then
      ! Oh, no! An error occured!
      if (present(eh)) then
        ! Throw an exception.
        call eh%throw( ERROR_CLASS, trace=tr, &
          code=i, &
          message='Catastrophic error! All panic!', &
          info='Not really. Just a test.' )
      else
        ! stop execution
        error stop 'Sorry, but I have to stop here. No exception handling.'
      end if
    else
      ! everything is fine, proceed
      call yet_another_subroutine( i, eh=eh )
      ! you can also catch an exception here
      if (eh%has_uncaught_exceptions()) then
        call eh%catch( e, trace=tr )
        call e%report
      end if
    end if
      
    ! ALLWAYS remove procedure from trace at the end and before return!
    if (present(eh)) & 
      call eh%remove_from_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
  end subroutine another_subroutine

  !> Just another dummy procedure.
  subroutine yet_another_subroutine( i, eh )
    !> some input parameter
    integer, intent(in) :: i
    !> exception handler
    type(ExceptionHandler), optional, intent(inout) :: eh
  
    character(len=*), parameter :: PROCEDURE_NAME = 'yet_another_subroutine'

    ! exception handling
    type(ProcedureTrace) :: tr
    ! local variables
    logical :: check

    ! ALLWAYS add procedure to trace at the beginning!
    if (present(eh)) &
      call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
      
    ! do stuff here

    ! check for errors
    check = (mod(i, 2) == 0)

    ! handle errors
    if (.not. check) then
      if (present(eh)) then
        ! Throw an exception.
        call eh%throw( WARNING_CLASS, trace=tr, &
          code=i, &
          message='Parameter `i` is not even.' )
      else
        print *, 'If this warning is printed, then no exception handler was passed.'
      end if
    end if
      
    ! ALLWAYS remove procedure from trace at the end and before return!
    if (present(eh)) & 
      call eh%remove_from_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
  end subroutine yet_another_subroutine
end module another_module
