# Fortran Exception Handling
---
`exception_handling` is a library that aims to provide useful tools for **exception handling in Fortran**
and **tracing**.

#### Table of contents
1.  [Motivation](#motivation)
2.  [Data types](#data-types)
3.  [Minimal example (explained)](#minimal-example-explained)
4.  [Tips, recommendations and best practice](#tips-recommendations-and-best-practice)

---
## Motivation

Imagine, you are developing a cool Fortran library that many other users and developers should incorporate
into their projects. From time to time it might happen that in some of your procedures undesired things happen.
This might be due to incorrect usage of your library, passing of invalid arguments to your procedures or simply
due to errors in you're code your not aware of. We call all these (and other) undesired events **exceptions**.

Generally, you don't want the code to stop immediately once an exception occurs. Instead you want to **keep track**
of all exceptions and your part of the code can **leave without terminating** the full program. Instead, you
just want to tell the users of your library that one or more exceptions were occured during the execution of
your code and let them **decide how to deal** with them. Maybe they want to proceed depending on the severity
of the exception or they want to do some finalization first or save important things to files before the programs
finally stops. This behaviour should be enabled with the help of this library. We say that your library should
**throw** exceptions and the users of your library should **catch** them and react approbriately.

---
## Data types

### `ExceptionClass`
```fortran
type(ExceptionClass) :: ec
ec = ExceptionClass( name [, report_units] )
```
This is is simple type. It just defines a class or type of exceptions by a name and a list of output units to
which exceptions of this class will be reported.  
There are **two globally predefined exception classes**:
* `ERROR_CLASS` for severe exceptions
* `WARNING_CLASS` for mild exceptions or notifications

### `Exception`
```fortran
type(Exception) :: e
e = Exception( class [, code, message, ...] )
```
This type describes an instance of an exception. Each exception is of a specific exception class and comes
with a custom exception code and report message. It might also contain additional data on where this exception
occured (was thrown) and where it was dealt with (was caught).

### `ExceptionHandler`
```fortran
type(ExceptionHandler) :: eh
eh = ExceptionHandler( name [, ...] )
```
An exception handler goes by a descriptive name and collects and keeps track of all exceptions. There can be
multiple exception handlers for different code parts. Each exception handler can handle exceptions of the default
classes `ERROR_CLASS` and `WARNING_CLASS`. But you can also add custom classes. This allows for different exceptions
handlers to report exceptions of the same class to different output units. An exception handler can **throw**
and **catch** exceptions.

### `ProcedureTrace`
```fortran
type(ProcedureTrace) :: tr
tr = ProcedureTrace( trace )
```
A trace is basically a list of module and procedure names leading to a given place in the code. They allow to track
where exceptions were thrown and caught and should help debugging.

For completenes, there is also the type `ExceptionOutputConfiguration` which is not that important. It's just a collection
of settings to control how the exception report output is formatted.

---
## Minimal example (explained)
```fortran
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
    if (e%class == ERROR_CLASS) error stop
  end do

  write( *, '(a,f10.6,a,f10.6)' ) 'The square root of', a, ' is', b

  call eh%remove_from_trace( module='minimal_handling', procedure='(main program)', trace=tr )
end program minimal_example
```
The example above contains a module `cool_library` which contains subroutine `real_sqrt` that tries to compute the
square root of a real input `a` and writes it to an output variable `b`. This subroutine allows for exception handling
and also takes an `ExceptionHandler` as an argument.

In the main program `minimal_example` we read a number form the terminal and try to compute it's square root using 
`real_sqrt`. For an invalid (negative) input, `real_sqrt` does not stop the program but returns silently and added an exception 
to the `ExceptionHandler` that was created before. The exception is caught and reported in the main program.

For a negative input, the above code produces the following output:
```
Enter a real number: -1
ERROR [my exception handler]
  trace:    lv. 1 ┬ MINIMAL_HANDLING % (MAIN PROGRAM)
                2 └─ COOL_LIBRARY % REAL_SQRT
            caught on level 1 in "MINIMAL_HANDLING % (MAIN PROGRAM)"            
  code:     42
  message:  Input parameter `a` must not be negative.
            (Result may be undefined.)            

```
The exception report starts with the name of the exception class (`ERROR`) followed by the name of the exception handler
that threw the exception (`[my exception handler]`). It follows a trace leading to the procedure in which the exception
occured (`COOL_LIBRARY % REAL_SQRT`) given by the module and procedure name separated by the percent sign. The following
lines tells where the exception was caught (`MINIMAL_HANDLING % (MAIN PROGRAM)`). After this, the custom exception code
and a message telling what happened follow.

Let's walk thorugh the most important steps and their explanation.  
In the **main program `minimal_example`**, an exception handler object is created with
```fortran
eh = ExceptionHandler( 'my exception handler' )
```
All constructors go by the same name as the types they construct. As an argument, we pass the name of our exception handler.  
On the next line
```fortran
call eh%add_to_trace( module='minimal_handling', procedure='(main program)', trace=tr )
```
we register the current location in the trace bound to the exception handler, `eh%trace`. The subrutine `add_to_trace` typically takes 
the name of the module and the procedure we are currently in. Here, we slightly missuse the routine by passing the name of the main
program as the module name and the info that we are in the main program as the procedure name, because we are not in a module
here. The subroutine also returns a copy of the current state of the type bound trace into the local variable `tr`. The local
copy is important to keep track because the type bound trace might (and will) change in between.  
Then, we call the subroutine `real_sqrt` which allows for exception handling
```fortran
call real_sqrt( a, b, eh )
```
Besides the necessary arguments `a` and `b`, we also pass the exception handler `eh`.  
After the subroutine was executed, we check if it added any exceptions to exception handler using the logical function
`eh%has_uncaught_exceptions()`. If this is the case, we catch the exception via `eh%catch`. The `catch` subroutine returns
the first exception of the exception stack into the variable `e`. Additionally, we pass the local state of the trace `tr`
to the subroutine which allows to correctly tracking down the point at which the exception is caught. Then, we report the
exception by calling `e%report`. This will write the report to the output units speciefied for the exception class of the
exception. The default unit for exceptions of class `ERROR_CLASS` is the standard error output `error_unit` from the intrinsic
module `iso_fortran_env`. Lastly, we descide to stop the program if the exception is a severe error. Everything together, this
looks like
```fortran
do while (eh%has_uncaught_exceptions())
  call eh%catch( e, trace=tr )
  call e%report
  if (e%class == ERROR_CLASS) error stop
end do
```
Finally, we deregister the current location from the type bound trace by calling
```fortran
call eh%remove_from_trace( module='minimal_handling', procedure='(main program)', trace=tr )
```
Passing the local trace `tr` is not strictly necessary, but allows to detect any trace corruptions that might happend on the way.  
In the **subroutine `real_sqrt`**, again, we start by registering the current location in the type bound trace and store the local
copy in `tr`
```fortran
call eh%add_to_trace( module='cool_library', procedure='real_sqrt', trace=tr )
```
Then, the value of the input argument `a` is checked and an exception is thrown if `a` is negative
```fortran
call eh%throw( ERROR_CLASS, trace=tr, &
  code=42, &
  message='Input parameter `a` must not be negative.', &
  info='Result may be undefined.' )
```
The subroutine `eh%throw` takes the type of exception as an `ExceptionClass` object parameter. Here, we throw an exception of the
class `ERROR_CLASS` which is predefined and publicly available from the module `exception_handling`. All other arguments are optional
but recommended. Again, we pass the local state of the trace `tr` to correctly track, where the exception is thrown. We also
provide a custom exception code, a message and some additional information.  
Lastly, we deregister the current location from the trace
```fortran
call eh%remove_from_trace( module='cool_library', procedure='real_sqrt', trace=tr )
```

---
## Tips, recommendations and best practice
1.  Form a habit of defining **module and procedure name constants** immediately when creating a new module or procedure
    and use them when adding or removing a location to or from the trace.  
    ```fortran
    module cool_library
      ! uses
      implicit none
      character(len=*), private, parameter :: MODULE_NAME = 'cool_library'
      ...
    contains
      subroutine real_sqrt(...)
        ! uses and arguments
        character(len=*), parameter :: PROCEDURE_NAME = 'real_sqrt'
        ! local variables
        ...
        call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
      end subroutine real_sqrt
    end module cool_library
    ```
2.  Always **register location at very beginning** of all procedures that allow for exception handling.   
    ```fortran
    subroutine any_procedure(...)
      ! uses, arguments and local variables
      call eh%add_to_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
      ...
    end subroutine any_procedure
    ```
3.  Always **deregister location as late as possible** in all procedures that allow for exception handling.
    ```fortran
    subroutine any_procedure(...)
      ! uses, arguments and local variables
      ...
      call eh%add_remove_from_trace( module=MODULE_NAME, procedure=PROCEDURE_NAME, trace=tr )
    end subroutine any_procedure
    ```
    > :warning: **WARNING**  
    > Beware **premature `return` statements** and remember to deregister before the procedure is left!
