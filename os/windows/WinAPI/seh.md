SEH provides a robust mechanism that allows applications to respond to unexpected asynchronous events. SEH allows specification of a code block or _exception handler_. Normally, an exception indicate a fatal error with no recovery. SEH is supported through a combination of Windows functions, compiler-supported language extension and run-time support.

```c
__try {
    /* Block of monitored code */
}
__except (filter_expression) { // usually a literal constant, a call to filter function or a conditional expression
    /* Exception handling block */
}

```

The runtime support unwinds the stack to find  the exception handler and then gives control to the handler.

`__try` can be used with any code blocks.

Exceptions are asynchronous. It is not possible or practical to test for an exception. Exception handlers provide a convenient mechanism for exiting from inner blocks or functions under program control without resorting to a `goto`, SJLJ or some other control.

# Filter Expression 

1. `EXCEPTION_EXECUTE_HANDLER`: executes the except block

2. `EXCEPTION_CONTINUE_SEARCH`: ignores the exception handler and searches for an exception handler in the enclosing block until it finds one

3. `EXCEPTION_CONTINUE_EXECUTION`: immediately returns control to the point at which the exception occurred. Not always possible and inadvisable.

# Exception Codes

The `__except` block or the filter expression can determine the exact exception using 

```c
// literal constants
DWORD GetExceptionCode(void);
// detailed info about the exception
LPEXCEPTION_POINTERS GetExceptionInformation(void);
```

# Raise An Exception

```c
__analysis_noreturn VOID RaiseException(
  DWORD           dwExceptionCode,
  DWORD           dwExceptionFlags,
  DWORD           nNumberOfArguments,
  const ULONG_PTR *lpArguments
);
```

# Termination Handler

Termination handlers are a convenient way to close handles, release resources, restore masks and otherwise restore the process to a known state when leaving a block.

```c
__try {

}
__finally {
   
}
```

The termination handler is executed when falling through, `return`, `break`, `goto`, `longjmp`, `continue`, `__leave`(directly go to the termination handler) or an exception is raised.

`BOOL AbnormalTermination(void)` determines how the block terminates.

Although chaining `__try`, `__catch` and `__finally` is not allowed, embedding one in another somehow is possible.

Termination handlers do not execute if a process or thread terminates whether the process or thread terminates itself by using `ExitProcess` or `ExitThread` or whether the termination is caused externally by =TerminateProcess= or =TerminateThread=.

SEH is not encouraged to use with C++. A Windows exception or termination handler will not call destructors to destroy C++ object instances.

# Console Control Handlers

`SetConsoleCtrlHandler` allows one or more specified functions to be executed on receipt of a Ctrl-C, Ctrl-break or one othree other console-related signals.

```c
BOOL WINAPI SetConsoleCtrlHandler(
  _In_opt_ PHANDLER_ROUTINE HandlerRoutine,
  _In_     BOOL             Add
);
```

# [Vectored Exception Handling](https://docs.microsoft.com/en-us/archive/msdn-magazine/2001/september/under-the-hood-new-vectored-exception-handling-in-windows-xp)

Unlike frame-based SEH, VEH is process-wide. The behavior is similar to POSIX signals.

When an exception occurs, the VEHs are called first, before the system unwinds the stack to look for structured exception handlers.

```c
PVOID AddVectoredExceptionHandler(
  ULONG                       First,
  PVECTORED_EXCEPTION_HANDLER Handler
);
```

The returned value is a handle to the exception handler and used to remove the handler with `RemoveVectoredExceptionHandler`.
