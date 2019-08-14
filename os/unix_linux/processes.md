# Process Environment

## `main` function

When a C program is executed by the kernel—by one of the exec functions, a special start-up routine is called before the main function is called. The executable program file specifies this routine as the starting address for the program; this is set up by the link editor when it is invoked by the C compiler. This start-up routine takes values from the kernel—the command-line arguments and the environment — and sets things up so that the main function is called. The startup routine ensures that if the `main` function returns, the `exit` function is called.

## Process Termination

### Normal termination

- return from `main`

- calling `exit`

- calling `_exit` or `_Exit`

- return of the last thread from its start routine

- calling `pthread_exit` from the last thread.
