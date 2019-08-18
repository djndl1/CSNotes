# Process Environment

## `main` function

When a C program is executed by the kernel—by one of the exec functions, a special start-up routine is called before the main function is called. The executable program file specifies this routine as the starting address for the program; this is set up by the link editor when it is invoked by the C compiler. This start-up routine takes values from the kernel—the command-line arguments and the environment — and sets things up so that the main function is called. The startup routine ensures that if the `main` function returns, the `exit` function is called.

## Process Termination

### Normal termination

- return from `main`

- calling `exit`

- calling `_exit` or `_Exit` (equivalent)

- return of the last thread from its start routine

- calling `pthread_exit` from the last 

Three functions, `_exit()`, `_Exit()`, which returns to the kernel immediately, and `exit()`, which performs certain cleanup processing and then returns to the kernel.

If any of these functions is called without an exit status, `main` does a return without a return value, or the main function is not declared to return an integer, the exit status of the process is undefined. If the return type of main is an integer and main falls off the end (an implicit return), the exit status of the process is 0. Returning an integer value from the main function is equivalent to calling `exit` with the same value.

A process can register at least 32 functions that are automatically called by `exit`, called _exit handlers_ and registered by calling the `atexit` function. The `exit` function calls these  functions in reverse order of their registration.

```
                           +------------+                         call
          +----------------+    user    |     exit            +---------------^----------------+
          |                |  functions +------------+        | +--------------+ exit handler  |
          |                +------------+            |        | |   return     +---------------+
          |                                          |        | |
          |                                          |        | |
          |                                          v        | v
          |                                          +--------|-+    call
          |               +------------+    exit     |   exit   +------------> +---------------+
          |               |    main    +------------>+ function |              |  exit handler |
          +<--------------+   function |             |          +<-----------+ +---------------+
          |               +--|-----|---+        +--->+----|--|-++    return
          |                  |     ^            |         |  ^ |
          |           return |     |            |         |  | |     call
          |                  |     | call       |         |  | +-------------->+------------+
          |                  |     |            |         |  |                 |  standard  |
          |              +---v-----|---+        |         |  +-----------------+    I/O     |
          |              | C start-up  +--------+         |         return     |  cleanup   |
 _exit    |              |  routine    |   exit           |                    +------------+
  or      |              +-----|-------+                  |
 _Exit    |                    ^                          |  _exit
          |                    |                          |   or
          |                    |exec                      |  _Exit
          |                    |                          |
          |                    |                          v
+---------v--------------------|--------------------------|------------------------------------------------------------------+
|                                                                                                                            |
|                                                                                                                            |
|                                                      kernel                                                                |
|                                                                                                                            |
|                                                                                                                            |
+----------------------------------------------------------------------------------------------------------------------------+

```

More at https://stackoverflow.com/questions/25434850/where-does-the-returned-value-for-main-function-go

# Environment 

Each program is also passed an environment list. The address of the array of pointers is contained in the global variable `environ`:

```c
extern char **environ; // array of strings `name=value`
```

POSIX.1 specifies that environ should be used instead of the (possible) third argument of the `main` function. Access to specific environment variables is normally through the `getenv`, `putenv`, `setenv` and `unsetenv` functions.

we can affect the environment of only the current process and any child processes that we invoke. We cannot affect the environment of the parent process, which is often a shell.

# Memory

```
                                        +---------------------------+
                                        |  command-line arguments   |
                                        |                           |
                                        |  environment variables    |
                                        +---------------------------+
                                        |                           |
                                        |           stack           |
                                        |                           |
                                        +-----------|---------------+
                                        |           |               |
                                        |           v               |
                                        |                           |
                                        |           ^               |
                                        |           |               |
                                        +-----------|---------------+
                                        |           heap            |
                                        |                           |
                                        +---------------------------+
                                        |                           |
                                        |    uninitialized data     |
                                        |          (bss)            |
                                        |                           |
                                        |                           |
                                        +---------------------------+
                                        |                           |
                                        |      initialized data     |
                                        |                           |
                                        |                           |
                                        +---------------------------+
                                        |                           |
                                        |                           |
                                        |          text             |
                                        |                           |
                                   low  +---------------------------+
```
