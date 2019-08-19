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
                                        |    (bss,block stared      |   
                                        |      by symbols)          |
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

Usually, the text segment is sahrable so that only a single copy needs to be in memory for frequently executed programs and is often read-only. The only portions of the program that need to be saved in the program file are the text segment and the initialized data. `size` command reports information of all above.

ISO C specifies three functions for memory allocation:

- `malloc`

- `calloc`: allocates space for a specified number of objects of a specified size. All space initialized to zero bits.

- `realloc`: increases or decreases the size of a previously allocated area.

The pointer returned is guaranteed to be suitably aligned so that it can be used for any data object. `free` causes the space to be deallocated. Most versions of `malloc` and `free` never decrease their memory size. They are usually reserved for a later allocation and not returned immediately to the kernel. Most implementations allocate more space than requested and use the additional space for record keeping. As a consequence, writing past the end or before the start of an allocated area could overwrite this record-keeping information in another block. These error would be hard to find out.

# `setjmp`, `longjmp` 

Useful for handling error conditions that occur in a deeply nested function call. On systems that don't have built-in hardware support for stacks, a C implementation might use a linked list for its stack frmes. To avoid multiple returns in a nested funcall situation, use `setjmp` and `longjmp`.

`setjmp` is called from where to return to. Normally the `env` variable is a global variable so to be referenced from another function. `longjmp` causes the stack to be unwound back, throwing away stack frames. However, where the control jumps back to, automatic and register variables may not be there as before the `setjmp` call.

```c
#include <stdio.h>
#include <setjmp.h>

static void f1(int, int, int, int);
static void f2(void);

static jmp_buf jmpbuffer;
static int globval;

int main(int argc, char *argv[])
{
        int             autoval;
        register int    regival;
        volatile int    volaval;
        static int      statval;

        globval = 1; autoval = 2; regival = 3; volaval = 4; statval = 5;

        if (setjmp(jmpbuffer) != 0) {
                printf("after longjmp:\n");
                printf("globval = %d, autoval = %d, regival = %d, volaval = %d, statval = %d\n",
                       globval, autoval, regival, volaval, statval);
                return (0);
        }

        globval = 95; autoval = 96; regival = 97; volaval = 98; statval = 99;

        f1(autoval, regival, volaval, statval);
        return 0;
}

static void f1(int i, int j, int k, int l)
{
        printf("in f1(:\n)");
        printf("globval = %d, autoval = %d, regival = %d,"
               "volaval = %d, statval = %d\n", globval, i, j, k, l);
        f2();
}

static void f2(void)
{
        longjmp(jmpbuffer, 1);
}

```

```bash
# without optimization
in f1(:
)globval = 95, autoval = 96, regival = 97,volaval = 98, statval = 99
after longjmp:
globval = 95, autoval = 96, regival = 3, volaval = 98, statval = 99

# with optimization
in f1(:
)globval = 95, autoval = 96, regival = 97,volaval = 98, statval = 99
after longjmp:
globval = 95, autoval = 2, regival = 3, volaval = 98, statval = 99
```

For global, static and volatile variables, their values after `longjmp` are the last values that they assumed.

# `getrlimit`, `setrlimit`

Every process has a set of resource limits, some of which can be queried and changed by `getrlimit` and `setrlimit` functions. A process can change its soft limit to a value less than or equal to its hard limit. A process can lower its hard limit to value greater than or equal to its soft limit. Only a superuser process can raise a hard limit. The resources limits affect the calling process and are inherited by any of its children.
