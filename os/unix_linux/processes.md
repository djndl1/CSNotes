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

The three forms of abnormal terminations:

1. Calling `abort()`: generates the `SIGABRT` signal

2. When the process receives certain signals.

3. The last thread responds to a cancellation request.

The parent process can obtain the termination status from either the `wait` or `waitpid`function. The exit status is converted into a termination status by the kernel when `_exit` is finally called. The kernel keeps a small amount of information for every terminating process, so that the information is available when the parent of the terminating process calls `wait` and `waitpid`.  performing a wait allows the system to  release  the  resources  associated with the child; if a wait is not performed, then the terminated child remains in  a  "zombie" state. A child that has terminates but is not waited for by its parent becomes a _zombie_. 

Whenever a parent process terminates, all the children will be adopted by PID 1. These children won't become zombies since PID 1  waits to fetch the termination status.

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

# Process Control

## Process Identification

Every process has a unique process ID, a non-negative integer. Most UNIX systems implement algorithms to delay reuse, , so that newly created processes are assigned IDs different from those used by processes that terminated recently.

Process ID 0 is usually the scheduler process and is often known as the _swapper_ ((how it is)[https://superuser.com/questions/377572/what-is-the-main-purpose-of-the-swapper-process-in-unix]). Process ID is usually the `init` process and is invoked by the kernel at the end of the bootstrap procedure. It reads the system-dependent initialization files and brings the system to a certain state. It is a normal user process, not a system process within the kernel.

`getpid()`, `getppid()`, `getuid()`, `geteuid()`, `getgid()`, `getegid()` return identifiers for every process.

## Create New Processes

An existing process can create a new one by calling the `fork` function. It is called once but returns twice. Both the child and the parent continue executing with the instruction that follows the call to `fork`. The child gets a copy of the parent's data space, heap and stack. Modern implementations employs copy-on-write instead of copying immediately. Linux provides a `clone` syscall for creating a child process.

In general, it is uncertain whether the child starts executing before or after the parent. The order depends on the scheduling algorithm used by the kernel.

One characteristic of `fork` is that all file descriptors that are open in the parent are duplicated in the child. The parent and the child share the same file offset. The child inherits from the parent:

- Real user ID, real group ID, effective user ID and effective group ID; The set-user-ID and set-group-ID flags

- Supplementary group IDs

- Process group ID

- Session ID

- Controlling terminal

- Current working directory

- root directory

- file mode create mask

- signal mask and dispositions

- The close-on-exec flag

- Environment

- Attached shared memory segments

- memory mappings

- resource limits

However, the set of pending signals (for the child it's zero), file locks set by the parents are not inherited by the child.

The `vfork` has different semantics than `fork`. It creates a process without copying the address space of the parent into the child. the child simply calls `exec` right after the the `vfork`. The child runs in the address space of the parent until it calls either `exec` or `exit`. `vfork` guarantees that the child runs first until the child calls `exec` or `exit`. 

When a process terminates, the kernel sends `SIGCHLD` to notify its parent, who can choose to ignore the signal or handle it.

A process that calls a `wait`/`waitpid` function can block if all of its children are still running, return immediately with the termination of a child if a child has terminated and is waiting for its termination status to be fetched, or return immediately with an error if it doesn't have any child process.

The termination status obtained by `wait`/`waitpid` can be determined by `WIFEXITED`, `WIFSIGNALED`, `WIFSTOPPED`, `WIFCONTINUED`.

`waitpid` has an `options` argument to let us control the operation of `waitpid` instead of simply blocking and waiting for a child process to terminate.

`waitid` is an even more flexible version of `waitpid` and returns more detained information about the child process. `wait3` and `wait4` provide a summary of the resources used by the terminated process and all its child.

If a process wants to wait for its parent to terminate

```c
// poling
while (getppid() != 1)
    sleep(1);
```

which is wasting CPU time.

## `exec`

The PID is not replaced. `exec` merely replaces the current processes, its text, data, heap and stack segments with a brand-new program from disk.

```c
// l: list of arguments
// v: vector (array) of arguments
// p: use the PATH variable
// e: accept environment

       int execl(const char *pathname, const char *arg, ...
                       /* (char  *) NULL */);
       int execlp(const char *file, const char *arg, ...
                       /* (char  *) NULL */);
       int execle(const char *pathname, const char *arg, ...
                       /*, (char *) NULL, char * const envp[] */);
       int execv(const char *pathname, char *const argv[]);
       int execvp(const char *file, char *const argv[]);
       int execve(const char *pathname, char *const argv[],
                  char *const envp[]);
       int execvpe(const char *file, char *const argv[],
                       char *const envp[]);
       int fexecve(int fd, char *const argv[], char *const envp[]);
```

POSIX.1 specifically requires that open directory streams be closed across an `exec`. the effective IDs can change, depending on the status of the set-user-ID and the set-group-ID bits for the program file that is executed. 

In many UNIX system implementations, only one of these seven functions, `execve`, is a system call within the kernel. The other six are just library functions that eventually invoke this system call. 

The  exec()  functions return only if an error has occurred. It has been replaced by another program.

## Changing User IDs and Group IDs

Read `man 7 credentials`.

When programs need additional privileges or need to gain access to resources that they currently aren't allowed to access, they need to change their user to group ID or an ID that has the appropriate privilege or access. 

```c
       int setuid(uid_t uid);
       int setgid(gid_t gid);
```

If process has superuser privileges, `setuid` set the real user ID, effective user ID, and saved set-user-ID to `uid`. If the process does not have superuser privileges but `uid` equals the real user ID or the saved set-user-ID, `setuid` sets only the effective user ID to `uid`.

Only a superuser process can change the real user ID. The effective user ID is set by the `exec` functions only if the set-user-ID bit is set for the program file. We have no portable way to obtain the current value of the saved set-user-ID.

```c
       int setreuid(uid_t ruid, uid_t euid);
       int setregid(gid_t rgid, gid_t egid);
```

set real and effective user/group IDs of the calling process.

```c
       int seteuid(uid_t euid);
       int setegid(gid_t egid);
```

An unprivileged user can set its effective user ID to either its real user ID or its saved set-user-ID.

By using set-user-ID, we can use the extra privileges only when we need elevated privileges.

## Interpreter File

```bash
#!pathname [optional argument]
```

The recognition of these files is done within the kernel as part of processing the `exec` syscall. The pathname of the interpreter file is passed to the interpreter.

TODO

## `system` function

it is convenient to execute a command string from within a program. `system` is implemented by calling `fork`, `exec` and `waitpid`.

If we call `system` call from a set-user-ID program, it causes a security hole. The spawned process have the privileged effective ID. If the parent of a spawned process is a set-user/group-ID program, the child process must change back to normal permissions after the `fork` before `exec`.

## Process Accounting

The kernel writes an accounting record each time a process terminates if enabled.

```c
           #define ACCT_COMM 16

           typedef u_int16_t comp_t;

           struct acct {
               char ac_flag;           /* Accounting flags */
               u_int16_t ac_uid;       /* Accounting user ID */
               u_int16_t ac_gid;       /* Accounting group ID */
               u_int16_t ac_tty;       /* Controlling terminal */
               u_int32_t ac_btime;     /* Process creation time
                                          (seconds since the Epoch) */
               comp_t    ac_utime;     /* User CPU time */
               comp_t    ac_stime;     /* System CPU time */
               comp_t    ac_etime;     /* Elapsed time */
               comp_t    ac_mem;       /* Average memory usage (kB) */
               comp_t    ac_io;        /* Characters transferred (unused) */
               comp_t    ac_rw;        /* Blocks read or written (unused) */
               comp_t    ac_minflt;    /* Minor page faults */
               comp_t    ac_majflt;    /* Major page faults */
               comp_t    ac_swaps;     /* Number of swaps (unused) */
               u_int32_t ac_exitcode;  /* Process termination status
                                          (see wait(2)) */
               char      ac_comm[ACCT_COMM+1];
                                       /* Command name (basename of last
                                          executed command; null-terminated) */
               char      ac_pad[X];    /* padding bytes */
           };
```

TODO

## User Identification

`getlogin()` fetches the login name. A single user might have multiple login names, each with the same user ID.

## Process Scheduling

Read `man sched`.

Lower nice values have higher scheduling priority.

A process can retrieve and change its nic

e value with `nice()`. `gerpriority()` get the nice value for a process or a group of related process. `setpriority` can be used to set the priority of a process, a process group, or all the processes belonging to a particular user ID.

nice value range ????

## Process Times

Any process can call the `times` function to obtain wall clock time, user CPU time and system CPU time for itself and any terminated children.

# Process Relationships

## Logins

`init` does a `fork` and `exec`s `getty`. `getty` opens terminal device, reads user name, set initial environment and `exec`s `login`. `login` checks the password (on modern Unix, it's done by PAM library) we give it, exits with wrong password. Otherwise, it changes to our home directory, changes the ownership and permissions of our terminal device, sets our group ID, initialize `HOME`, `SHELL`, `USER`, `LOGNAME` and a default `PATH` and finally invoke our login shell. The login shell now reads its startup files.

A software driver called _pseudo terminal_ is used to terminate the behavior of a serial terminal and map terminal operations to network operations and vice versa. In BSD, a single process, `inetd`, waits for most network connections. `inetd` does a `fork` and `exec` of the appropriate program.

## Process Groups

Each process belongs to a process group, a collection of one or more processes, usually associated with the same job. Each process group can have a process group leader. The leader is identified by its process group ID being equal to its process ID. It is possible for a process group leader to create a process group, create processes in the group, and then terminate.

```c
       int setpgid(pid_t pid, pid_t pgid);
       pid_t getpgid(pid_t pid);

       pid_t getpgrp(void);                 /* POSIX.1 version */
       pid_t getpgrp(pid_t pid);            /* BSD version */

       // create a new process group
       int setpgrp(void);                   /* System V version */
       int setpgrp(pid_t pid, pid_t pgid);  /* BSD version */
```

A process can set the process group ID of only itself or any of its children.

## Sessions

A session is a collection of one or more process groups. The processes in a process group are usually placed there by a shell pipeline.

```c
       pid_t setsid(void);
       pid_t getsid(pid_t pid);
```

## Controlling terminal

TODO

## Job Control

Job control allows us to start multiple jobs from a single terminal and to control which jobs can access the terminal and which jobs are run in the background.

TODO

## Shell Execution

TODO

# Daemon Processes

