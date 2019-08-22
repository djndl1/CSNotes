# Fundamental Concepts

TODO

# System Programming Concepts

- system call: a system call is a controlled entry point into the kernel, allowing a process to request that the kernel perform some action on the process's behalf. A syscall changes the processor state from user mode to kernel mode so that the CPU can access protected kernel memory. Each syscall is identified by a unique number. 

The application program makes a syscall by invoking a wrapper function in the C library. The wrapper accepts arguments and puts them in specific registers. The wrapper function puts the system call number into a specific CPU register. The wrapper executes a `trap`/`sysentr`/`syscall` machine instruction. The kernel invokes its `system_call()` routine to handle the trap. It saves arguments from registers onto the kernel stack, checks the validity of the syscall number and invokes the appropriate syscall service routine. After the service routine returns, the return value is then placed on the stack. Returns to the wrapper. Any error value is set on the global variable `errno`.

- library function: a library function is simply one of the multitude of functions that constitutes the standard C library. Many library functions don't make any use of system calls. Often, library functions are designed to provide a more caller-friendly interface than the underlying system call.

Almost every system call and library function returns some type of status value indicating whether the call succeeded or failed.  This status value should always be checked to see whether the call succeeded.
