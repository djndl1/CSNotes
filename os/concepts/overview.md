Operating system provides user programs with a better, simpler, cleaner, model of the computer.

Most computers have two modes of operation: _kernel mode_ and _user mode_. The operating system runs in kernel mode (also called [supervisor mode](https://en.wikipedia.org/wiki/Protection_ring#Supervisor_mode)), in which, it has access to all the hardware and can execute any instructions the machine is capable of executing. It is sometimes difficult to draw a line between kernel space and user space. Some OSes, specially embedded one, do not have this distinction. Many sensitive OS-related programs run in user mode.

Operating systems perform two essentially unrelated functions: 

- providing application programmers a clean abstract set of resources instead of the messy hardware ones; OS works as an extended machine. The abstract is the key to managing all the complexity.

- managing hardware resources. OS works as a resource manager. Resource management includes multiplexing resources in time (CPU multiprocessing) and in space ( virtual memory).
