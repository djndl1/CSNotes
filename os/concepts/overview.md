Operating system provides user programs with a better, simpler, cleaner, model of the computer.

Most computers have two modes of operation: _kernel mode_ and _user mode_. The operating system runs in kernel mode (also called [supervisor mode](https://en.wikipedia.org/wiki/Protection_ring#Supervisor_mode)), in which, it has access to all the hardware and can execute any instructions the machine is capable of executing. It is sometimes difficult to draw a line between kernel space and user space. Some OSes, specially embedded one, do not have this distinction. Many sensitive OS-related programs run in user mode.

Operating systems perform two essentially unrelated functions: 

- providing application programmers a clean abstract set of resources instead of the messy hardware ones; OS works as an extended machine. The abstract is the key to managing all the complexity.

- managing hardware resources. OS works as a resource manager. Resource management includes multiplexing resources in time (CPU multiprocessing) and in space (virtual memory).

# History

## The First Generations

plugboards, punched cards.

## The Second Generations

A program first writes the program on paper and then punch it on cards. It was then handed to the input machine, which collected a tray full of _jobs_ and output them onto a magnetic tape. The batch system on the mainframe then processed the job  and printed their output to a tape, one by one.

## The Third Generations

OS/360, IC, compatible across different computer systems, multiprogramming (CPU multiplexing and memory partitioning), spooling (Simulatenous Peripheral Operation On Line, that is, read next job into memory while processing the previous job), timesharing. MULTICS (MULTiplexed Information and Computing Service), primitive cloud computing concept. UNIX, POSIX, MINIX, Linux.
