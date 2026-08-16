Operating system provides user programs with a simpler, cleaner model of the computer.

Most computers have two modes of operation: _kernel mode_ and _user mode_. The operating system runs in kernel mode (also called [supervisor mode](https://en.wikipedia.org/wiki/Protection_ring#Supervisor_mode), usually represented as a bit in the program status word), in which, it has access to all the hardware and can execute any instructions the machine is capable of executing. It is sometimes difficult to draw a line between kernel space and user space. Some OSes, specially embedded one, do not have this distinction. Many sensitive OS-related programs run in user mode.

```
Application+ +---------------------------+
  Programs | |       Web Browser         |
           + |       Banking System      |
          +  +---------------------------+
          |  |   Compilers Editors       |
System    |  |   Command Interpreter     |
Programs  |  +---------------------------+
          |  |                           |
          +  |     Operating System      |
             |                           |
          +  +---------------------------+
          |  |                           |
          |  |     Machine Language      |
          |  |                           |
          |  +---------------------------+
          |  |                           |
Hardware  |  |     Microarchitecture     |
          |  |                           |
          |  +---------------------------+
          |  |                           |
          |  |     Physical devices      |
          |  |                           |
          +  +---------------------------+
```

Operating systems perform essentially unrelated functions:

- extended machine/control program (top-down): providing application programmers a clean abstract set of resources instead of the messy hardware ones; OS works as an extended machine. The abstract is the key to managing all the complexity.
  - abstraction in two aspects: for normal users (interactive interface) and programmers (better API)
  - abstraction is an understanding in system function. API design is grounded in system design.
>Abstraction is often taught as ""hiding unnecessary details." That's true, but incomplete. The harder part is deciding which details are essential for the system’s function and meaning. When you design an abstraction, you’re defining a new layer of reality—a model that must faithfully represent the capabilities below while shaping how those capabilities can be used. the interface is the visible boundary, but the function is what the system actually accomplishes through that boundary. 

- resource manager/resource allocator (bottom-up): managing hardware resources. OS works as a resource manager. Resource management includes multiplexing resources in time (CPU multiprocessing) and in space (virtual memory).

The operating system might include the always-running kernel, middleware frameworks that ease application development and provide features, and system programs that aid in managing the system while it is running.

# History

## The First Generation

plugboards, punched cards.


## The Second Generation

A program first writes the program on paper and then punch it on cards. It was then handed to the input machine, which collected a tray full of _jobs_ and output them onto a magnetic tape. The batch system on the mainframe then processed the job  and printed their output to a tape, one by one.

## The Third Generation

- OS/360, IC, compatible across different computer systems, multiprogramming (multiple programs in the memory, CPU multiplexing and memory partitioning), spooling (Simulatenous *Peripheral Operation* On Line, that is, a combination of buffering and queuing), timesharing.

- MULTICS (MULTiplexed Information and Computing Service), primitive cloud computing concept.

- MInicomputers: UNIX, POSIX, MINIX, Linux.

### Spooling

Peripheral devices have been slower than the processing units and the PUs cannot simply wait for efficiency reason.
A dedicated queued buffer mechanism to free the PUs (becasuse the main system was not capable of multitasking 
 ).

- spooler: the spooling manager program

- spool (a spool of thread, like a tape): the jobs in queue

Historically used for task reading in batch processing, Nowadays mainly for sending printing jobs to printers 
to free the printing software (e.g. Office suites)

## The Fourth Generation

- Personal computers, 8080 and Z-80, CP/M, DOS, MS-DOS,

- Mac OS, Windows 95/98/ME, Windows NT, Linux, FreeBSD,

- distributed operating systems, network operating systems.

## The Fifth Generation: Mobile Computers

PDA, Android, iOS, Symbian OS, Blackberry OS

# OS zoo

## Mainframe Operating Systems

Large amound of I/O capacity; 

services: batch, transaction processing (large numbers of small requests), timesharing

## Server Operating Systems

print service, file service, web service. Solaris, FreeBSD, Linux, Windows Server

## Multiprocessor Operating Systems

## Personal Computer Operating Systems

All modern ones support multiprogramming.

## Handheld Computer Operating Systems

Android, iOS

## Embedded Operating Systems

Embedded Linux, QNX, VxWorks

## Sensor-Node Operating Systems

Event-driven. TinyOS.

## Real-time Opearting Systems

Hard deadlines, soft deadlines,  sometimes the operating system is simply a library linked in with the application programs, with everything tightly coupled and no protection between parts of the system.

## Smart Card Operating Systems

limited power (computing and eletrical), sinples functions, sometimes JVM-based.

# Hardware basics

## CPU

- ISA and registers

- microarchitecture 
  + simple fetch-decode-execute model
  + modern pipelined, superscalar CPU (multiple execution units carrying out instructions from a buffer pool)

- Privileges: generally, all instructions involving I/O and memory protection are disallowed in user mode. Setting the PSW mode bit to enter kernel mode is also forbidden.
  + kernel mode access is restricted from users. Users can only enter kernel mode by certain instructions and pass through certain routes under kernel mode.

- Hyperthreading/hardware multithreading: allows the CPU to hold the state of multiple different threads and then switch back and forth on a nanosecond time scale.
  This does not offer true parallelism, but thread-switching time is reduced to the order of a nanosecond. Each thread appears to the OS as a separate CPU. This reduces  the time wasted on  memory access because the CPU can execute the other thread.

- Multicore: Many CPUs have multiple cores. GPUs have thousands of tiny cores, good for many small computations done in parallel.

## Memory

- Hierarchical architecture: registers, cache, main memory, nonvolatile storage.

- Cached data are transferred in cache lines. 
  - L1 cache in CPUs (dozens of KB) and L2 cache (several MBs)
  - when and which item to cache, which cache line to use: a new item will generally be entered on every cache miss.
  - when and which item to evict

- Main memory: volatile RAM, non-volatile RAM (ROM, EEPROM, Flash memory, CMOS)
  - MMU for virtual memory management

- Non-volatile storage
  - hard disks: Surface (disk surface), cylinder, track (an annular region), sector (segments of a track, typically 512 bytes). 
    tracks through several surfaces forms a cylinder. Moving the arm of read/write heads is slow, taking 5-10 msec to a random cylinder and another 5-10 msec for the neeeded sector to rotate.
  - SSDs: not dissk at all but flash memory.
  - Persistent memory: Intel Optane

## I/O Devices

I/O devices generally consist of two parts: a _controller_ and the device itself (between them e.g. SATA). The controller presents a simpler interface to the OS (e.g. AHCI, xHCI, EHCI, OHCI). Each controller has a _device driver_. Each controller manufacturer has to supply a driver for each OS it supports. It may be relinked to the kernel, registered to the system or installed on the fly without rebooting. Controller is operated through the device register, which forms the _I/O port space_.

I/O operatins may be carried out through busy waiting, interrupt mechanism, or DMA (which controls the flow of bits between memory and some controller without constant CPU intervention).

## Bus

The main bus is PCIe bus. It has multiple serial lanes that transport different messages, instead of different bits of the same message. The CPU talks to memory over a DDR bus, to an external graphics device over PCIe, and to all other devices via a hub over a DMI (Direct Media Interface) bus. 

Intel and M$ designed plug and play, which has the system automatically collect information about the I/O devices, centrally assign interrupt levels and I/O addresses, and then tell each device what its numbers are.

## Booting

BIOS - Self-Check - find boot devices - bootloader - load the OS

# OS Concepts

## Process

A program in execution, with its address space, registers, a list of open files, outstanding alarms, lists of related processes, et al. A container that holds all the information needed to run a program. In many OSes, all the information about a process other than the contents of its own address space, is stored in an operating sytem table called the _process table_. A process consists of its address space (_core image_).

## Address Space

- Memory Protection

- Virtual Memory

## Filesystem

- hierarchical filesystem

- file, directory

- file descriptor

- mounted file system, special file, pipe

## Protection

access control, privilege

## OS Structures

### Monolithic Systems

The most common organization. The entire operating system runs as a single program in kernel mode. The OS is written as a collection of procedures linked together into a single large executable binary program. A crash in any of the procedures will take down the entire OS.

The OS has a basic structure:

- A main program that invokes the requested service procedure;

- A set of service procedures that carry out the syscalls;

- a set of utility procedures that help the service procedures.

### Layered Systems

A more generalized approach is to organize the OS as a hierarchy of layers, each one constructed upon the one below it. e.g. THE:

- layer 0: CPU management, processes, multiprogramming. Everything running on atop of this layer sees sequential processes only.

- layer 1: memory management. Upper layers care only about abstract memory instead of physical memory or paging.

- layer 2: Inter-Process Communication

- layer 3: I/O. Abstract I/O now above this layer

- layer 4: user programs

- layer 5: system operator process

MULTICS had a series of rings/layers, with each outer ring calling inner ring services via trap instructions.

### Microkernels

Puts as little as possible in kernel mode (/Principle of Least Authority/). The basic idea behind the microkernel design is to achieve high reliability by splitting the operating system up into small, well-defined modules, only one of which—the microkernel—runs in kernel mode and the rest run as relatively powerless ordinary user processes, in particular, running each device driver and file system as a separate user process.

Common desktop operating systems do not use microkernels (except MacOS). However, they are dominant in real-time, industrial, avionics, and military applications that are mission critical and have very high reliability requirements.

The MINIX3 OS has a microkernel that handles interrupts, processes, scheduling, interprocess communication, with a set of kernel calls and the clock driver. Outside the kernel, the system is structured as three layers of processes all running in user mode:

- Drivers

- Servers: do most of the work of the OS. File servers manage the file systems; the process manager creates, destroys, and manages processes. the reincarnation server checks if the other servers and drivers are functioning correctly and replaces a faulty one.

- User Programs

This puts the mechanism for doing something in the kernel and let user-mode processes handle the policy.

### Virtual Machines

IBM provided virtual machine systems in the 1970s with VM/370, which provided full OS and hardware virtualization or a CPM single-user interactive systems.
A VM monitor system separates multiprogramming (hypervisor) from an extended machine (guest OS).

In order to run virtual machine software on a computer, its CPU must be virtualizable (purposely designed for virtualization). Privileged instructions executed under user mode are ignored by CPUs without virtualization support unless some interpreter are used. It is essential that the hardware trap to the virtual machine monitor so that instruction can be emulated in software.

- type 1 hypervisor: virtual machine monitor

- type 2 hypervisor: running on top of a host OS, possibly with some kernel modules. Early efforts on PCs required an interpreter, later machine simulatorws with binary translation were introduced and finally a kernel module was added.

Practical hypervisors use a hybrid strategy. They add a kernel module to do the heavy lifting.

- Paravirtualization

- The Java virtual machine

- Containers, provided by the host OS. Each container shares the host OS kernel 
and typically the binaries and libraries in a read-only fashion.

### Exokernels and Unikernel

Exokernels partition resources and allocate them to user-level virtual machines and do not hide the fact that the underlying resources might be shared. 
This saves a layer of resource remapping. 
The functionality of exokernels is limited to ensuring protection and multiplexing of resources. The user application is free to build any custom abstraction on limited hardware resources (not real bare metal), or even runs in full kernel mode. Abstractions are moved into untrusted user-space libraries (libOS) and security is controlled by the exokernel, which also exposes the underlying hardware to its guests directly instead of providing abstraction to its guests. Abstraction (LibOS) is separated from security (exokernel).

> The exokernel concept is a compromise: let the kernel allocate the basic physical resources of the machine (e.g. disk blocks, memory pages, and processor time) to multiple application programs, and let each program decide what to do with these resources. The program can then link to a support library that implements the abstractions it needs (or it can implement its own).

Library OSes, which provide OS services to user programs in the form of a linked library, instead of an independent OS. A program linked with a library OS runs on bare metal, a hypervisor or another OS in user mode. 

Wine on Linux can be seen somewhat as a library OS. And also [the abstraction layer SQLPAL](https://www.microsoft.com/en-us/sql-server/blog/2016/12/16/sql-server-on-linux-how-introduction/) used by SQL Server Linux: the Drawbridge project is even called a library OS and SQL Server itself had been already designed to be quite self contained, only uses a few low-level features from Windows and then the two are merged together to provide Windows NT services to SQL Server. 

A unikernel is a computer program statistically linked with one or more library OSes that provide OS services and can run as a guest of a hypervisor.

#### Case Study: [DrawBridge](https://www.microsoft.com/en-us/research/project/drawbridge/publications/) and [SQLPAL](https://www.microsoft.com/en-us/sql-server/blog/2016/12/16/sql-server-on-linux-how-introduction/)

##### SQLOS

- Background: the hardware trend of SMT and NUMA requires software developers to take advantage of parallelism with optimized concurrent solutions.

A new platform layer to enable ful support for the current and the future hardware features: a user-level operating system, with support for parallelism
and locality and dynamic configuration, but not platform independence. It should be configurable so that the SQL Server could run on low-end as well as high-end 
hardware platforms while hiding complexity from the high level developers but give broad range of flexibility to the low level developers and expand the OS' services
over the new hardware.

- Non-preemptive scheduling

- memory management

- deadlock detection

- exception handling

- external components hosting

>The major objects in SQLOS design are nodes, schedulers, and tasks. Each object at its level exposes functionality maximizing local state and minimizing global state.  SQLOS attempts to minimize global state as much as possible. 

Abstractions:

- memory nodes: memory attached to a CPU or a set of CPUs.
  - different hardware configurations have different relationships between CPU and  memory.

- CPU node: a logical grouping of CPUs. Locality of reference, scheduling affinity

TODO

##### DrawBridge

##### SQLPAL

SQL Server needed to be ported to Linux, however, its library dependencies heavily rely on Windows API and SQL Server could not simply discard
them nor implement them nor port them without sacrificing features, performance or  semantics. SQL Server already had a heavy abstraction layer ([SQLOS](https://learn.microsoft.com/en-us/archive/blogs/slavao/platform-layer-for-sql-server)), which did not rely much on Windows but unfornately still carried Windows API semantics and SQLOS could not help solve the problem of other dependencies.

Drawbridge existed as a library OS, a research project originally intended to reduce virtualization overhead by providing a cut-down Windows environment 
within a single process on atop of a small interface (PAL or hoste extension) to the host environment.

By merging the two, SQL Server either calls into SQLOS APIs, a library or the hosted user-mode Windows OS APIs and all these APIs are built on atop of the new abstraction layer. Only very few I/O calls are directly built on Linux APIs with a small amount of conversion code. When the host extension starts as a native Linux executable, it loads and initializes SQLPAL and then SQLPAL brings up SQL Server. The whole application runs in a single Linux process but SQLPAL creates software-isolated process (a collection of threads) for various components of SQL Servers (Think about how Oracle DBMS manages processes). The ultimate goal is to remove SQLOS from SQLPAL.

# Services

## For Users

- User Interface: GUI, CLI

- Program Execution

- I/O operations

- Filesystem manipulation

- Communication

- Error Detection

## For Efficiency

- Resource Allocation

- Logging, performance counting

- Protection and security

## System Calls: Interface to the OS Services

### Services

- Process Control: create process; terminate process; load, execute; get/set process attributes; wait event, signal event; allocate/free memory

- File Management: create/delete/open/close/read/write/ file; retrieve/set file attributes

- Device management: request/release/read/write/ device; attach/detach devices

- information maintenance: date, time, manage system data;

- commnications: create/close communication; send/receive messages; transfer status information; attach/detach remote devices

- protection

### The Flow

A syscall is structurally similar to an RPC call: contract mandated by the server (kernel), 
different execution contexts (context switch), untrusted input validation. Upon a syscall, the execution is taken over
and the user program is suspended, what is executed is the restricted execution route defined by the kernel. 
The major difference is the transport layer (hardware registers and CPU interrupt/except routing vectors)

A procedure library makes syscalls written in assembly easier to use in C.

- Pushing parameters onto the stack;

- the library procedure puts the syscall number in a place where the OS expects it;

- executes a trap instruction and switches into kernel mode;

- the kernel examines the syscall number and then dispatches to the correct syscall handler;

- after handling the syscall, control returns to the user-space procedure;

- the procedure returns to the user program.

### Win32 API

A Windows program is normally event driven.

On Windows, the library calls and the actual syscalls are highly decoupled. Win32 API are used to get OS services. By decoupling the API interface from the actual syscalls, Microsoft retains the ability to change the actual syscalls in time. Win32 provides compatibility among versions of Windows.

The Win32 API has a huge number of calls for managing windows, geometric figures, text, fonts, scrollbars, dialog boxes, menus, and other features of the GUI.
