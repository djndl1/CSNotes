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

- an interface to the user

- extended machine/control program (top-down): providing application programmers a clean abstract set of resources instead of the messy hardware ones; OS works as an extended machine. The abstract is the key to managing all the complexity.

- resource manager/resource allocator (bottom-up): managing hardware resources. OS works as a resource manager. Resource management includes multiplexing resources in time (CPU multiprocessing) and in space (virtual memory).

The operating system might include the always-running kernel, middleware frameworks that ease application development and provide features, and system programs that aid in managing the system while it is running.

# History

## The First Generation

plugboards, punched cards.


## The Second Generation

A program first writes the program on paper and then punch it on cards. It was then handed to the input machine, which collected a tray full of _jobs_ and output them onto a magnetic tape. The batch system on the mainframe then processed the job  and printed their output to a tape, one by one.

## The Third Generation

OS/360, IC, compatible across different computer systems, multiprogramming (CPU multiplexing and memory partitioning), spooling (Simulatenous Peripheral Operation On Line, that is, read next job into memory while processing the previous job), timesharing. MULTICS (MULTiplexed Information and Computing Service), primitive cloud computing concept. UNIX, POSIX, MINIX, Linux.

## The Fourth Generation

Personal computers, CP/M, DOS, MS-DOS, Mac OS, Windows 95/98/ME, Windows NT, Linux, FreeBSD, distributed operating systems, network operating systems.

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

JVM

# Hardware basics

## CPU

- fetch-decode-execute model

- Program counter; stack pointer; program status word;

- pipeline; superscalar (multiple execute units carrying out instructions from a buffer pool)

Generally, all instructions involving I/O and memory protection are disallowed in user mode. Setting the PSW mode bit to enter kernel mode is also forbidden.

Hyperthreading/hardware multithreading: allows the CPU to hold the state of multiple different threads and then switch back and forth on a nanosecond time scale. This does not offer true parallelism, but thread-switching time is reduced to the order of a nanosecond. Each thread appears to the OS as a separate CPU.

Many CPUs have multiple cores. GPUs have thousands of tiny cores, good for many small computations done in parallel.

## Memory

- Hierarchical architecture; 

Certain correspondence is established between cache lines and memory addresses.

L1 cache: inside the CPU and usually feeds decoded instructions into the CPU's execution engine; L2: several megabytes of recently used memory words.

## Disks

- Surface, cylinder, track, sector
  - tracks through several surfaces forms a cylinder

SSDs are not disks at all, they are flash memory.

## I/O Devices

I/O devices generally consist of two parts: a _controller_ and the device itself. The controller presents a simpler interface to the OS. Each controller has a _device driver_. Each controller manufacturer has to supply a driver for each OS it supports. It may be relinked to the kernel, registered to the system or installed on the fly without rebooting. Controller is operated through the device register, which forms the _I/O port space_.

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

## Ontogeny Recapitulates Phylogeny

## Syscalls

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

## OS Structures

### Monolithic Systems

The most common organization. The entire operating system runs as a single program in kernel mode. The OS is written as a collection of procedures linked together into a single large executable binary program. A crash in any of the procedures will take down the entire OS.

The OS has a basic structure:

- A main program that invokes the requested service procedure;

- A set of service procedures that carry out the syscalls;

- a set of utility procedures that help the service procedures.

### Layered Systems

A more generalized approach is to organize the OS as a hierarchy of layers, each one constructed upon the one below it.

### Microkernels

Puts as little as possible in kernel mode. The basic idea behind the microkernel design is to achieve high reliability by splitting the operating system up into small, well-defined modules, only one of which—the microkernel—runs in kernel mode and the rest run as relatively powerless ordinary user processes, in particular, running each device driver and file system as a separate user process.

Common desktop operating systems do not use microkernels. However, they are dominant in real-time, industrial, avionics, and military applications that are mission critical and have very high reliability requirements.

The MINIX3 OS has a microkernel that handles interrupts, processes, scheduling, interprocess communication, with a set of kernel calls and the clock driver. Outside the kernel, the system is structured as three layers of processes all running in user mode:

- Drivers

- Servers: do most of the work of the OS. File servers manage the file systems; the process manager creates, destroys, and manages processes. the reincarnation server checks if the other servers and drivers are functioning correctly and replaces a faulty one.

- User Programs

This puts the mechanism for doing something in the kernel and let user-mode processes handle the policy.

### Virtual Machines

In order to run virtual machine software on a computer, its CPU must be virtualizable. It is essential that the hardware trap to the virtual machine monitor so that instruction can be emulated in software.

- type 1 hypervisor

- type 2 hypervisor: running on top of a host OS 

Practical hypervisors use a hybrid strategy. They add a kernel module to do the heavy lifting.

- Paravirtualization

- The Java virtual machine

### Exokernels

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

- Process Control: create process; terminate process; load, execute; get/set process attributes; wait event, signal event; allocate/free memory

- File Management: create/delete/open/close/read/write/ file; retrieve/set file attributes

- Device management: request/release/read/write/ device; attach/detach devices

- information maintenance: date, time, manage system data;

- commnications: create/close communication; send/receive messages; transfer status information; attach/detach remote devices

- protection
