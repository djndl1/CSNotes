Operating system provides user programs with a better, simpler, cleaner, model of the computer.

Most computers have two modes of operation: _kernel mode_ and _user mode_. The operating system runs in kernel mode (also called [supervisor mode](https://en.wikipedia.org/wiki/Protection_ring#Supervisor_mode), usually represented as a bit in the program status word), in which, it has access to all the hardware and can execute any instructions the machine is capable of executing. It is sometimes difficult to draw a line between kernel space and user space. Some OSes, specially embedded one, do not have this distinction. Many sensitive OS-related programs run in user mode.

Operating systems perform two essentially unrelated functions: 

- providing application programmers a clean abstract set of resources instead of the messy hardware ones; OS works as an extended machine. The abstract is the key to managing all the complexity.

- managing hardware resources. OS works as a resource manager. Resource management includes multiplexing resources in time (CPU multiprocessing) and in space (virtual memory).

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

- Hierarchical architecture; L1 cache: inside the CPU and usually feeds decoded instructions into the CPU's execution engine; L2: several megabytes of recently used memory words.

## Disks

- Surface, cylinder, track, sector

SSDs are not disks at all, they are flash memory.

<<<<<<< HEAD
=======
## I/O Devices

I/O devices generally consist of two parts: a _controller_ and the device itself. The controller presents a simpler interface to the OS. Each controller has a _device driver_. Each controller manufacturer has to supply a driver for each OS it supports. It may be relinked to the kernel, registered to the system or installed on the fly without rebooting. Controller is operated through the device register, which forms the _I/O port space_.

Interrupt machanism.

DMA chip can control the flow of bits between memory and some controller without constant CPU intervention.
>>>>>>> refs/remotes/origin/master


## Bus

The main bus is PCIe bus. It has multiple serial lanes that transport different messages, instead of different bits of the same message. The CPU talks to memory over a DDR3 bus, to an external graphics device over PCIe, and to all other devices via a hub over a DMI (Direct Media Interface) bus. 

Intel and M$ designed plug and play, which has the system automatically collect information about the I/O devices, centrally assign interrupt levels and I/O addresses, and then tells each device what its numbers are.

## Booting

BIOS - Self-Check - find boot devices - bootloader - load the OS
