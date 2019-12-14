# Linux

## History

MULTICS -> UNICS (UNIX) -> PDP-11 UNIX in C -> Protable UNIX and C compiler -> System V

1BSD - 2BSD - 3BSD (for VAX) - 4BSD (virtual memory, paging, longer file names, TCP/IP)

Standard UNIX: various unices by end of the 1980s. POSIX by IEEE. The IEEE committee took the intersection of all the features from various unices.

MINIX: functionally equivalent to Version 7 UNIX; one of the first UNIX-like systems based on a microkernel design. Microkernel design didn't affect performance much.

Linux: initially as a rewrite of MINIX, grew into a full, production UNIX clone. Linux makes use of many special features of GCC (Clang 10 now compiles the mainline Linux kernel). Linux 1.0 in 1994 was sufficiently compatible with UNIX. Linux 2.0 in 1996 included support for 64-bit architectures, symmetric multiprogramming, new networking protocols and numerous other features.

## Overview

UNIX was designed by programmers, for programmers, to use in an environment in which the majority of the users are relatively sophisticated and are engaged in software development projects. The model of a group of experienced programmers working together closely to produce advanced software is obviously very different from the personal-computer model of a single beginner working alone with a word processor, and this difference is reflected throughout UNIX from start to finish.

### Kernel Structure

```
                       +-----------------------------------------------------------+
+----------------------+                      System Calls                         +-----------------+
|                      +-----------------------------------------------------------+                 |
|                                                                                                    |
|                                                                                                    |
|                  I/O Component                           Memory Managment       Process Management |
|                                                             Component                              |
|  +--------------------------------------------+         +----------------+     +---------------+   |
|  |    +-------------------------------+       |         |                |     |               |   |
|  |    |      Virtual File System      |       |         |   Virtual      |     |   Signal      |   |
|  |    +-------------------------------+       |         |   Memory       |     |   Handling    |   |
|  |                                            |         |                |     |               |   |
|  | +--------+   +---------+  +--------------+ |         +----------------+     +---------------+   |
|  | |Terminal|   |         |  |    File      | |         |                |     |               |   |
|  | |        |   | Sockets |  |   systems    | |         |    Paging      |     |Process/thread |   |
|  | +--------+   |         |  +--------------+ |         |     page       |     |   creation &  |   |
|  | | line   |   +---------+  |   Generic    | |         |    replacement |     |  termination  |   |
|  | |disci-  |   | Network |  |  block layer | |         |                |     |               |   |
|  | |line    |   |protocols|  +--------------+ |         |                |     |               |   |
|  | +--------+   +---------+  | I/O Scheduler| |         +----------------+     +---------------+   |
|  | |Character   | Network |  +--------------+ |         |                |     |               |   |
|  | | Device |   | device  |  | Block Device | |         |     Page       |     |               |   |
|  | | Drivers|   | drivers |  |   Drivers    | |         |                |     |      CPU      |   |
|  | +--------+   +---------+  +--------------+ |         |     Cache      |     |               |   |
|  |                                            |         |                |     |  Scheduling   |   |
|  |                                            |         |                |     |               |   |
|  +--------------------------------------------+         +----------------+     +---------------+   |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|               +--------------------------------------------------------------------+               |
|               |       Interrupt                          Dispatcher                |               |
|               +--------------------------------------------------------------------+               |
+----------------------------------------------------------------------------------------------------+
```

Interrupt handlers are the primary way for interacting with devices. The dispatching occurs when an interrupt happens. I/O operations all integrated under a VFS.


## Processes

parent/child hierarchy; PID; daemon process; pipes; signal; process group; 

In most cases, after a fork, the child will need to execute different code from the parent. It uses the `exec` syscall which causes its entire core image to be replaced by the file named in its first paramter. Several signal-related syscalls exist for setting signal handlers, sending signals. 

### Implementation

The Linux kernel internally represents processes as _tasks_, via the structure `task_struct`. Linux uses this task structure to represent any execution context. A multithreaded process has one task structure for each of the user-level threads. The kernel itself is multithreaded, and has kernel-level threads which are not associated with any user process and are executing kernel code. The kernel organizes all processes in a doubly linked list of task structures. The information in the process descriptor falls into a number of broad categories that can be roughly described:

1. scheduling parameters: process priority, amount of CPU time consumed recently, amount of time spent sleeping recently.

2. memory image: pointer to the text, data, and stack segments, or page tables; information about whether it's on disk and how to find its parts on disk.

3. signals: masks showing which signals are being ignored, which are being caught, which are being temporarily blocked, and which are in the process of being delivered.

4. machine registers: the machine registers are saved here

5. syscall state: information about the current syscall, including the paramter and results

6. file descriptor table

7. accounting: pointer to a table that keeps track of the user and system CPU time used by the process.

8. kernel stack: a fixed stack for use by the kernel part of the process

9. miscellaneous: current process state, event being waited for, time until alarm clock goes off, PID, PID of the parent process, and user and group identification

For a process to be created, a new process descriptor and user area are created for the child process and filled in largely from the parent. The child is gen a PID, its memory map is set up, and it is given shared access to its parent's files. Then its registers are set up and it is ready to run. The PID hash table entry points to the new task structure. The new process' memory image is shared with the parent and is copied on write to save RAM. After `exec`ing, the old address space and its page tables are replaced by a new empty page table. The executable file is memory mapped. When the new process starts to run, a page fault brings the first page of the executable into memory. The arguments and environment strings are copied to the new stack, the signals are reset and the registers are initialized to all zeros.

#### Threads in Linux

The `clone` syscall blurred the distinction between processes and threads and possibly even inverted the primacy of the two concepts. It provides fine grained control over how a new thread is created, whether it shares different resources with the calling thread.

This fine-grained sharing is possible because Linux maintains separate data structures for various items in the task structure. A new task structure has these item pointer either point to the old thread's scheduling, memory and other data structures. Both PID and TID fields are stored in the tasks structure.

```bash
man 2 clone
```

More see "Linux thrading model compared: LinuxThreads and NPTL"

Linux distinguishes three classes of threads for scheduling purposes:

The first two classes have priority of 0-99

1. read-time FIFO: highest priority, not preemptible by a newly readied real-time FIFO with even higher priority

2. real-time round robin: preemptible; 

3. timesharing: conventional threads form a separate class and are scheduled by a separate algorithm so they do not compete with the real-time threads, with priority level from 100 to 139.

In Linux, time is measured as the number of clock ticks. Originally, each tick was 1ms, called a _jiffy_. The tick now can be configured to 500, 250 or even 1Hz. The kernel can be configured in tickless mode, useful when there is only one process running in the system or when the CPU is idle and needs to go into power-saving mode. On newer systems, high-resolution timers allow the kernel to keep track of time in sub-jiffy granularity.

Nice value (ranging from -20 to 19)defaults to 0. Only the sysadmin can ask for better (-20 to -1).

- _runqueue_: a key data structure used by the scheduler to track all the runnable tasks in the system and select the next one to run.

- **O(1) scheduler**: performs task-management operations on the runqueue in constant time, independent of the total number of tasks in the system. The runqueue is orgainized in two arrays, _active_ and _expired_, each of which is an array of 140 (priority level) doubly-linked-list heads. The scheduler selects a task from the highest-priority list in the active array. If a task's quantum expires, it is moved to the expired list. Blocking tasks were placed back in the active array when the expected even occurs. When there are no more tasks in the active array, the scheduler simply swaps the pointers of two arrays. Different priority levels are assigned different quanta. Linux distinguishes between static and dynamic priority. A thread's dynamic priority is continuously recalculated, so as to reward interactive threads, and punish CPU-hogging threads. In O(1), bonus/penalty ranges from -5 to 5. A `sleep_avg` variable is maintained to determine the bonus. O(1) resulted in poor performance for interactive tasks.

- **Completely Fair Scheduler** (**CFS**): uses a red-black tree as the runqueue data structure. Tasks are ordered in the tree based on the amount of time they spend running on the CPU (_vruntime_). The children on the left had less time on the CPU. CFS always schedules the task which has had the least amount of time on the CPU. CFS increments the task's vruntime and compares with the leftmost node to decide if it will continue to run or otherwise to be inserted at the appropriate place in the tree. The priorities and niceness are represented through the different effective rate at which a task's virtual time passes when it is running on the CPU. Selecting a node to run can be done in constant time whereas inserting a task in the runqueue is done in $O(\log (N))$ time.

The Linux scheduler includes special features particularly useful multiprocessor or multicore platforms. The runqueue structure is associated with each CPU in the multiprocessing platform. A set of syscalls are available to further specify or modify the affinity requirements of a select thread. The scheduler performs periodic load balancing across runqueues of different CPUs to ensure that the system load is well balanced.

The scheduler considers only runnable tasks in the runqueue. Tasks that are not runnable and are waiting on various events are placed on _waitqueue_. A waitqueue is associated with each event that task may wait on. The head of the waitqueue includes a pointer to a linked list and a spinlock, necessary to ensure that the waitqueue can be concurrently manipulated.

### Synchronization

At the lowest level, Linux provides wrappers around the hardware-supported atomic instructions. Linux provides memory barriers.

Spinlock; mutexes; semaphores; futexes; completions; RCU locks etc.

## Memory Management

In order to avoid allocating a physical page frame full of zeros, during initialization, Linux allocates a static _zero page_, a write-protected page full of zeros for bss. Whenever a process actually attempts to write in this area, an actual page frame is allocated to the process. The data segment can change. The data segment can grow an shrink during execution (`brk`/`sbrk`).

When a program starts up, the stack is not empty. It contains all the env-var as well as the command line typed to the shell to invoke.

Linux supports shared text segments so that two processes can share the same piece of text. Data and stack segments are never shared except after a fork.

Processes in Linux can access file data through memory-mapped files. Mapping a file in makes random access to it much easier than using I/O syscalls. Shared libraries are accessed by mapping them in using this mechanism. A file can be mapped into multiple processes at the same time. Writes to the file by any one of them are them instantly visible to the others.

The most used Linux syscalls for memory management is `sbrk`/`brk`, `mmap`/`munmap`

### Implementation

32-bit Linux process typically gets 3GB of virtual process for user space with the remaining 1GB for page tables and other kernel data.

#### Physical Memory

Linux maintains node descriptors. Each node descriptor contains information about the memory usage and zones on that particular node. on UMA platforms, all memory is decribed via one node descriptor. The first few bits within each page descriptor are used to identify the node descriptor.

Linux distinguishes between the following memory zones and maintains a `zone` structure for each of three zones:

1. `ZONE_DMA` and `ZONE_DMA32`: pages that can be used for DMA;

2. `ZONE_NORMAL`: normal, regularly mapped pages;

3. `ZONE_HIGHMEM`: pages with high-memory addresses, which are not permanently mapped. Not defined on x86_64 machines.

For each zone Linux maintains a zone descriptor: memory utilization, an array of free areas.

Main memory in Linux consists of three parts. The first two steps, the _kernel_ and _memory map_ are pinned in memory, never paged out. The rest of memory is divided into page frames, each of which can contain a text, data, or stack page, a page-table page, or be on free list. 

The kernel maintains a map of the main memory which contains all information about the use of the physical memory in the system, such as its zones, free page frames and so forth.

Linux maintains an array `mem_map` of _page descriptors_ (`page`), one for each physical page frame. A page descriptor contains a pointer to the address space that it belongs to and a pair of pointers that allow it to form doubly-linked lists with other descriptors (e.g. to keep together all free page frames) and a few other fields.

Linux makes use of a four-level paging scheme for 32- and 64-bit architectures. On machines supporting fewer levels, the upper levels have only one entry in a table of size zero. 

The kernel itself is hardwired, no part of it never paged out. 

- page cache: holding pages containing file blocks that have recently been read or have been read in advance in expectation of being used in the near future, or page of file blocks which need to be written to disk. Dynamic in size and competes for the same pool of pages as the user processes. The set of user page that are no longer needed and are waiting around to be paged out.

# Android

The open source part of Android is **Android Open Source Project** (**AOSP**). _Compatibility Definition Document_ describes the way Android must behave to be compatible with third party applications. It describes what is required to be a compatible Android device. Google provides some proprietary services, especially Google Play, to ensure that applications will work on the device it delivers to.

## Architecture

- `init` starts as the first user process. It spawns `zygote`, which is the root of the higher-level Java language processes. The daemon `adbd` listens for remote connections that request shell access. Other service started include `system_server`, which contains all of the core operating system services. Applications interact with the OS through calls to libraries, which together compose the _Android Framework_.

## Linux Extensions



# Windows 8

## History

- MS-DOS: 16-bit real-mode, single-user, command-line-oriented OS consisting of 8KB of memory resident code.

- MS-DOS Windows: a graphical inteface for MS-DOS. Before Win 95, all programs ran in the same address space and a bug in any one of them could bring the whole system to a halt. Win 95 added virtual memory, process management, multiprogramming and 32-bit programming interfaces. It had poor isolation between applications and the OS.

- NT-based Windows: initially as NT OS/2. Designed to be portable across different processors and emphasized security and reliability, as well as compatibility with the MS-DOS based versions of Windows. NT was designed in an era when multiprocessor, 32-bit, multimegabytes, virtual memory systems were common. In NT, threads are the units of concurrency, dynamic libraries are the units of composition. NT's early success was in the server market, competing with VMS and NetWare. The compatibility with Win95 made it easier for users to migrate to NT. Windows 2000 added plug-and-play, network directory services, improved power management and improved GUI. Windows XP ended the era of MS-DOS based Windows.

- Windows Vista and 7: .NET, WinFS, over 70 million lines. Backward-compatibility makes Windows bloated.

- Modern Windows (from Windows 8): Microsoft began a process to redesign itself as a _devices and services_ company. Fundamental changes in the programming models.

## Windows Programming Models

```
       Modern Windows Apps               Windows Services                Windows Desktop Apps

     +--------------------+            +--------------------+       +---------------------------+
     | Modern app mgr     |            |      Modern        |       |  Desktop Manager(explorer)|
     +--------------------+            | broker processes   |       +---------------------------+
     |WinRT:.NET/C++, WWA |            +--------------------+       |  .NET: base classes, GC   |
     +--------------------+            |NT services, smss,  |       +---------------------------+
     |        COM         |            |lsass, services     |       |GUI :shell32, user32, gdi32|
     +--------------------+            |      winlogon      |       +---------------------------+
     |    AppContainer    |            +--------------------+       |dynamic libraries (ole,rpc)|
     +--------------------+            |  Win32 subsystem   |       +---------------------------+
     |Process Lifetime mgr|            |process (csrss.exe) |       |Subsystem API (kernel32)   |
     +--------------------+            +--------------------+       +---------------------------+

                    +--------------------------------------------------------+
                    |       Native NT API, C/C++ run|time (ntdll.dll)        |
    User mode       +--------------------------------------------------------+
   +-----------------------------------------------------------------------------------------------------+
    kernel mode     +--------------------------------------------------------+
                    |           NTOS kernel layer (ntoskrnl.exe)             |
                    +--------------------------------------------------------+

      +----------------------+      +---------------------+  +----------------------+
      | Drivers: devices,    |      |                     |  |                      |
      | file systems         |      | NTOS executive layer|  |      GUI driver      |
      |     network          |      |    (ntoskrnl.exe)   |  |      win32k.exe      |
      |                      |      |                     |  |                      |
      |                      |      |                     |  |                      |
      +----------------------+      +---------------------+  +----------------------+

         +---------------------------------------------------------------------------+
         |                    Hardware abstraction layer (hal.dll)                   |
         +---------------------------------------------------------------------------+

+--------------------------------------------------------------------------------------------------------+
     +-----------------------------------------------------------------------------------+
     |                              Hypervisor (hvix, hvax)                              |
     +-----------------------------------------------------------------------------------+
```

Windows includes a number of programming interfaces which are implemented as services that run as separate processes. Applications communicate with user-mode services through RPCs.

- NTOS kernel-mode program (`ntoskrnl.exe`): traditional syscall interfaces upon which the rest of the OS is built. Only programmers at Microsoft write to the syscall layer. The published user-mode interfaces all belong to operating system personalities that are implemented using _subsystems_ that run on top of the NTOS layers.

OS/2, POSIX subsystems have been removed. All windows applications are written using APIs that are built on top of the Win32 subsystem, such as WinFX in the .NET model.

- WinRT: Traditional desktop experience was deprecated in favor of running a single application at a time on the full screen with an emphasis on touch over use of the mouse. These APIs have versions available for C++ and .NET programs but also Javascript for applications hosted in a browser-like environment `wwa.exe`. _Modern Software Development Kit_ (MSDK), a new API model, included and omitted many Win32 APIs to encourage this new programming model. The processes running modern applications have their lifetimes managed by the OS. Applications that need to run tasks in the background must specifically arrange to do so using a new set of WinRT APIs. These changes were made to make Windows function better o mobile devices. Modern applications have to be installed using Windows' AppStore, following the same model introduced by Apple. A modern application runs in a sandbox called an _AppContainer_. The Windows AppContainer treats each application as a distinct user and uses Windows security facilities to keep the application from accessing arbitrary system resources. When an application needs access to a system resouce, there are WinRT APIs that communicates to _broker processes_ which have access to more of the system.

NT subsystems are constructed out of four components:

- a subsystem service process `csrss.exe` for Win32, started by `smss.exe` (session manager), the initial user-mode program started by NT.

- a set of libraries: higher-level OS functions specific to the subsystem, stub routines which communicate between processes using the subsystem and the subsystem process itself. Calls to the subsystem process normally take place using the kernel-mode Local Procedure Call facilities, which implement cross-process procedure calls.

- hooks

- support in the kernel: general-purpose facilities that can be used writing operating-system-specific subsystems.

NTOS's syscalls are implemented in the NTOS executive layer. Most of the native NT syscalls operate on kernel-mode objects of one kine or another, including files, processes, threads, pipes, semaphores, and so on. Every call creating or opening an object returns a result called a _handle_ to the caller. Every object has a _security descriptor_, specifying the kinds of operations on the object. Kernel objects in Windows use a uniform facility based on handles and names in the NT namespace (a hierarchical tree-structured collection of directories, symbolic links and objects)  to reference kernel objects, along with a unified implementation in a centralized _object manager_. The object manager provides unified facilities for synchronization, security, and object lifetime management. The root of the NT namespace is maintained in the kernel's virtual memory. A named object can be marked _permanent_ so that it continues to exist until explicitly deleted or the system reboots.

Most of the functionality of the native NT APIs are provided through the Win32 API. Win32 API has poor layering that intermixes both high-level and low-level functions in the same API. Memory-mapping files are notable. TODO

Each filesystem volume is implicitly mounted in the NT namespace. The low-level I/O model in Windows is fundamentally asynchronous.

Every thread has a _token_, which provides information about the identity and privileges associated with the thread.

Windows attaches a special kind of filesystem to the NT namespace called the _registry_. The registry is organized into separate volumes called _hives_. Each hive is kept in a separate file. The SYSTEM hive is loaded into memory by the system boot program when booting. It keeps a great deal of crucial information in SYSTEM hive. The registry has become seriously disorganized over time as Windows has evolved. The registry is a strange cross between a file system and a database.

## System Structure

```
                            +------------------------------------------------------------------------------------------------+
                            |       System library kernel user-mode dispatch routines (ntdll.dll)                            |
               User mode    +------------------------------------------------------------------------------------------------+

+------------------------------------------------------------------------------------------------------------------------------------------+

                +------------------------------------------------------------------------------------------------------------+
   Kernel mode  |             +--------------------------------------------------------------------------------------------+ |
                | NTOS        |                         Trap/exception/interrupt dispatch                                  | |
                | kernel      +--------------------------------------------------------------------------------------------+ |
                | layer       +--------------------------------------------------------------------------------------------+ |
                |             |                CPU scheduling and synchronization: threads, ISRs, DPCs, APCs               | |
                |             +--------------------------------------------------------------------------------------------+ |
                +------------------------------------------------------------------------------------------------------------+

                +-----------------+  +---------------------------------------------------------------------------------------+
                |      Drivers    |  | NTOS executive layer                                                                  |
                |                 |  |                                                                                       |
                | File systems    |  | +----------------------+ +---------------------+ +---------------+ +----------------+ |
                |                 |  | |Processes and threads | |   Virtual memory    | | Object Manager| |  Config manager| |
                | volume manager  |  | +----------------------+ +---------------------+ +---------------+ +----------------+ |
                |                 |  |                                                                                       |
                | TCP/IP stack    |  | +----------------------+ +---------------------+ +---------------+ +----------------+ |
                |                 |  | |        LPC           | |    Cache Manager    | |   I/O manager | |Security Monitor| |
                | net interfaces  |  | +----------------------+ +---------------------+ +---------------+ +----------------+ |
                |                 |  | +-----------------------------------------------------------------------------------+ |
                | graphics devices|  | |                          Executive run-time library                              | |
                | and others      |  | +-----------------------------------------------------------------------------------+ |
                +-----------------+  +---------------------------------------------------------------------------------------+

                +------------------------------------------------------------------------------------------------------------+
                |                                   Hardware abstraction layer                                               |
                +------------------------------------------------------------------------------------------------------------+

+-------------------------------------------------------------------------------------------------------------------------------------------+

    Hardware    +------------------------------------------------------------------------------------------------------------+
                |                    CPU, MMU, interrupt controller, memory, physical devices, BIOS                          |
                +------------------------------------------------------------------------------------------------------------+

```

- `ntdll.dll`: a number of support functions for the compiler run-time and low-level libraries, similar to `libc` in Unix. It also contains special code entry points used by the kernel to initialize threads and dispatch exceptions and user-mode APCs (Asynchronous Procedure Calls). Every user-mode process created by NTOS has `ntdll` mapped at the same fixed address.

- _Hardware abstraction layer_: abstracts low-level hardware details like access to device registers  and DMA operations and the way parentboard firmware represents configuration information and deals with differences in the CPU support chips, such as various interrupts controllers. The HAL abstractions are presented in the form of machine-independent services that NTOS and the driver can use. The HAL provices a service for identifying devices by mapping bus-relative device addresses onto systemwide logical addresses.  It also provides services to name interrupts in a systemwide way and also provides ways to allow drivers to attach interrupt service routines to interrupt in a portable way. It sets up and manages DMA transfers in a device-independent way. The time services decouple the drivers from the actual frequencies at which the clocks run. The HAL provides primitives to manage synchronizaton in multiprocessor systems. The HAL talks to BIOS and inspects the system configuration to find out which buses and I/O devices the system contains and how they have been configured after booting, and put into the registry.

- Hyper-V hypervisor: optional

- device drivers: Windows uses device drivers for any kernel-mode facilities which are not part of NTOS or HAL, including filesystems, network protocol stacks, and kernel extensions like antivirus and DRM software as well as drivers for managing physical devices, interfacing to hardware buses and so on. The I/O and virtual memory components cooperate to load/unload device drivers into kernel memory and link them to the NTOS and HAL layers. Device drivers in Windows are dynamic link libraries which are loaded by the NTOS executive. Much of the Win32 subsystem is loaded as a driver.

NTOS (running in kernel mode) consists of two layers

- _executive_: contains most of the services. It runs using the control abstractions provided by the kernel layer.

- _kernel_ : implements the underlying thread scheduling and synchronization abstractions. The term `kernel` may mean `ntoskrnl.exe` file (the kernel layer and the executive layer), the kernel layer within NTOS, or Win32 library `kernel32.dll` or all the code that runs in kernel mode. The kernel layer provides a set of abstractions for managing the CPU. The most central abstraction is threads, the kernel also implements exception handling, traps and several kinds of interrupts. The kernel layer is responsible for scheduling and synchronization of threads. The executive layer can be implemented using the same preemptive multithreading model used to write concurrent code in user mode. The kernel layer is also responsible for providing low-level support for _control objects_ (data structures that the kernel layer provides as abstractions as the executive layer for managing the CPU) and _dispatch objects_ (the class of ordinary executive objects that use a common data structure for synchronization).

### Booting

```
                                                                    +-------------+
                                                        +---------->+WinResume.exe|
                                                        |           +-------------+
+------------+              +------------+              |
|            |              |            | hibernated   |
|  Firmware  +------------->+  BootMgr   +------------->+                                    +-------------+
|            |              |            |              |                                    |             |
+------------+              +------------+              |                                    | ntoskrnl.exe|           +---------+
                                                        |           +-------------+          |             |           |         |
                                                        +---------->+ WinLoad.exe +--------->+    hal.dll  +---------->+ smss.exe|
                                                                    +-------------+          |             |           |         |
                                                                                             | SYSTEM hive |           +---------+
                                                                                             |             |
                                                                                             | win32k.sys  |
                                                                                             |             |
                                                                                             |other drivers|
                                                                                             |             |
                                                                                             |hypervisor if|
                                                                                             |  enabled    |
                                                                                             +-------------+

```
