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
