# Architecture

`wneserver` provides services, which implement basic Windows functionality, integrates with the X windowing system, and translation of signals into native Windows exceptions. Wine allows for loading both Windows DLLs and Unix shared objects for its Windows programs. Wine has builtin implementation of the most basic Windows DLLs, namely `ntdll`, `kernel32`, `gdi32`, `user32`.

https://wiki.winehq.org/Wine_Developer%27s_Guide/Architecture_Overview

```
+---------------------+                                  \
|     Windows EXE     |                                   } application
+---------------------+                                  /

+---------+ +---------+                                  \
| Windows | | Windows |                                   \ application & system DLLs
|   DLL   | |   DLL   |                                   /
+---------+ +---------+                                  /

+---------+ +---------+     +-----------+  +--------+  \
|  GDI32  | |  USER32 |     |           |  |        |   \
|   DLL   | |   DLL   |     |           |  |  Wine  |    \
+---------+ +---------+     |           |  | Server |     \ core system DLLs
+---------------------+     |           |  |        |     / (on the left side)
|    Kernel32 DLL     |     | Subsystem |  | NT-like|    /
|  (Win32 subsystem)  |     |Posix, OS/2|  | Kernel |   /
+---------------------+     +-----------+  |        |  / 
                                           |        |
+---------------------------------------+  |        |
|                 NTDLL                 |  |        |
+---------------------------------------+  +--------+

+---------------------------------------+               \
|            Wine executable            |                } unix executable
+---------------------------------------+               /
+---------------------------------------------------+   \
|                   Wine drivers                    |    } Wine specific DLLs
+---------------------------------------------------+   /

+------------+    +------------+     +--------------+   \
|    libc    |    |   libX11   |     |  other libs  |    } unix shared libraries
+------------+    +------------+     +--------------+   /  (user space)

+---------------------------------------------------+   \
|         Unix kernel (Linux,*BSD,Solaris,OS/X)     |    } (Unix) kernel space
+---------------------------------------------------+   /
+---------------------------------------------------+   \
|                 Unix device drivers               |    } Unix drivers (kernel space)
+---------------------------------------------------+   /
```

- `wineserver`: IPC, synchronization, process/thread management. It creates a Unix socket so that processes can communicate with it. All the clients' Win32 objects are managed by the `wineserver`.

- builtin DLLs: each DLL is implemented in a Unix shared library. The DLL descriptors of these DLLs create an in-memory PE header.

Every Win32 process in Wine has its own dedicated native process on the host system, and therefore its own address space. 

https://wiki.winehq.org/Wine_Developer%27s_Guide/Kernel_modules
