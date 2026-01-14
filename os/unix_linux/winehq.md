# Use

## Use without X

By default `wine` starts `explorer.exe`. 
To disable this, set `WINEDLLOVERRIDES="explorer.exe=d"`.

## Fonts

One may link linux fonts to `$WINEPREFIX/drive_c/windows/Fonts`.
and set `FREETYPE_PROPERTIES="truetype:interpreter-version=35"`

There are other font rendering configuration, refer to archwiki.

# Architecture

`wineserver` provides services, which implement basic Windows functionality, integrates with the X windowing system, and translation of signals into native Windows exceptions. Wine allows for loading both Windows DLLs and Unix shared objects for its Windows programs. Wine has builtin implementation of the most basic Windows DLLs, namely `ntdll`, `kernel32`, `gdi32`, `user32`.

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

## Wine Prefix

a virtual Windows fileystem containing a `C:` drive asnd a bunch of other files to mimic a real Windows installation.

```shell
.wine
├── dosdevices                     # DOS devices in the form of symlinks to folders or special devices
│  ├── c: -> ../drive_c
│  ├── com1 -> /dev/ttyS0
│  ├── com2 -> /dev/ttyS1
│  └── z: -> /
├── drive_c
│  ├── ProgramData
│  ├── Program Files
│  ├── Program Files (x86)
│  ├── users
│  └── windows
├── system.reg                     # real registry files
├── user.reg
└── userdef.reg
```

## DLL Components

- Built-in: Wine-created DLLs are

- Native: Microsoft-owned DLLs

Some DLLs are (were?) not implemented as PE files but simply loads the real ELF libraries.

## Virtual Desktop

A window that mimics the behavior of the Windows desktop/screen and is useful to run fullscreen apps.


# How To Debug

The developer has to guess what the application is trying to do and figure out why Wine is failing to meet its requirements.
Most Wine debugging is done with printf-debugging, controlled by `WINEDEBUG`, which takes a comma-separated list of debug channels (log sources, typically a software component, but special channels `tid`, `pid`, `relay`, `timestamp` exist to output certain info).
The basic Wine debugging loop is to run the app with logging enabled, demonstrate the failure, examine the log, add more logging if necessary and repeat until the problem is solved.

Logs serveries are 

- `TRACE`: general debugging and silenced by default

- `WARN`: suspicious but non-fatal condition; silenced by default.

- `FIXME`: some features are missing; printed by default

- `ERR`: a serious problem and will also be printed by default.

Grep for `err:`, `warn:`, `fixme:` that might be related to the issues to solve. Add `+seh` to the debug channel list to debug bad pointer dereferences. Go back from the crash site and hopefully find some `fixme:` or `err:` or some failed `HRESULT` to be the causes.

# The Source Tree

Windows components are placed in `dlls`, `programs`, `servers`.

- `dlls/ntdll`: where the core OS APIs are implemented

- `dlls/kernel32`: the user typically access OS functionality through this

- `dlls/user32`: GUI handling

- `dlls/winex11.drv`, `dlls/winemac.drv`: map Windows GUI interfaces and other platform-specific functions to the native platform.

- `dlls/d3d**` and `dlls/wined3d`: the Direct3D graphics API on top of OpenGL.

- `programs/services`: background service manager

- `programs/wineboot`: bootstrap the initial prefix creation and other tasks when a prefix is booted.

- `programs/winecfg`: the wine configuration program

- `server`: all corss-process functionality including message routing, multi-process synchronization primitives, registry handling and much more.
