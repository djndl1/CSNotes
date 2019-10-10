# Nonblocking I/O

The slow system calls are those that can block forever

- reads that can block the caller forever if data isn't present with certain file type (pipes, terminal devices, and network devices)

- writes that can block the caller  forever if the data can't be accepted immediately by these same file types

- Opens that block until some condition occurs on certain file types.

- reads and writes of files that have mandatory record locking enabled.

- Certain `ioctl` operations

- Some of the interprocess communication functions

With nonblocking I/O, if the operation cannot be completed, the call returns immediately with an error noting that the operation would have blocked. If the IO cannot be done immediately and the program keeps trying, this type of looping is _polling_.

- `open()` with `O_NONBLOCK` flag

- `fcntl()` turns on `O_NONBLOCK`.

Sometimes, we can avoid using nonblocking I/O by designing our applications to use multiple threads. However, the overhead of synchronization can add more complexity than is saved from using threads. Individual threads block in I/O calls while other threads continue to work.

# Record Locking (byte-range locking)

Record locking is the term normamlly used to describe the ability of a process to prevent other processes from modifying a region of a file while the first process is reading or modifying that portion of a file. It is useful for running database systems.

Record  locks  are  not inherited by a child created via `fork`, but are preserved across an `execve`. Because of the buffering performed by the stdio library,  the  use  of record  locking  with  routines  in  that  package should be avoided; use `read` and `write`instead.

 If a process closes any file descriptor referring to a file, then  all of  the  process's  locks on that file are released, regardless of the file descriptor(s) on which the locks were obtained. The threads in a process share locks.  In other words, a multithreaded program can't use record locking to ensure that threads don't simultaneously access the same region of a file. Record locks  are  automatically released when the process terminates.


`fcntl` provides advisory record locking, with `F_SETLK`, `F_SETLKW` and `F_GETLK` as commands:

```c
struct flock {
     ...
     short l_type;    /* Type of lock: F_RDLCK,
                         F_WRLCK, F_UNLCK */
     short l_whence;  /* How to interpret l_start:
                         SEEK_SET, SEEK_CUR, SEEK_END */
     off_t l_start;   /* Starting offset for lock */
     off_t l_len;     /* Number of bytes to lock */
     pid_t l_pid;     /* PID of process blocking our lock
                         (set by F_GETLK and F_OFD_GETLK) */
     ...
};
```

```c
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>


#define read_lock(fd, offset, whence, len) \
        lock_reg((fd), F_SETLK, F_RDLCK, (offset), (whence), (len))

#define readw_lock(fd, offset, whence, len)                          \
        lock_reg((fd), F_SETLKW, F_RDLCK, (offset), (whence), (len))

#define write_lock(fd, offset, whence, len)                          \
        lock_reg((fd), F_SETLK, F_WRLCK, (offset), (whence), (len))

#define writew_lock(fd, offset, whence, len)                         \
        lock_reg((fd), F_SETLKW, F_WRLCK, (offset), (whence), (len))

#define un_lock(fd, offset, whence, len)                         \
        lock_reg((fd), F_SETLK, F_UNLCK, (offset), (whence), (len))

int lock_reg(int fd, int cmd, int type,
             off_t offset, int whence, off_t len)
{
        struct flock  lock;

        lock.l_type = type;
        lock.l_start = offset;
        lock.l_whence = whence;
        lock.l_len = len;

        return (fcntl( fd, cmd, &lock));
}

// cannot be used by a process to see whether it is currently holding a portion of a file locked
pid_t lock_test(int fd, int type, off_t offset, int whence, off_t len)
{
        struct flock  lock;

        lock.l_type = type;
        lock.l_start = offset;
        lock.l_whence = whence;
        lock.l_len = len;
        if (fcntl(fd, F_GETLK, &lock) < 0) {
                fprintf(stderr, "fcntl error\n");
                exit(1);
        }

        if (lock.l_type == F_UNLCK)
                return 0;
        return (lock.l_pid);
}

#define is_read_lockable(fd, offset, whence, len) \
        (lock_test((fd), F_RDLCK, (offset), (whence), (len)) == 0)
#define is_write_lockable(fd, offset, whence, len) \
        (lock_test((fd), F_WRLCK, (offset), (whence), (len)) == 0)
```

Locks are associated with a process and a file. When a process terminates, all its locks are released. Whenever a descriptor is closed, any locks on the file referenced by that descriptor for that process are released.  Locks are never inherited by the child across a `fork`. Locks are meant to prevent multiple processes from writing to the same file at the same time. Locks are inherited by a new program across an `exec`.

Locks can be used to ensure that only one instance of the daemon is running.

```c
#include <unistd.h>
#include <fcntl.h>

int
lockfile(int fd)
{
	struct flock fl;

	fl.l_type = F_WRLCK;
	fl.l_start = 0;
	fl.l_whence = SEEK_SET;
	fl.l_len = 0;
	return(fcntl(fd, F_SETLK, &fl));
}
```

## Mandatory Locking

Mandatory locking (enforcement-mode locking) causes the kernel to check every `open`, `read`, and `write` to verify that the calling process isn't violating a lock on the file being accessed. Mandatory locking is enabled for a particular file by turning on the set-group-ID bit and turning off  the group-execute bit (On Linux `mount -o mand` is needed).

TODO 

# I/O Multiplexing

## Scenario

`telnet` has two pairs of read/write operations. It doesn't know which input to read at a certain time.

### Solutions 

- two processes/threads, each processing a pair of read/write.

- nonblocking I/O polling: wasting CPU

- asynchronous I/O: the kernel signals when an I/O is ready. Portability and limited forms.

- I/O multiplexing

`select()` and `pselect()` allow a program to monitor multiple file  descriptors,  waiting  until  one or more of the file descriptors become "ready" for some class of I/O operation (e.g., input possible).  A file  descriptor  is considered ready if it is possible to perform a corresponding I/O operation (e.g., `read`,  or  a  sufficiently  small  `write`) without blocking. With three file descriptor sets `NULL`, `select` becomes a high-precision `sleep`.

```c
       int select(int nfds, fd_set *readfds, fd_set *writefds,
                  fd_set *exceptfds, struct timeval *timeout);

       void FD_CLR(int fd, fd_set *set);
       int  FD_ISSET(int fd, fd_set *set);
       void FD_SET(int fd, fd_set *set);
       void FD_ZERO(fd_set *set);
```

`poll` is similar to `select`.

```c
       int poll(struct pollfd *fds, nfds_t nfds, int timeout);
```

`select` and `poll` are a synchronous form of notification.

# Asynchronous I/O 

We incur additional complexity when using the POSIX asynchronous I/O interfaces:

- The interfaces involve a lot of extra setup and processing rules 

- recovering from error can be difficult;

- three sources of errors for every asynchronous operation.

## POSIX Asynchronous I/O

The POSIX asynchronous I/O interfaces give us a consistent way to perform asynchronous I/O, regardless of the type of file. The POSIX asynchronous I/O interfaces were originally introduced to provide real-time applications with a way to avoid being blocked while performing I/O operations.

 The `aiocb` ("asynchronous I/O control block") structure defines parameters that control an I/O operation.
 
 ```c
  struct aiocb {
               /* The order of these fields is implementation-dependent */

               int             aio_fildes;     /* File descriptor */
               off_t           aio_offset;     /* File offset */
               volatile void  *aio_buf;        /* Location of buffer */
               size_t          aio_nbytes;     /* Length of transfer */
               int             aio_reqprio;    /* Request priority */
               struct sigevent aio_sigevent;   /* Notification method */
               int             aio_lio_opcode; /* Operation to be performed;
                                                  lio_listio() only */

               /* Various implementation-internal fields not shown */
           };
 ```

We have to provide an explicit offset when performing asynchronous I/O. The asynchronous I/O interfaces don't affect the file offset maintained by the OS.

To perform asynchronous I/O, we need to initialize an AIO control block and call either the `aio_read` function to make an asynchronous read or the `aio_write` function to make an asynchronous write. When these functions return success, the asynchronous I/O request has been queued for processing by the operating system. The return value bears no relation to the result of the actual I/O operation. To determine the completion status of an asynchronous read, write, or synch operation, we need to call the `aio_error` function. If the asynchronous operation succeeded, we can call the `aio_return` function to get the asynchronous operation’s return value. Until the asynchronous operation completes, we need to be careful to avoid calling the aio_return function. The results are undefined until the operation completes. we can call the `aio_suspend` function to block until an operation completes. When we have pending asynchronous I/O operations that we no longer want to complete, we can attempt to cancel them with the `aio_cancel` function.

```c
       int lio_listio(int mode, struct aiocb *const aiocb_list[],
                      int nitems, struct sigevent *sevp);
```

The  `lio_listio()` function initiates the list of I/O operations (whether async or not) described by the array `aiocb_list`.

The `readv` and `writev` functions let us read into and write from multiple noncontiguous buffers in a single function call. These operations are called _scatter read_ and _gather write_.

# Memory-Mapped I/O

Memory-mapped I/O lets us map a file on disk into a buffer in memory so that, when we fetch bytes from the buffer, the corresponding bytes of the file are read. Similarly, when we store data in the buffer, the corresponding bytes are automatically written to the file. This lets us perform I/O without using read or write.

```c
        // map `length` bytes of the content of `fd` starting from `offset`
       void *mmap(void *addr, size_t length, int prot, int flags,
                  int fd, off_t offset);
       int munmap(void *addr, size_t length);
       
       // change the permissions on an existing mapping
       int mprotect(void *addr, size_t len, int prot);
       
    // flushes  changes made to the in-core copy of a file that was mapped into memory back to the filesystem.
       int msync(void *addr, size_t length, int flags);
```

A memory-mapped region is inherited by a child across a `fork`, but not by the new program after `exec`ing.
