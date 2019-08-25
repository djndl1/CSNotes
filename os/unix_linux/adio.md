# Nonblocking I/O

The slow system calls are those that can block forever

- reads that can block the caller forever if data isn't present with certain file type (pipes, terminal devices, and network devices)

- writes that can block the caller  forever if the data can't be accepted immediately by these same file types

- Opens that block until some condition occurs on certain file types.

- reads and writes of files that have mandatory record locking enabled.

- Certain `ioctl` operations

- Some of the interprocess communication functions

With nonblocking I/O, if the operation cannot be completed, the call returns immediately with an error noting that the operation would have blocked.

- `open()` with `O_NONBLOCK` flag

- `fcntl` turns on `O_NONBLOCK`.

Sometimes, we can avoid using nonblocking I/O by designing our applications to use multiple threads. However, the overhead of synchronization can add more complexity than is saved from using threads.

# Record Locking (byte-range locking)

Record locking is the term normamlly used to describe the ability of a process to prevent other processes from modifying a region of a file while the first process is reading or modifying that portion of a file. It is useful for running database systems.

TODO

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
