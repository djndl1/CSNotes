# I/O

## Five basic IO functions
p
Most file I/O on a UNIX system can be performed using only five functions: `open`, `read`, `write`, `lseek`, `close`. Each `read` and `write` are unbuffered.

To the kernel, all open files of a process are referred to by file descriptors, a nonnegative integer. By convention, file descriptor `0` is the standard input of a process, `1` the standard input, `2` the standard error. It is not a feature of the UNIX kernel. They should be replaced by the symbolic constants `STDIN_FILENO`, `STDOUT_FILENO` and `STDERR_FILENO`. File descriptors range from 0 through `"OPEN_MAX-1` (On Linux 5.1 the value is 1024). 

A file is opened or created by calling either the `open` function or the `openat` function. The file descriptor returned by `open` and `openat` is guaranteed to be the lowest-numbered unused descriptor. `openat` gives threads a way to use relative pathnames to open files in directories other than the CWD and it provides a way to avoid time-of-check-to-time-of-use errors. `O_CREAT` flag creates the file if it doesn't exist.

A new file can also be created by calling the `creat` function. It's equivalent to `open(path, O_WRONLY | O_CREAT | O_TRUNC, mode)`. This syscall is somewhat redundant. One deficiency with `creat` is that the file is opened only for writing.

A open file is closed by calling the `close` function. Closing a file also releases any record locks that the process may have on the file. When a process terminates, all of its open files are closed automatically by the kernel.

Every open file has an associated ‘‘current file offset,’’ normally a non-negative integer that measures the number of bytes from the beginning of the file. Read and write operations normally start at the current file offset and cause the offset to be incremented by the number of bytes read or written. By default, this offset is initialized to 0 when a file is opened, unless the `O_APPEND` option is specified. A open file's offset can be set explicitly by calling `lseek`. We can seek zero bytes from the current position to determine the current offset, or to determine if a file si capable of seeking.

```c
off_t currpos = lseek(fd, 0, SEEK_CUR); // nonseekable files return -1 and set `errno` to ESPIPE
```

The standard input is seekable. For regular files, the offset must be non-negative. The file's offset can be greater than the file's current size, in which case the next `write` will extend the file, creating a hole in the file and is allowed. The hole are read back as 0. The hole may or may not have storage on disk (doesn't allocate disk. `ls -lh` and `du` gives different file sizes). On Linux 5.1 64-bit, `off_t` is by default 64-bit long.

Data is read from an open file with the `read` funciton. The number o bytes actually read might be less than the amount requested. Data is writen to an open file with `write` funciton. A common cause for a write error is either filling up a disk or exceeding the file size limit for a given process.

## I/O efficiency

Most file systems support some kind of read-ahead to improve performance. When sequential reads are detected, the system tries to read in more data than an application requests, assuming that the application will read it shortly.

Beware when trying to measure the performance of programs that read and write files. The operating system will try to cache the file incore (in main memory), so if you measure the performance of the program repeatedly, the successive timings will likely be better than the first. 

## File Sharing

The UNIX system supports the sharing of open files among different processes.

Thre kernel uses three data structures to represent an openfile.

- Every process has an entry in the process table, within which is a table of open file descriptors.

- The kernel maintains a file table for all open files, containing the file status flags (read/write/append...), offset, and a pointer to the v-node table entry for the file.

- Each open file has a _v-node_  strcture that contains information about the type of file and pointers to functions that operate on the file. For most files,the v-node also contains the i-node for the file. This information is read from disk when the file is opened. (Linux has a generic filesystem-independent 
i-node insetead of a v-node).

```bash
process table entry                             +-----------+
+----|-----+                               +--->-v-node info|
| fd |     |       file table entry        |    |  v-data+----+
| 0  |   +-------+  +--------------------+ |    +-----------+ |
+----------+     |  |file status flags   | |                  |
| fd |     |     +-->current file offset | |                  |
| 1  |   +----+     | v-node pointer+------+    +-----------+ |
+----------+  |     +--------------------+      |i-node info| |
|    |     |  |                                 |file size  <-+
|    |     |  |                                 |           |
|    |     |  |                                 |i_^node    |
|    |     |  |     +--------------------+      +-----------+
|    |     |  |     | file status flags  |
|    |     |  +-----> current file offset|
|    |     |  +     | v-node pointer+------+     +----------+
+----|-----+        +--------------------+ |     +v-node info
                                           +---->+   v-data+-----+
                                                 +----------+    |
                                                                 |
                                                +-----------+    |
                                                |i-node info|    |
                                                |file size  <----< 
                                                |           |
                                                | i_^node   |
                                                +-----------+
```

Each process that opens the same file gets its own file table entry. Every process has its own file table entry with its own current file offset. The offset changes as the process write. When the offset goes beyond the file size, the i-node entry is set to the current file offset. 

It is possible for more than one file desciptor entry to point to the same file table entry. `dup()` and `dup2()` duplicate an existing file descriptor. These duplicated file descriptors share the same file table entry. Another way to duplicate file descriptor is with the `fcntl` function. Opening or creating `/dev/fd/n` is identical to duplicate the corresponding file descriptor. The main use of `/dev/fd/n` is from the shell to allow programs that use pathname arguments to handle standard input and standard output in the same manner as other pathnames.

```bash
filter file2 | cat file1 /dev/fd/0 file3 | lpr # sometimes a better choice for `-` and pipeline
```

To summarize, a file table entry (maintained by the kernel) is associated with a file descriptor of a certain process. The v-nodes are shared, maintained by the kernel.

## Atomic Operation

Atomicity here means for IO, the syscall either succeeds or does nothing at all.

The positioning to the current end of file and the write should be an atomic operation with regard to other processes. The UNIX system provides an atomic way to do this if we set the `O_APPEND` when a file is opened. The SUS includes two functions that allow applications to seek and perform I/O atomically: `pread` and `pwrite`.

The `pread()` and `pwrite()` system calls  are  especially  useful  in  multi-threaded  applications.  They allow multiple threads to perform I/O on the same file descriptor without being affected by changes to the file  offset by other threads.

## Cache, delayer write

Traditional implementations of the UNIX System have a buffer cache or page cache in the kernel through which most disk I/O passes. When writing data to a file, the data is normally copied by the kernel into one of its buffers and queued for writing to disk at some later time. 

The `sync`, `fsync` and `fdatasync` are provided to ensure consistency of the file system on disk with the contents of the buffer cache. The function `sync` is normally called periodically (usually every 30 seconds) from a system daemon, often called `update`.

## `ioctl` function

The `ioctl` function has always been the catchall for I/O operations. The system provides generic `ioctl` commands for different classes of devices.
