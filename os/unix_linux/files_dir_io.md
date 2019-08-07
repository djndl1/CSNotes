# I/O

## Five basic IO functions

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
