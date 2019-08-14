
# I/O

## Five basic IO functions

Most file I/O on a UNIX system can be performed using only five functions: `open`, `read`, `write`, `lseek`, `close`. Each `read` and `write` are unbuffered.

To the kernel, all open files of a process are referred to by file descriptors, a nonnegative integer. By convention, file descriptor `0` is the standard input of a process, `1` the standard input, `2` the standard error. It is not a feature of the UNIX kernel. They should be replaced by the symbolic constants `STDIN_FILENO`, `STDOUT_FILENO` and `STDERR_FILENO`. File descriptors range from 0 through `"OPEN_MAX-1` (On Linux 5.1 the value is 1024). 

A file is opened or created by calling either the `open` function or the `openat` function. The file descriptor returned by `open` and `openat` is guaranteed to be the lowest-numbered unused descriptor. `openat` gives threads a way to use relative pathnames to open files in directories other than the CWD and it provides a way to avoid time-of-check-to-time-of-use errors. `O_CREAT` flag creates the file if it doesn't exist.

A new file can also be created by calling the `creat` function. It's equivalent to `open(path, O_WRONLY | O_CREAT | O_TRUNC, mode)`. This syscall is somewhat redundant. One deficiency with `creat` is that the file is opened only for writing.

A open file is closed by calling the `close` function. Closing a file also releases any record locks that the process may have on the file. When a process terminates, all of its open files are closed automatically by the kernel.

Every open file has an associated ‘‘current file offset,’’ normally a non-negative integer that measures the number of bytes from the beginning of the file. Read and write operations normally start at the current file offset and cause the offset to be incremented by the number of bytes read or written. By default, this offset is initialized to 0 when a file is opened, unless the `O_APPEND` option is specified. A open file's offset can be set explicitly by calling `lseek`. We can seek zero bytes from the current position to determine the current offset, or to determine if a file is capable of seeking.

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

Each process that opens the same file gets its own file table entry (which indicates the status of a file opening). Every process has its own file table entry with its own current file offset. The offset changes as the process write. When the offset goes beyond the file size, the i-node entry is set to the current file offset. 

It is possible for more than one file desciptor entry to point to the same file table entry. `dup()` and `dup2()` duplicate an existing file descriptor. These duplicated file descriptors share the same file table entry. Another way to duplicate file descriptor is with the `fcntl` function. Opening or creating `/dev/fd/n` is identical to duplicate the corresponding file descriptor. The main use of `/dev/fd/n` is from the shell to allow programs that use pathname arguments to handle standard input and standard output in the same manner as other pathnames.

```bash
filter file2 | cat file1 /dev/fd/0 file3 | lpr # sometimes a better choice for `-` and pipeline
```

To summarize, a file table entry (maintained by the kernel) is associated with a file descriptor of a certain process. The v-nodes are shared, maintained by the kernel.

## Atomic Operation

Atomicity here means for IO, the syscall either succeeds or does nothing at all.

The positioning to the current end of file and the write should be an atomic operation with regard to other processes. The UNIX system provides an atomic way to do this if we set the `O_APPEND` when a file is opened. The SUS includes two functions that allow applications to seek and perform I/O atomically: `pread` and `pwrite`.

The `pread()` and `pwrite()` system calls  are  especially  useful  in  multi-threaded  applications.  They allow multiple threads to perform I/O on the same file descriptor without being affected by changes to the file  offset by other threads.

## Cache, delayed write

Traditional implementations of the UNIX System have a buffer cache or page cache in the kernel through which most disk I/O passes. When writing data to a file, the data is normally copied by the kernel into one of its buffers and queued for writing to disk at some later time. 

The `sync`, `fsync` and `fdatasync` are provided to ensure consistency of the file system on disk with the contents of the buffer cache. The function `sync` is normally called periodically (usually every 30 seconds) from a system daemon, often called `update`.

## `fcntl` function

The `fcntl` function can change the properties of a file that is already open. 
- Duplicate an existing descriptor

- Get/set file descriptor flags

- Get/set file status flags

- Get/set asynchronous I/O ownership

- Get/set record locks



## `ioctl` function

The `ioctl` function has always been the catchall for I/O operations. The system provides generic `ioctl` commands for different classes of devices.

# Files and Directories

Given a pathname, the `stat` function returns a structure of information about the named file. The `fstat` function obtains information about the file that is already opened on the descriptor `fd`. The `lstat` function can `stat`s symbolic links. The `fstatat` funciton provides a way to return the file statistics for a pathname relative to an open directory represented by the `fd` argument.

```c
struct stat {
    dev_t     st_dev;         /* ID of device containing file */
    ino_t     st_ino;         /* Inode number */
    mode_t    st_mode;        /* File type and mode */
    nlink_t   st_nlink;       /* Number of hard links */

    uid_t     st_uid;         /* User ID of owner */
    gid_t     st_gid;         /* Group ID of owner */

    dev_t     st_rdev;        /* Device ID (if special file) */
    
    off_t     st_size;        /* Total size, in bytes */
    blksize_t st_blksize;     /* Block size for filesystem I/O */
    blkcnt_t  st_blocks;      /* Number of 512B blocks allocated */

    struct timespec st_atim;  /* Time of last access */
    struct timespec st_mtim;  /* Time of last modification */
    struct timespec st_ctim;  /* Time of last status change */
};
```

```c
struct timespec {
    time_t          tv_sec;     // elaped time in whole seconds
    long            tv_nsec;    // the rest of the elapsed time in nanoseconds
};
```

## File types

```c
    mode_t    st_mode;        /* File type and mode */
```

A file on a UNIX system can be a

- regular file, with no distinction of text or data type. Any interpretation is left to the application.

- directory file: a file that contains the names of other files and pointers to information on these files. Only the kernel can write directly to a directory file.

- block special file: a type of file providing buffered I/O access in fixed-size units to devices such as disk drives.

- character special file: a type of file providing unbuffered I/O access in _variable-sized_ units to devices.

- FIFO/named pipe: a type of file used for communication between processes

- Socket: a type fo file for network communication between processes, also for non-network communication between processes on a single host.

- Symbolic link

All devices on a system are either block special files or character special files.

The type of a file can be determined using macros.

```c
S_ISREG()
S_ISDIR()
S_ISCHR()
S_ISBLK()
S_ISFIFO()
S_ISLNK()
S_ISSOCK()
```

## Permissions, Ownership and Groups

```c
   uid_t     st_uid;         /* User ID of owner */
   gid_t     st_gid;         /* Group ID of owner */
```

Every process has six or more IDs associated with it

- real user ID; real group ID

- effective user ID; effective group ID; supplementary group IDs

- saved set-user-ID; saved set-group-ID: contain copies of the effective user ID and the effective group ID, respectively when a program is executed.

the set-user-ID bit and the set-group-ID bit causes the effective user/group ID to be the user/group ID of the owner. These two bits are in `st_mode` and can be tested against the constants `S_ISUID` and `S_ISGID`.

The `st_mode` value also encodes the access permission bits of the file. There are a few rules about permissions besides the obvious ones.

- To access a file under a directory, the user must have execute permission to the directory and directories above. The execute permission bit for a directory is often called the _search_ bit.

- we must have write permission for a file to specify the `O_TRUNCATE` flag.

- To create a new file in a directory, write permission and execute permission for the directory are required.

- to delete an existing file in a directory, write permission and execute permission for the directory are required but read permission or write permission for the file itself are unnecessary.

```bash
 djn  debian  ~/FOSS/playground/perm  lh
Permissions Size User Date Modified Name
.---------     5 djn  13 Aug  0:30  a.txt

 djn  debian  ~/FOSS/playground/perm  rm a.txt 
rm: remove write-protected regular file 'a.txt'? y

```

The file access tests that the kernel performs each time a process opens, creates, or deletes a file depend on the owner of the file (`st_uid` and `st_gid`), the effective IDs of the process and the supplementary gorup ID of the process (if the effective group ID of the process or one of the supplementary group IDs of the process equals the group ID of the file, access is allowed if the appropriate group access permission bit is set). The test order is superuser ID, effective user ID, group IDs and other access permission. If the process owns the file, access is granted or denied only based on the user access permissions.

The user ID of a new file is set to the effective user ID of the process. The group ID of a new file can be the effective group ID of the process or the group ID of the directory in which the file is being created depending on the implementation. On Linux, this is determined by whether the set-group-ID bit is set. If it's set, then the permission is copied from the directory (the subdirectory will be set-group-ID automatically), otherwise it's set to the effective group ID of the process.

The `access` and `faccessat` functions base their tests on the real user and group IDs instead of effective user ID. If `AT_EACCESS` flag is set, the access checks are made using the effective user and group IDs of the calling process instead of the real user and group IDs.

The `umask` sets the file mode creation mask for the process and returns the previous value. The file mode creation mask is used whenever the process creates a new file or a new directory. It disables the corresponding mode bits that it is set to. If anyone can read a file, the `umask` needs to be set to 0.

The `chmod`, `fchmod`, `fchmodat` functions allow to change the file access permissions for an existing file.

Sticky (sticking to the swap area) bit (formally called saved-text) was initially used to indicate a executable file should be cached. If the sticky bit is set for a directory, a file in the directory can be removed or renamed only if the user has write permissions for the directory and owns the file or owns the directory or is the superuser. e.g. `/var/tmp`, `/tmp` so that any one can create, read, write a file there but no one except the superuser can delete files owned by other users (the directories belong to root).

The `chown` functions allow us to change a file's user ID and group ID.


## File Size and Trucation

```c
    off_t     st_size;        /* Total size, in bytes */
    blksize_t st_blksize;     /* Block size for filesystem I/O */
    blkcnt_t  st_blocks;      /* Number of (typically 512B) blocks allocated */
```

This field is meaningful only for regular files, directories and symbolic links. A regular file of size 0 is allowed. The file size of a symbol link is the nubmer of bytes in the filename it points to.

`truncate()` and `ftruncate()` truncate an existing file to a specified size (may increase the size).

## File Systems

A disk is divided into one or more partitions, each of which contains a file system.

```bash
          file    system
  +------|-------|---------------------------------------|-------------------------------------|-----|------------------------------------------+
  |      |       |                                       |                                     |     |                                          |
  |      |       |                                       |                                     |     |                                          |
  |      |       | cylinder group 0                      |        cylinder group 1             | ... |           cylinder group 2               |
  |      |       |                                       |                                     |     |                                          |
  |      |       |                                       |                                     |     |                                          |
  |   +  |    +  |                                       |                                     |     |                                          |
  +------|-------|---------------------------------------|-------------------------------------|-----|------------------------------------------+
      |       |
      |       |                                                                 cylinder group
      |       +--> super block            +--------|--------|--------|--------|---------------------|-----------------------------------------+
      v                                   |        |        |        |        |                     |                                         |
                                          | super  |        |        |        |                     |                                         |
boot block                                | block  |   cg   | i-node | block  |                     |                                         |
                                          |  copy  |  info  |  map   | bitmap |       i-nodes       |               data blocks               |
                                          |        |        |        |        |                     |                                         |
                                          |        |        |        |        |                     |                                         |
                                          +--------|--------|--------|--------|---------------------|-----------------------------------------+
```

Every i-node has link count that contains the number of directory entries that point to it. Only when the link count goes to 0 can the file be deleted (unlinking).

The i-node contains all the information about the file. Most of the information in the `stat` structure is obtaiend from the i-node. Only two items of interest are stored in the directory entry: the filename and the i-node number.

Any leaf directory has a link count of 2, the directory itself contains one and its parent directory contains the other.
