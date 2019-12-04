Essential requirements for long-term information storage:

1. possible to store a large amount of information;

2. the information must survive the termination of the process using it;

3. multiple processes must be able to access the information at once.

For discussing about file systems, we think of a disk a linear sequence of fixed-size blocks and supporting two operations: read block k and write block k.

- file: logical units of information created by processes. Abstraction of persistent storage. Files are managed by the OS, how they are structured, named, accessed, used, protected, implemented and managed are major topics in operating system design. 

- file system: the part of the OS dealing with files is known as the file system.

# Files

The OS does not know or care what is in the file. All it sees are bytes. Any meaning must be imposed by user-level programs. This provides the maximum amount of flexibility (used by UNIX and Windows). When punched cards were widely used, many mainframe OS based their file systems on files consisting 80-character records, in effect, card images. Another kind of file structure is tree structure. A file consists of.

Many OSes support several types of files. UNIX have regular files, directories, character special files, block special files. The OS will execute a file only if it has the proper format. Every OS must recoginize its own executable file. Files may be accessed randomly or sequentially. Files may have extra attributes (metadata) like size, time of last change, lock flags.

Some common file operations:

- `create`

- `delete`

- `open`: allow the system to fetch the attributes and list of disk addresses into main memory for rapid access on later calls.

- `close`: the attributes and disk addresses are no longer needed

- `read`

- `write`

- `append`

- `seek`: for random-access file

- `get_attributers`

- `set_attributes`

# Directories

File systems normally have directories or folders.

- single level directory systems: still used on simple embedded devices such as digital cameras and some portable music players.

- hierarchical directory systems

When the file system is organized as a directory tree, some way is needed for specifying file names:

- absolute path name

- relative path name

Directory operations:

- `create`

- `delete`

- `opendir`

- `closedir`

- `readdir`

- `rename`

- `link`: allows a file to appear in more than one directory

- `unlink`

## File System Implementation


### Files 

- Contiguous Allocation: easy to implement, good read performance; fragmented over time. Feasible on CD-ROMs. DVD moves are generally stored as three or four 1-GB files.

- linked-list allocation: No space is lost to disk fragmentation. Random access is extremely slow.

- linked-list allocation using a table in memory: taking the pointer word from each block and putting it in a table in memory. Such a table is main memory is called a _FAT_. Following a chain of links is much faster in memory than in disk. The disadantage is that the entire table must be in memory all the time to make it work. It was the original MS-DOS file system.

- I-nodes: lists the attributes and disk-addresses of the file's blocks. The i-node scheme requires an array in memory whose size is proportional to the maximum number of files that may be open at once. NTFS uses a similar idea.

### Directories

The directory entry provides the information needed to find the disk blocks. This information may be the disk address of the entire file, the number of the first block, or the number of the i-node. Entries can be fixed-length, variable-length with length information or a pointer to the data structure containing the information. Hash tables can be used to accelerate searching.

### Shared Files

- hard linking: both the owner and the sharers can have pointers to a file. However, if the owner deletes the file, the file can still be there and owned by the owner, but may not under a directory controlled by the owner.

- symbolic linking: only the true owner has a pointer to the i-node. Extra overhead required.

### Log-Structured File System

As CPUs get faster and RAM memories get larger, disk caches are also increasing rapidly,  it is possible to satisfy a very substantial fraction of all read requests directly from the file-system cache and most disk accesses will be writes. The basic idea is to structure the entire disk as a great big log. 

Periodically, all the pending writes being buffered in memory are collected into a single segment and written to the disk as a single contiguous segment at the end of the log. At the start of each segment is a segment summary, telling what can be found in the segment. I-nodes are now scattered all over the log. To make it possible to find i-nodes, an i-node map, indexed by i-number, is maintained, kept on disk, but also cached.

LFS has a cleaner thread that scans the log circularly to compact it. It checks the segment summary against the i-node map to find those i-nodes and blocks still in use and read them into memory to write out in the next segment. The original segment is then marked as free.

Not widely used.

### Journaling File Systems

The basic idea is to keep a log of what the file system is going to do before it does it. NTFS, ext4 and ReiserFS all use journaling. What the journaling file system does is first write a log entry listing the actions to be completed. The log entry is then written to disk (and for good measure, possibly read back from the disk to verify that it was, in fact, written correctly). Only after the log entry has been written, do the various operations begin. After the operations complete successfully, the log entry is erased. If the system now crashes, upon recovery the file system can check the log to see if any operations were pending. If so, all of them can be rerun (multiple times in the event of repeated crashes) until the file is correctly removed. 

The logged operations must idempotent. Journaling file systems have to arrange their data structures and loggable operations so they all are idempotent. A file system can introduce the database concept of an atomic transaction.

### Virtual File System

The key idea is to abstract out the part of the file system that is common to all file systems and put that code in a separate layer that calls the underlying concrete file systems to actually manage the data.

```
+--------------------------------------------------------------------------+
|                                                                          |
|                               POSIX I/O Interface                        |
|                                                                          |
|                                                                          |
+--------------------------------------------------------------------------+
|                                                                          |
|                                                                          |
|     +------------------------------------------------------------+       |
|     |                                                            |       |
|     |                     Virtual File System                    |       |
|     |                                                            |       |
|     |                                                            |       |
|     +-------|-----------------------|-----------------------|----+       |
|             |                       |                       |            |
|             |                       |                       |            |    VFS interface
|             |                       |                       |            |
|             v                       |                       |            |
|       +-----|----+            +-----v-----+          +------v----+       |
|       |          |            |           |          |           |       |
|       |   FS 1   |            |   FS 2    |          |    FS 3   |       |
|       |          |            |           |          |           |       |
|       |          |            |           |          |           |       |
|       +-----^----+            +------^----+          +------|----+       |
|             |                        |                      ^            |
|             |                        |                      |            |
|             |                        |                      |            |
|             |                        |                      |            |
|      +------v------------------------v----------------------v------+     |
|      |                                                             |     |
|      |                         Buffer Cache                        |     |
|      |                                                             |     |
|      +-------------------------------------------------------------+     |
+--------------------------------------------------------------------------+
```

Internally, most VFS implementation are essentially object oriented, enen if they are written in C rather than C++. These objects include the _superblock_ (which describes a file system), the _v-node_ (which describe a file) and the _directory_ (which describes a file system directory). When a file system registers, what it basically does is provide a list of the addresses of the functions the VFS require. When reading a file from a file system, the VFS finds the file system where the file is and creates a v-node (in RAM) for the file and fills the v-node with the information in the i-node of the file along with a pointer to the table of functions to call for operations on v-nodes. Now using the file descriptor we can find the v-table of the file and perform operations. `read` locates the v-node form the process and file descriptor and follows the pointer to the table of functiions, all of which are addresses within the concrete file system on which thee requested file resides.
