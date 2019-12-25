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

- Contiguous Allocation: easy to implement, good read performance; fragmented over time. Feasible on CD-ROMs. DVD movies are generally stored as three or four 1-GB files.

- linked-list allocation: No space is lost to disk fragmentation. Random access is extremely slow.

- linked-list allocation using a table in memory: taking the pointer word from each block and putting it in a table in memory. Such a table is main memory is called a _FAT_. Following a chain of links is much faster in memory than in disk. The disadantage is that the entire table must be in memory all the time to make it work. It was the original MS-DOS file system.

- I-nodes: lists the attributes and disk-addresses of the file's blocks. The i-node scheme requires an array in memory whose size is proportional to the maximum number of files that may be open at once. NTFS uses a similar idea.

### Directories

The directory entry provides the information needed to find the disk blocks. This information may be the disk address of the entire file, the number of the first block, or the number of the i-node. Entries can be fixed-length, variable-length with length information or a pointer to the data structure containing the information. Hash tables can be used to accelerate searching.

### Shared Files

- hard linking: both the owner and the sharers can have pointers to a file. However, if the owner deletes the file (the original link), the file can still be there and owned by the owner, but may not under a directory controlled by the owner.

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


Internally, most VFS implementation are essentially object oriented, even if they are written in C rather than C++. These objects include the _superblock_ (which describes a file system), the _v-node_ (which describe a file) and the _directory_ (which describes a file system directory). When a file system registers, what it basically does is provide a list of the addresses of the functions the VFS require. When reading a file from a file system, the VFS finds the file system where the file is and creates a v-node (in RAM) for the file and fills the v-node with the information in the i-node of the file along with a pointer to the table of functions to call for operations on v-nodes. Now using the file descriptor we can find the v-table of the file and perform operations. `read` locates the v-node from the process and file descriptor and follows the pointer to the table of functiions, all of which are addresses within the concrete file system on which thee requested file resides.

# File System Management and Optimization

## Disk-Spacee Management

Nearly all file system chop files up into fixed-size blocks that need not be adjacent.

### block size

In a paging system, the page size is a major candidate. Reading each block normally requires a seek and rotational delay, so reading a file consisting of many small blocks will be slow. Performance and space utilization are inherently in conflict.we. Small blocks are bad for performance but good for disk-space utilization. 

### keeping track of free blocks

- linked list: generally, free blocks are used to hold the free list, so the storage is essentially free. Only one block of pointers need be kept in main memory. When it runs out, a new block of pointers is read in from the disk. When a file is deleted, its block are freed and added to the block of pointers in main memory.

- bitmap: a fixed-size data structure


Only if the disk is nearly full will the linked-list scheme require fewer blocks than the bitmap.

### Disk Quotas

Multiuser OSes often provide a mechanism for enforcing disk quotas.

## File System Backups

1. It is desirable to back up only specific directories and everything in them ratherthan the entire file system;

2. It is wasteful to back up files that have not changed since the previous backup, which leads to the idea of _incremental dumps_ ;

3. compression;

4. backup on an active file system

Two strategies can be used for dumping a disk to a backup disk:

1. physical dump: writes all the disk blocks onto the output disk in order and stops when it has copied the last one. Simple and fast . However, there is no value in backing up unused disk blocks. Blocks may go bad. Paging and hibernation files are not needed.

2. logical dump: starts at vone or more specified directories and recursively dumps all files and directories found there that have changed since some given base date. Logical dumping is the most common form.

## File System Consistency

Many file systems read blocks, modify them, and write them out later. If the system crashes before all the modified blocks have been written out, the file system can be left in an inconsistent state.

The general principle is to use the file system's inherent redundancy to repair it.

Two kinds of consistency checks can be made: 

1. blocks: checks the use of blocks in each file's i-node. It then builds a table of blocks in use and their use counts, and a table of free blocks. It checks the state of these two tables and checks one against the other.

2. files: checks the directory system and builds a table of file use. It then checks the table against the file i-nodes.

## File System Performance

- block/buffer cache: a collection of blocks that logically belong on the disk but are being kept in memory for performance reasons. Windows tends to use _write-through caches_, since originally, MS-DOS runs on removable floppy disks. As hard disks became the norm, the UNIX approach with its better efficiency, became the norm.

Caching algorithms are  similar to paging algorithms.

- block read ahead: trying to get blocks into the cache before they are needed to increase the hit rate.

- reducing disk-arm motion: keeping track of disk storage in groups of consecutive blocks; group i-nodes and their corresponding data blocks together.

## Defragmenting Disks

The performance can be restored by moving files around to make contiguous and to put all of the free space in one or more large contiguous regions on the disk.

SSDs do not really suffer from fragmentation at all. Defragmenting an SSD is counterproductive. SSDs would wear out.

# Example File System

## MS-DOS File System

```
                                The MS-DOS Directory Entry

+---------|-----------|----------|------|------|--------------------|------+
|FileName | Extension | Reserved | Time | Date | First Block Number | Size |
+---------|-----------|----------|------|------|--------------------|------+
```

MS-DOS keeps track of file blocks via a file allocation table in main memory. The first file block is used as an index into a 64K entry FAT in main memory. By following the chain, all the blocks can be found.

FAT-12/16/32 (28 actually) has disk block pointers of different length and supports different block sizes.

## The UNIX V7 File System

Filenames can be up to 14 character and can contain any ASCII char except `/`. `NUL` pads out names shorter than 14 chars.

```
Bytes    2                        14
     +----------|---------------------------+
     | I-number |       File Name           |
     +----------|---------------------------+
```

64K files per file system.

The UNIX i-node contains some attributes. The attributes contain the file size, create/last access/last modification times, owner, group, protection information, and a count of the number of directory entries that point to the i-node.

The first 10 disk addresses are stored in the i-node itself. For somewhat larger files, one of the addresses in the i-node is the address of a disk block called _single indirect blocks_. For even larger files, another address in the i-node, called a _double indirect block_ contains the address of a block that contains a list of single indirect blocks. Triple indirect blocks can be used if not enough.

To open a file, the system locates the root directory, and then finds the directory entry matching the filename recursively. The directory entry contains the i-node for the file.

## CD-ROM File Systems (ISO-9660)

No bookkeeping mechanism for free blocks.

CD-ROMs has a single continuous spiral containing the bits in a linear sequence. Logical blocks are of 2352 bytes, of which 2048 are payload portion. The function of the first 16 blocks is not defined by the standard, which can be used as bootloader. One block after is the _primary volume descriptor_, which contains some general information about the CD-ROM.

ISO-9660 uses directory entries of variable length, with the first byte indicating the length of the entry.
