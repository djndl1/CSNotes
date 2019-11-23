https://linux-audit.com/elf-binaries-on-linux-understanding-and-analysis/

http://www.sco.com/developers/gabi/latest/contents.html

https://reverseengineering.stackexchange.com/questions/1992/what-is-plt-got

https://www.technovelty.org/linux/plt-and-got-the-key-to-code-sharing-and-dynamic-libraries.html

https://www.airs.com/blog/archives/38

https://wiki.gentoo.org/wiki/Hardened/Introduction_to_Position_Independent_Code

https://wiki.gentoo.org/wiki/Hardened/Position_Independent_Code_internals

An ELF file consists of 

1. ELF header

2. File data

# ELF header

- `hexdump`

- `redelf -h`

- `dumpelf`

# File Data

- Program Header/Segments: An ELF file consists of zero or more segments, and describe how to create a process/memory image for runtime execution. When the kernel sees these segments, it uses them to map them into virtual address space, using the mmap(2) system call. In other words, it converts predefined instructions into a memory image.

```bash
readelf -l
```

- Section Header/Sections: used for linking and relocation

```bash
readelf -S
```

- Data
