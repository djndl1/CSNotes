# Architecture

Eight general-purpose registers

1. Accumulator register `AX`: Used in arithmetic operations

2. Counter register `CX`: Used in shift/rotate instructions and loops.

3. Data register `DX`: Used in arithmetic operations and I/O operations.

4. Base register `BX`. Used as a pointer to data (located in segment register DS, when in segmented mode).

5. Stack Pointer register `SP`: Pointer to the top of the stack.

6. Stack Base Pointer register `BP`: Used to point to the base of the stack.

7. Source Index register `SI`: Used as a pointer to a source in stream operations.

8. Destination Index register `DI`: Used as a pointer to a destination in stream operations.

The order is same as what is used in a push-to-stack operation.

Six Segment Registers:

- Stack Segment `SS`: Pointer to the stack.

- Code Segment `CS`: Pointer to the code.

- Data Segment `DS`: Pointer to the data.

- Extra Segment `ES`: Pointer to extra data ('E' stands for 'Extra').

- F Segment `FS`: Pointer to more extra data ('F' comes after 'E').

- G Segment `GS`: Pointer to still more extra data ('G' comes after 'F').

In real mode, segment registers were used to addressing a 20-bit address bus using 16-bit registers. Later, they were used as selectors to protect memory access.

The `EFLAGS` is a 32-bit register used as a collection of boolean bits to store the results of operations and the state of the processors.

https://en.wikibooks.org/wiki/X86_Assembly/16,_32,_and_64_Bits

https://wiki.osdev.org/Segmentation

https://unix.stackexchange.com/questions/469253/does-linux-not-use-segmentation-but-only-paging#

https://wiki.osdev.org/Global_Descriptor_Table
