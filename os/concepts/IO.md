#  Basic Principles

## Hardware

I/O devices can be roughly divided into two categories

- block devices:

- character devices: 

I/O units often consist of a mechanical component and an electronic component:

- device controller/adapter: often takes the form of a chip on the parentboard or a printed circuit card that can be inserted into an extension slot. Each controller has a few registers that are used for communicating with the CPU. By writing into these registers, the OS can command the device to deliver data, accept data, switch itself on or off. Many devices have a data buffer that the OS can read and write.

- the device itself

e.g. The disk controller has a buffer (for error checking and transfer between devices of different rates) that contains a stream of bits from the disk and the controller checks if this block of bits, lumped together out of the stream, contain any errors. If none, this block will be sent to the main memory. An LCD controller manipulates the polarization of the backlight so that the OS programmer can focus higher-level stuff like displaying a line of pixels on the screen.

- memory-mapped I/O: The control registers may or may not be mapped into the same address space as the main memory. They are directly accessible using C/C++ instead of assembly procedure. No special protection mechanism is needed to keep user processes from performing I/O. Normal virtual memory will do. However, memory-mapped I/O shouldn't be cached by memory caching. The hardware has to be able to selectively disable caching. All memory modules and all I/O devices must examine all memory references to see which ones to respond to.

To save CPU time, _Direct Memory Access (DMA)_ is used to access I/O instead of CPU. The OS can use only DMA if the hardware has a DMA controller, which most systems do. The DMA contains several registers that can be written and read by the CPU, including a memory address register, a byte count register, and a few control registers that specify the I/O port to use, the direction of transfer, the transfer unit and the number of bytes to transfer in one burst. 

Without DMA, the OS has to read one byte or one word repeatedly from the controller's register and store it in main memory. With DMA, the CPU program the DMA controller by setting its registers so it knows what to transfer where. The DMA issues a command to the disk controller telling it to read data from the disk into its internal buffer and check the error. When valid data are in the disk controller's buffer, then the DMA takes over the transfer work. When the count reaches zero, the DMA controller interrupts the CPU to let it know that the transfer is now complete. Sophisticated DMA controllers can handle concurrent transfers from/to different I/O. (fly-by mode)

Alternatively, some DMA controllers can first receive the word from the I/O controller, and then issue a second bus request to send it to wherever it is supposed to go. This scheme requires more bus cycles but it's more flexible, possible to perform device-to-device transfers and even memory-to-memory copies.

May buses can operate in two modes:

- word-at-a-time mode: the controller uses the bus occasionally between CPU's uses (cycle stealing)

- block mode: the CPU can be blocked from using the bus.

Most DMA controllers use physical memory addresses, for which the CPU must first perform virtual address translation.

- interrupt: the interrupt signal is detected by the interrupt controller chip on the parentboard, which then decides what to do. If the interrupt is not handled immediately by the CPU, it will be still asserted . To handle the interrupt, the interrupt controller puts a number on the address lines specifying which device wants attention and asserts a signal to interrupt the CPU. This number is then used to index into the _interrupt vector_ (a table) to fetch the address of the corresponding interrupt-service procedure. The interrupt-service procedure starts up and acknowledges the interrupt by writing a a certain value to one of the interrupt controller's I/O ports so that the controller is now free to issue another interrupt. This way (acknowledging an interrupt only after it has been handled), race conditions involving multiple interrupts can be avoided.

To save the current context before handing an interrupt, some register values have to be pushed onto the stack (user process's or kernel?). To make things complicated, modern CPUs are heavily pipelined and often superscalar. There is no clear boundary between what instruction has been completed and what's not. The program counter may point to the next instruction to be pushed into the pipeline.

- _precise interrupt_: an interrupt that leaves the machine in a well-defined state. The PC is saved in a known place; all instructions before the one pointer to by the PC have completed; No instruction beyond the one pointed to by the PC has finished; the execution state of the instruction pointed to by the PC is known (executed or not).

- imprecise interrupt: machines with imprecise interrupts usually vomit a large amount of internal state onto the stack to give the OS the possibility of figuring out what was going on, which makes OS complicated and slow. This leads to a situation where very fast superscalar CPU sometimes being unsuitable for real-time work due to slow interrupts.

Some computers are designed so that some kinds of of interrupts and traps are precise and others are not. Some superscalar machines have precise interrupts, resulting in complex interrupt logic within the CPU and large chip area.

## Software

Some issues:

- device-independent

- uniform naming: name of a file or a device should simply be a string or an integer and not depend on the device in any way

- error handling: errors should be handled as close to the hardware as possible. Only if the lower layers are not able to deal with the problem should the upper layers be told about it.

- synchronous/asynchronous: most physical I/O is asynchronous. User programs are easier to write if the I/O operations are blocking. It is up to the OS to make operations that are actually interrupt-driven look blocking to the user programs.

- buffering:

- sharable vs. dedicated devices: Some I/O devices such as dissk can be used by many users at the same time; other devices, such as printers, have to be dedicated to a single user until that user is finished.

There are three fundamentally different ways that I/O can be performed:

1. programmed I/O: have the CPU do all the work. The CPU continuous polls the device to see if it is ready to accept another request. Programmed I/O is simple but has the disadvantage of tying up the CPU full time until all the I/O is done.

```c
copy_from_user(buffer, p, count);
for (i = 0; i < count; i++) {
    while (*printer_status_reg != READY);
    *printer_data_register = p[i];
}
return_to_user();
```

2. interrupt-driven I/O

3. I/O using DMA: in essence, DMA is programmed I/O, only with the DMA controller doing all the work, instead of the main CPU. However, the DMA controller is usually much slower than the main CPU.

# I/O Software Layer

```
       +------------------------------------+
       |      User|level I/O software       |
       +------------------------------------+
       |   Device-independent OS software   |
       +------------------------------------+
       |          Device Drivers            |
       +------------------------------------+
       |         Interrupt handlers         |
+------|------------------------------------|------+
|                                                  |
|                    Hardware                      |
|                                                  |
+--------------------------------------------------+
```

- _interrupt handlers_: interrupts should be hidden way, deep in the bowels of the OS. The driver can block itself after starting an I/O operation and the interrupt procedure can unblock the driver that was waiting for it. This model works best if drivers are structured as kernel processses.

- device drivers: each I/O device attached to a computer needs some device specific code for controlling it. Sometimes, wildly different devices are based on the same underlying technology (e.g. USB). It is possible to construct drivers that run in user space, with syscalls for reading and writing the device registers. Most OSes define a standard interface that all block drivers must support and a second standard interface that all character drivers must support. A device driver accepts abstract read and write requests from the device independent software above it and see that they are carried out. The driver must initialize the device if needed. It may also need to manage its power requirements and log events. Drivers have to be reentrant.
