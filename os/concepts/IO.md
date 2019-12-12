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

- device-independence

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
       |      User-level I/O software       |
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

## interrupt handlers

Interrupts should be hidden way, deep in the bowels of the OS. The driver can block itself after starting an I/O operation and the interrupt procedure can unblock the driver that was waiting for it. This model works best if drivers are structured as kernel processses.

## device drivers

Each I/O device attached to a computer needs some device specific code for controlling it. Sometimes, wildly different devices are based on the same underlying technology (e.g. USB). It is possible to construct drivers that run in user space, with syscalls for reading and writing the device registers. Most OSes define a standard interface that all block drivers must support and a second standard interface that all character drivers must support. A device driver accepts abstract read and write requests from the device independent software above it and see that they are carried out. The driver must initialize the device if needed. It may also need to manage its power requirements and log events. The driver may need to wait for an interrupt if the I/O takes long. Drivers have to be reentrant.

## device-independent I/O

Some functions can be done in a device-independent way

- uniform interfacing for device drivers: all drivers have the same interface. It is much easier to plug in a new driver, providing it conforms to the driver interface. For each class of devices, such as disks or printers, the OS defines a set of functions that the driver must supply. When the driver is loaded, the OS records the address of this table of function pointers. Another aspect is how I/O devices are named. In Unix, a device name specifies the i-node for a special file, and this i-node contains the major and minor device number. In both Unix and Windows, devices appear in the file system as named objects. The usual protection rules for files would apply to I/O devices.

- buffering: user-space buffering; kernel-space buffering; double buffering (two buffers in kernel space taking turns); circular buffering (a circular queue); The device itself might have a buffer too.

- error reporting: the framework for error handling is device independent.

- allocating and releasing dedicated devices: perform `open` on devices; the OS managing the blocking queue

- providing a device-independent block size.

## User-Space I/O software

Syscalls, including the I/O syscalls, are normally made by library procedures.

Spooling system, running as a daemon.

# Disks

TODO

# Clocks (timers)

## Clock Hardware

Clocks/timers are essential to the operation of any multiprogrammed system for a variety of reasons. They maintain the time of day and prevent one process from monopolize the CPU, among other things. The clock software can take the form of a device driver.

A clock is typically built out of a crystal oscillator, a counter, and a holding register. The counter is decremented at each pulse. When the counter goes to zero, it cuases a CPU interrupt.

- clock tick: the periodic interrupts.

Programmable clocks typically have several modes of operation:

1. _one-shot mode_: it causes an interrupt and stops when it goes to zero until it is explicitly started again by the software.

2. _square-wave mode_: the holding register is automatically copied into the counter and the whole process is repeated again indefinitely.

The interrupt frequency can be controlled by software.

All the clock hardware does is generate interrupts at known intervals.

## Clock Software

The duties of the clock driver usually include among:

1. maintaining time of day; (real time)

2. preventing processes from running longer than they are allowed to;

3. accounting for CPU usage;

4. handling the `alarm` syscall made by user processes: the clock driver must simulate multiple virtual clocks with a single physical clock.

5. providing watchdog timers for parts of the system itself: when a timer goes off, the clock driver calls procedure supplied by the caller.

6. doing profiling, monitoring, and statistics gathering.

During a clock interrupt, the clock driver has to increment the real time,, decrement the quantum and ckeck for zero, do CPU accounting, and decrement the alarm counter. Each of these operations has been carefully arranged to be very fast.

## Soft Timers

TODO

# User Interfaces: Keyboard, mouse, monitor

## Input Software

- keyboard: an interrupt is generated whenever a key is struck and a second one is generated whenever a key is released. Normal keyboards have fewer than 128 keys, so only 7 bits are needed to represent the key number (scan code). The eighth bit is set to 0 on a key press and to 1 on a release. It is up to the driver to keep track of the status of each key and the driver determines whether it is lowercase, uppercase, CTRL-A, ALT-A, CTRL-ALT-A or some other combination. Since driver can tell which keys have been struck but not yet released. 

```
depress SHIFT, depress A, release A, release SHIFT      # SHIFT-A, an uppercase
```

Two possible approaches can be adopted for the driver:

- _character oriented_: a program reading from the keyboard gets a raw sequence of ASCII codes e.g. emacs

- _line oriented_: the dreiver handles all the intraline editing and just delivers corrected lines to the user programs.

TODO

## Output Software

### Text Windows

Escape sequences: a series of commands to move the cursor, insert and delete characters or line at the cursor and so on.

There were hundreds of terminal types, each with its own escape sequences. To solve this issue, BSD Unix introduced a terminal database called _termcap_. It defines a number of basic actions.

### The X Windowing System

Runs completely in user space, intended for connecting a large number of remote user terminals with a central computer server.

- X server: the software that collects input and writes output to the screen


- X client: the running GUI programs

```
+--------|---------+
| Window |   GUI   |
|        | program |
| Manager|         |
+------------------+
|       Motif      | uniform look and feel
+------------------+
|    Intrinsics    | manages widgets
+------------------+
|       Xlib       | X functionality
+------------------+              +------------------+
|     X client     | +          +-+     X server     |
+------------------+ |          | +------------------+
|       UNIX       | |          | |       UNIX       |
+------------------+ |          | +------------------+
|     Hardware     | |          | |     Hardware     |
+------------------+ |          | +------------------+
                     |          |
                     +----------+
                      X protocol on network
```

- window manager: controls the creatiion, deletion and movement of windows on the screen. It sends commands to the X server telling it what to do.

In Windows, the windowing and GUI systems are mixed together in the GDI and located in the kernel.

- _resource_: a key concept in X, a data structure that holds certain information, e.g. windows, fonts, colormaps (color palettes), pixmaps (bitmaps), cursors, and graphic contexts (in which properties of a window are stored).

When an X program starts, it opens a connection to one or more X servers. X considers this connection to be reliable in the sense that lost and duplicate messages are handled by the networking software. Usually, TCP/IP is used between the client and server. Four kinds of messages go over the connection:

1. drawing commands from the program to the workstation: most drawing commands are sent as one-way messages.

2. replies by the workstation to program queries;

3. keyboard, mouse, and other event announcements;

4. error messages.

```c
#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main(int argc, char *argv[])
{
        Display disp;                   // server
        Window win;                     // window  
        GC gc;                          // graphic context
        XEvent event;                   // storage for one event
        int running = 1;

        disp = XOpenDisplay(argv[1]);           // connect to the X server
        win = XCreateSimpleWindow(disp, ...); // allocate memory for new window
        XSetStandardProperties(disp,...);       // announces window to window manager
        gc = XCreateGC(disp, win, 0, 0);        // create graphic context
        XSelectInput(disp, win, ButtonPressMask | KeyPressMask | ExposureMask);
        XMapRaised(disp,win);           // display window; send exposure event

        while (running) {
                XNextEvent(disp, &event);  // get next event
                switch (event.type) {
                case Expose: 
                        ...
                        break;
                case ButtonPress:
                        ...
                        break;
                case KeyPress:
                        ...
                        break;
                }
        }

        XFreeGC(disp, gc);
        XDestroyWindow(disp, win);
        XCloseDisplay(disp);
        return 0;
}
``**

### Graphical User Interfaces

A GUI has four essential elements: WIMP

1. Windows: rectangular blocks of screen area used to run programs

2. Icons: little symbols that can be clicked on;

3. Menus: lists of actions to choose;

4. Pointing device: mouse, trackball, or other hardware device used to move a cursor around the screen to select them

Output of GUI goes to a graphics adapter, which contains video RAM that holds the images that appear on the screen.

Win32 GUI TODO

#### Touch Screen

- opaque touch device: touchpad on a notebook

- transparent device: touch screen on a smartphone or tablet

A capacitive touch display was decribed as early as 1965.

- resistive screens: a flexible plastic surface on top, under which a thin film of ITO is printed in thin lines. Another surface under is also coated with a layer of ITO. Two layers run horizontally and vertically respectively. When the screen is touched, the upper line touches the lower line. Measuring the resistance in both layers at all positions will determine the position of the touch. Typically does not support multitouch.

- capacitive screens: two hard surfaces, each coated with ITO. Two separate charged surfaces form a grid of small capacitors. Voltages are applied alternately to the horizontal and vertical lines, The voltage values are affected by the capacitance of each intersection.

# Power Management

Two general approaches to reducing energy consumption:

1. for the OS to turn off parts of the computer when they are not in use.

2. for the application to use less energy.

## Hardware Issues

The general approach most computer vendors take to battery conservation is to design the CPU, memory and I/O devices to have multiple states: on, sleeping, hibernating and off.

## OS Issues

- display: shutting down the display when no activity for some time.

- hard disk: spinning. Many computers, especially notebooks, spin the disk down after a certain number of minutes of being idle.

- the CPU: there is relationship between CPU voltage, clock cycle and power usage. Scaling down CPU cores does not always imply a reduction in performance.

- the memory: hibernation (writing the contents of main memory to the disk)

- wireless: one way is to allow the base station and the mobile device to synchronize somehow so that the device wouldn't miss messages. In 802.11, a mobile computer can notify the access point that it is going to sleep but it will wake up before the base station sends the next beacon frame.

- thermal management

- battery: smart batteries that can be controlled and queried by the OS.

Several OSes have an elaborate mechanism for power management called **ACPI** (**Advanced Configuration and Power Interface**). The OS can send any conformant driver commands asking it to report on the capabilities of its devices and their current states. It can also send commands to drivers instructing them to cut their power levels.

## Application Issues

Telling the programs to use less energy at the price of poorer user experience.
