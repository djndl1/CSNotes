- Virtual Machine Monitor (hypervisor)

The advantage of virtualization is that a failure in one virtual machine does not bring down any others. The reason virtualization works is that most service outages are due not to faulty hardware, but to ill-designed unreliable, buggy and poorly configured software, emphatically including OSes. Having fewer machines saves money on hardware and electricity. Virtualization helps in trying out new ideas. Checkpointing and migrating virtual machines is much easier than migrating processes running on a normal operating system. All that have to be moved are the memory and disk images. Another use for virtual machines is to run legacy applications on operating systems no longer supported or which do not work on current hardware. Software development is also a use of virtual machines.

The key idea of a cloud is to outsource computation or storage needs to a well-managed data center run by a company specializing in this and staffed by experts in the area.

# History

- IBM: SIMMON, CP-490, CP-67, VM/370, 

For a computer architecture to support virtualization efficiently, a set of requirements need to be satisfied ("Formal Requirements for Virtualizable Thrid Generation Architectures"). 

- '90s: VMware, Xen, KVM, VirtualBox, Hyper-V, Parallels etc.
# Requirements

1. Safety: the hypervisor should have full control of the virtualized resources. Some instructions can be executed directly, while some unsafe ones must be simulated.

2. Fidelity: the behavior of a program on a virtual machine should be identical to that of the same program on bare hardware. _Sensitive instructions_ (instructions that behave differently when executed in kernel mode than when in user mode) must be a subset of _privileged instructions_ (instructions that cause a trap when executed in user mode). In simple words, something that cannot be done in user mode should make the hardware trap when doing in user mode. x86 failed to satisfy this property until 2005, when Intel and AMD introduced VT (Virtualization Technology) and SVM (Secure Virtual Machine) respectively. The basic idea is to create containers in which virtual machines can be run ???. Before that, virtualization on x86 is done through _binary translation_ (emulating unsafe instructions in safe code).

3. Efficiency: much of the code in the virtual machine should run without intervention by the hypervisor

_Paravirtualization_: presents a machine-like interface that explicitly exposes the fact that it is a virtualized environment. Guests use _hypercalls_ for privileged sensitive operations like updating the page tables. The overall system can be simpler and faster. However, the guest has to be aware of the virtual machine API. It should be customized explicitly for the hypervisor.

_Process-level virtualization_: WINE compatibility layer.


# Type 1 and Type 2 Hypervisors

- type 1 hypervisor: support multiple copies of the actual hardware, called _virtual machines_.

- type 2 hypervisor (hosted hypervisors): runs on top of a host operating system.

# Efficient Virtualization

## Before VT

Making use of binary translation and hardware features such as protection rings. For many years, x86 processors has supported four protection modes/rings:

1. Ring 3: least privileged, where normal user processes execute; Cannot execute sensitive instructions

2. Ring 1, 2: normally not used; Guest OSes run on Ring 1 with type 1 hypervisors

3. Ring 0: where bare metal OSes and type 1 hypervisors run; free to use whatever instructions.

- basic block: a short, straight-line sequence of instructions that ends with a branch.

For guest's kernel code, The hypervisor rewrites the code, one basic block at a time. It replaces sensitive instructions in a basic block with a call into the hypervisor. The branch on the last instruction is also replaced by a call into the hypervisor to make sure it can repeat the above procedure for the next basic block (control is returned to the hypervisor, which locates its successor). Dynamic translation and emulation are not expensive as usually expected. Most code blocks do not contain sensitive or privileged instructions and thus can be executes natively. Blocks can be cached. Eventually, most of the program will be in the cache and run at close to full speed.

However, type 2 hypervisors is required to manipulate the hardware at the lowest level since guest kernel code runs as a user process. Most modern type 2 hypervisors have a kernel module operating in ring 0 that allows them to manipulate the hardware with privileged instructions. If the host kernel code needs to run, the hypervisor restores the processor context. 

- _world switch_: going from a hardware configuration for the host kernel to a configuration for the guest operating system

## With VT

https://arstechnica.com/technopaedia/2008/05/binary-translation/

With VT, _trap-and-emulate_ virtualization is possible. However, this is not always faster than binary translation.

## Paravirtualization and Microkernel

With paravirtualization, the OS knowns that it's not running on bare metal. The OS itself is modified so that when it contains no sensitive instructions. Instead, it makes hypercalls to obtain the same results. The hypervisor itself just carries out hypercalls. There  is not need for the emulation of sensitive instructions and the hypervisor behaves like a microkernel, which just provides very basic services such as process dispatching and managing the MMU.

VMI (Virtual Machine Interface) was proposed to form a low-level layer that interfaces with the hardware or hypervisor, designed to be generic and not tied to any specific hardware platform or any particular hypervisor. This way, the core of the operating system remains portable yet is hypervisor friendly and still efficient. Another similar idea is _paravirt ops_, which is already included in the Linux mainline kernel.

# Memory Virtualization

(type 1 hypervisor assumed)

Virtualization greatly complicates memory management. In general, for each virtual machine, the hypervisor needs to create a _shadow page table_ that maps the virtual (physical) pages used by the virtual machine onto the actual pages the hypervisor gave it. Every time the guest OS changes its page table, the hypervisor must change the shadow page table as well, but the hypervisor may not be able to know the change. 

One solution is that the hypervisor keeps track of which page in the guest's virtual memory contains the top-level page table after the first time the guest attempts to load the hardware register that points to it because this information is sensitive and traps. The hypervisor then marks this page as read-only. When the guest tries to modify the table, a page fault occurs and the hypervisor takes control, which can analyze the instruction stream, figure out what the guest OS is trying to do and update the shadow page table accordingly.

Another solution is to allow the guest to add new mappings to its page tables at will. As soon as the guest tries to access any of the new pages, a fault will occur and the hypervisor takes control.

Page faults are expensive (both guest-induced and hypervisor-induced). The hypervisor gains control (_VM exit_), and records the cause of the VM exit and the address of the guest instruction that caused the exit. Next, a context switch is done. Then the hypervisor's processor state is loaded. Then the hypervisor starts handling the page fault.

In a paravirtualized OS, the guest can inform the hypervisor by making hypercalls.

## Hardware Support for Nested Page Tables

nested page tables (AMD); Extended Page Tables (Intel). They remove most of the overhead by handling the addition page-table manipulation all in hardware without any traps.

guest virtual addresses -> guest physical addresses -> host physical addresses. The hardware first walks the regular page tables to translate the guest virtual address to a guest physical address.

## Reclaiming Memory

_ballooning_: a common solution to memory overcommitment is to trick the guest OS into making paging decisions for it.

# I/O Virtualization

TODO

# Virtual Appliances

A preconfigured virtual machine image, ready to run on a hypervisor.

# Virtual Machines on Multicore CPUs

The number of CPUs available can be set by the software. A single computer becomes a virtual multiprocesssor.

Virtual machines can share memory.

# Clouds

1. On-demand self-service

2. broad network access

3. resource pooling: the computing resource should be pooled to serve multiple users and with the ability to assign and reassign resources dynamically;

4. rapid elasticity: possible to acquire and release resources elastically, perhaps even automatically, to scale immediately with the user's demands.

5. measured service

_Infrastructure As a Service (IAAS)_: direct access to virtual machine, which the user can use in any way he sees fit: Amazon EC2

_Platform As A Service (PAAS)_: delivers an environment that includes things such as a specific OS

_Software As A Service (SAAS)_: offers access to specific software

Hypervisors decouple the virtual machine from the physical hardware. The administrator could simply shutdown all the virtual machines and restart them again on a new machine. A slightly better approach is to pause the virtual machine, copy over the memory pages used by the virtual machines to the new hardware as quickly as possible, configure things correctly in the new hypervisor and then resume execution. What modern virtualization solutions offer is _live migration_. They employ techniques like _pre-copy memory migration_ (copying memory pages while the machine is still serving requests).

# Case Study: VMWARE

VMware Workstation: type 2 hypervisor, the first virtualization product for 32-bit x86 computers.

VMware ESX Server - type 1 hypervisor

vSphere - a single point of management for a cluster of servers running virtual machines

VMotion: live migration

TODO
