- Virtual Machine Monitor (hypervisor)

The advantage of virtualization is that a failure in one virtual machine does not bring down any others. The reason virtualization works is that most service outages are due not to faulty hardware, but to ill-designed unreliable, buggy and poorly configured software, emphatically including OSes. Having fewer machines saves money on hardware and electricity. Virtualization helps in trying out new ideas. Checkpointing and migrating virtual machines is much easier than migrating processes running on a normal operating system. All that have to be moved are the memory and disk images. Another use for virtual machines is to run legacy applications on operating systems no longer supported or which do not work on current hardware. Software development is also a use of virtual machines.

The key idea of a cloud is to outsource computation or storage needs to a well-managed data center run by a company specializing in this and staffed by experts in the area.

# History

- IBM: SIMMON, CP-490, CP-67, VM/370, 

For a computer architecture to support virtualization efficiently, a set of requirements need to be satisfied ("Formal Requirements for Virtualizable Thrid Generation Architectures"). 

- '90s: VMware, Xen, KVM, VirtualBox, Hyper-V, Parallels etc.
# Requirements

1. Safety: the hypervisor should have full control of the virtualized resources. Some instructions can be executed directly, while some unsafe ones must be simulated.

2. Fidelity: the behavior of a program on a virtual machine should be identical to that of the same program on bare hardware. _Sensitive instructions_ (instructions that behave differently when executed in kernel mode than when in user mode) must be a subset of _privileged instructions_ (instructions that cause a trap when executed in user mode). In simple words, something that cannot be done in user mode should make the hardware trap when doing in user mode ???. x86 failed to satisfy this property until 2005, when Intel and AMD introduced VT (Virtualization Technology) and SVM (Secure Virtual Machine) respectively. The basic idea is to create containers in which virtual machines can be run. Before that, virtualization on x86 is done through _binary translation_ (emulating unsafe instructions in safe code).

3. Efficiency: much of the code in the virtual machine should run without intervention by the hypervisor

- Paravirtualization: presents a machine-like interface that explicitly exposes the fact that it is a virtualized environment. Guests use _hypercalls_ for privileged sensitive operations like updating the page tables. The overall system can be simpler and faster. However, the guest has to be aware of the virtual machine API. It should be customized explicitly for the hypervisor.

- Process-level virtualization: WINE compatibility layer.


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
