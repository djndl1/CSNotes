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
