# Introduction




# Chap. 6 Parallel Processors From Client to Cloud

keywords: multiprocessors, task-level parallelism, process-level parallelism, parallel processing program(a single program that runs on multiple processors simultaneously), cluster, multicore microprocessors, shared memory processors (SMPs, a parallel processor with a single physical address space), 
- _serial_ versus _parallel_: 

- _sequential_ versus _concurrent_: code/conceptual level

Uniprocessor design techniques take advantage of instruction-level parallelism normally without the involvement of the programmer. Workload distribution and communication between processors pose great challenges for multiprocessors.

TODO

## SISD, MIMD, SIMD, SPMD and Vector

- SPMD: Single Program Multiple Data, writing a single program that runs on all processors of a MIMD computer, relying on conditional statements when different processors should execute distinct section of codes, the normal way to program a MIMD computer.

- MISD: the closest example is a stream processor that performs a series of computation on a single data stream in a pipelined fashion.

- SIMD: From a programmer's perspective, this is close to SISD. So it would be easy to compile sequential code for SISD into a program running on SIMD. This reduces the cost of the control unit over dozens of execution units and instruct bandwidth and space. SIMD works best when dealing with arrays in `for` loops, in the case of _data-level parallelism_.

### Examples

- x86: MMX, SSE, AVX

- vector architecture: pipelined execution on a large set of vectors; good performance at lower cost; Vector instructions specify a great deal of work. Fewer pipeline stalls.

TODO

## Hardware Multithreading

Hardware multithreading allows multiple threads to share the functional units of a single processors in an overlapping fashion to try to utilize the hardware resources efficiently. The processor duplicates the independent state of each thread. Each thread has a separate copy of the register file and the program counter. The hardware supports the ability to change to a different thread relatively quickly.

- Fine-grained multithreading: switches between threads on each instruction, often in a round-robin fashion. Pipeline stall is not an issue since.

TODO

## Graphics Processing Units

Graphics processing evolved its own style of processing and terminology. GPUs are accelerators that supplement a CPU. GPUs does not rely on multiple caches to overcome the long latency to memory. GPUs rely on hardware multithreading to hide the latency to memory. The GPU memory is oriented toward bandwidth rather than latency. GPUs can accommodate many parallel processors (MIMD) as well as many threads.

A GPU is a MIMD composed of multithreaded SIMD processors. The Thread Block Scheduler hardware assigns blocks of threads to multithreaded SIMD processors. The machine object that the hardware creates, manages, schedules, and executes is a thread of SIMD instructions, a traditional thread containing exclusively SIMD instructions. The SIMD Thread Scheduler includes a controller that dispatches a certain thread to run on the multithreaded SIMD processor.

GPU hardware has two levels of hardware schedulers:

1. The _Thread Block Scheduler_ that assigns blocks of threads to multhreaded SIMD processors;

2. The SIMD Thread Scheduler within a SIMD processor which schedules when SIMD threads should run.

TODO

