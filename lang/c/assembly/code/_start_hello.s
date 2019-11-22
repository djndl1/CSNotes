/* specify --dynamic-linker -lc -L/path/to/libc/ when linking*/

  .text
  .global _start
  .extern printf, exit
  .type _start,@function
_start:
  pushq  %rbp
  movq   %rsp, %rbp
  lea   hello(%rip), %rdi
  mov   $0, %rax
  callq printf
  xor   %rdi, %rdi
  callq exit

  .data
hello:  .asciz   "hello\n"
