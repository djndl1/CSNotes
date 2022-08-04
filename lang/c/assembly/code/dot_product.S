  DEFAULT REL
  segment .data

extern  _GLOBAL_OFFSET_TABLE
A       dd      1,2,3,4,5
B       dd      1,2,3,4,5
C       dd      0

  segment .text
  global main

main:
  push  rbp
  mov   rbp, rsp

  mov   eax, 0
  mov   edx, 0
.for:
  cmp   edx, 5

  jge   .end_for
  lea   rdi, [A]
  mov   ecx, [rdi+4*rdx]
  lea   rdi, [B]
  imul  ecx, [rdi+4*rdx]
  add   eax, ecx
  inc   edx
  jmp   .for
.end_for:
  mov   [C], eax
  leave
  ret
