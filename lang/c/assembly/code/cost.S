segment .data
  cost  dq      0
  pkh   dq      3
  kh    dq      500
  dol   dq      0
  pen   dq      0

  segment .text
  global main

main:
  mov   rax, [kh]
  mul   qword [pkh]
  add   rax, 5
  mov   rbx, 1000
  div   rbx
  mov   [dol], rax
  mov   rax, rdx
  mov   rdx, 0
  mov   rbx, 100
  mul   qword rbx
  mov   [pen], rax
  ret

