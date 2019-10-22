segment .data
  a     dq      55
  b     dq      45
  c     dq      89
  d     dq      54
  aver  dq      0
  rem   dq      0

segment .text
  global main

main:
  mov   rax, [a]
  add   rax, [b]
  add   rax, [c]
  add   rax, [d]
  mov   rdx, 0
  mov   rbx, 4
  div   rbx
  mov   [aver], rax
  mov   [rem], rdx
  ret
