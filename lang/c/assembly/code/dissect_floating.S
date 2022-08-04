DEFAULT REL
segment .data
  f     dq      -1.25
  sign  db      0
  frac  dq      0
  exp   dd      0

segment .text
  global main

main:
  mov   rax, [f]
  mov   rbx, 0xfffffffffffff
  and   rbx, rax
  mov   [frac], rbx
  shr   rax, 52
  mov   rbx, 0x7ff
  and   rbx, rax
  sub   rbx, rbx
  mov   [exp], rbx
  shr   rax, 11
  mov   rbx, 0x1
  and   rbx, rax
  mov   [sign], rbx
  ret
