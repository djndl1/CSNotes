segment .data
  a     dq      0xaf
  b     dq      0x3a

segment .text
  global main

main:
  mov   rax, [a]
  xor   rax, [b]
  xor   [b], rax
  xor   rax, [b]
  mov   [a], rax
  ret
