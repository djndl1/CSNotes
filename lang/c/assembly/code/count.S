DEFAULT REL
segment .data
  a      db     0xaf

segment .text
  global main

main:
  mov   rbx, [a]
  xor   rax, rax
  
