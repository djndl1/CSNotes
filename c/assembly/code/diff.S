segment .data
  a     dq      3
  b     dq      5
  c     dq      6
  d     dq      12
  diffx  dq      0
  diffy dq      0

segment .text
  global main

main:
  mov   rax, [a]
  sub   rax, [b]
  mov   [diffx], rax

  mov   rax, [c]
  sub   rax, [d]
  mov   [diffy], rax
  mov   rax, 1
  mov   rbx, 0
  cmovz rax, rbx
  
  ret
