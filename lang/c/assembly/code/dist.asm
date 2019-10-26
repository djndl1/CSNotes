  segment .data
aa  dq  4
b  dq  5
c  dq 10
d  dq 13
dist dq 0

  segment .text
  global _start

_start:
  push rbx
  push rax
  push rdx

  mov rax, [aa]
  sub rax, [c]
  mov rbx, [b]
  sub rbx, [d]

  imul rax, rax
  imul rbx, rbx
  add rax, rbx

  mov [dist], rax
  leave
  ret
