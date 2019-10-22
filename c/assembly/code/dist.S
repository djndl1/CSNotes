  segment .data
ax  dq  4
ay  dq  5
bx  dq 10
by  dq 13
dist dq 0

  segment .text
  global _start
_start:
  push rbx
  push rax
  push rdx

  mov rax, [ax]
  sub rax, [bx]
  mov rbx, [ay]
  sub rbx, [by]

  imul rax, rax
  imul rbx, rbx
  add rax, rbx

  mov [dist], rax
  ret
