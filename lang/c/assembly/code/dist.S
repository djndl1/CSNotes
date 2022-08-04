  DEFAULT REL

  segment .data
A_x  dq  4
A_y  dq  5
B_x  dq 10
B_y  dq 13
dist dq 0

  segment .text
  global main

main:
  push rbp
  mov rbp, rsp

  mov rax, [A_x]
  sub rax, [B_x]
  mov rbx, [A_y]
  sub rbx, [B_y]

  imul rax, rax
  imul rbx, rbx
  add rax, rbx

  mov [dist], rax

  pop rbp
  ret
