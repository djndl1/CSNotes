segment .data
  cost  dq      0
  pkh   dq      3
  kh    dq      1500
  dol   dq      0
  pen   dq      0

  segment .text
  global main

main:
  push rbp
  mov rbp, rsp

  mov rax, [kh]
  sub rax, 1000
  cmovg rbx, rax
  imul rbx, [pkh]

  add rbx, 500
  mov [cost], rbx

  pop rbp
  ret
