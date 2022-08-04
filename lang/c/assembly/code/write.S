DEFAULT REL
  segment .data
msg:    db      "Hello World!",0x0a
len:    equ     $-msg           ; the current assembly point minus the address of msg

  segment .text
  global        main
  extern        write, exit

main:
  push  rbp
  mov   rbp, rsp

  mov   rdx, len
  lea   rsi, [msg]
  mov   rdi, 1
  call  write wrt ..plt
  xor   rdi, rdi
  call  exit wrt ..plt
