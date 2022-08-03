    section .data
hello:  db  "Hello World", 0x0a

    section .text
    global main

main:
    push    rbp
    mov     rbp, rsp

    mov     rax, 1
    mov     edi, 1
    lea     rsi, [hello]
    mov     rdx, 13
    syscall

    mov     eax, 60
    xor     edi, edi
    syscall
