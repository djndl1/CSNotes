    section .data
hello:  db  "Hello World", 0x0a

    section .text
    global main
    extern write, exit
main:
    push    rbp
    mov     rbp, rsp

    mov     rdi, 1
    lea     rsi, [hello]
    mov     rdx, 13

    call    write

    mov     edi, 0
    call    exit

    pop     rbp
    ret
