    section .data
hello:  db  "Hello World", 0x0a

    section .text
    global main

main:
    push    ebp
    mov     ebp, esp

    mov     eax, 0x04
    mov     ebx, 0x01
    lea     ecx, [hello]
    mov     edx, 13
    int     0x80

    pop     ebp
    ret
