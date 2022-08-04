DEFAULT REL
section     .bss
fib_array:  resd    100

section     .text
global      main

main:
    push    rbp
    mov     rbp, rsp

    mov     DWORD [fib_array], 0
    lea     r8, [fib_array]
    mov     DWORD [r8+4], 1
    mov     eax, 0
    xor     edx, edx

.for:
    test    dl, dl
    jnz     .end_for

    mov     ecx, DWORD [r8+4*rax]
    add     ecx, DWORD [r8+4+4*rax]

    seto    dl

    mov     DWORD [r8+8+4*rax], ecx

    inc     eax

    jmp     .for

.end_for:


    xor     eax, eax

    pop     rbp
    ret
