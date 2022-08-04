DEFAULT REL
    section     .data
    arr     dd  3,45,34,6,1,3,6,7
    cnt     db  8

    section     .text
    global      main

main:
    push    rbp
    mov     rbp, rsp

.do_begin:
    xor     cl, cl

    mov     esi, [cnt]
    sub     esi, 1

    xor     eax, eax
.for:
    cmp     eax, esi
    jge     .end_for

    lea     r8, [arr]
    mov     edi, DWORD [r8+4*rax]
    cmp     edi, DWORD [r8+4+4*rax]
    jle     .next_for

    mov     edx, DWORD [r8+4+4*rax]
    mov     DWORD [r8+4+4*rax], edi
    mov     DWORD [r8+4*rax], edx
    mov     cl, 1

.next_for:
    inc     eax
    jmp     .for
.end_for:

    test    cl, cl
    jnz     .do_begin
.do_end:

    pop     rbp
    ret
