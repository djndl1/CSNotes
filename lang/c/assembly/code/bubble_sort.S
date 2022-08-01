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

    mov     edi, DWORD [arr+4*eax]
    cmp     edi, DWORD [arr+4+4*eax]
    jle     .next_for

    mov     edx, DWORD [arr+4+4*eax]
    mov     DWORD [arr+4+4*eax], edi
    mov     DWORD [arr+4*eax], edx
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
