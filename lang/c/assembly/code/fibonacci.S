section     .bss
fib_array:  resd    100

section     .text
global      main

main:
    push    rbp
    mov     rbp, rsp

    mov     DWORD [fib_array], 0
    mov     DWORD [fib_array+4], 1
    mov     eax, 0
    xor     edx, edx

for:
    test    dl, dl
    jnz     end_for

    mov     ecx, DWORD [fib_array+4*eax]
    add     ecx, DWORD [fib_array+4+4*eax]

    seto    dl

    mov     DWORD [fib_array+8+4*eax], ecx

    inc     eax

    jmp     for

end_for:


    xor     eax, eax

    pop     rbp
    ret
