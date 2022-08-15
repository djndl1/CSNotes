DEFAULT REL
    section .data
scanf_int  db  `%d`,0x00
printf_fmt  db  `%d^2 + %d^2 = %d^2\n`, 0x00

    section .text

    global main
    extern printf, scanf

is_pythagorean:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 16

    mov     [rsp+8], edx

    mov     eax, edi
    mul     eax

    mov     [rsp+12], eax

    mov     eax, esi
    mul     eax

    mov     ecx, eax

    mov     eax, DWORD [rsp+8]
    mul     eax
    add     eax, ecx

    cmp     [rsp+12], eax

    setz    al

    add     rsp, 16
    pop     rbp
    ret

main:
    push    rbp
    mov     rbp, rsp

    sub     rsp, 32
    lea     rsi, [rsp+24]
    lea     rdi, [scanf_int]
    xor     al, al

    call    scanf wrt ..plt               ;rsi, rdi free

    mov     eax, DWORD [rsp+24]
    mul     eax

    shr     eax, 1                 ;rdx free

    mov     [rsp+20], eax       ;eax free; [rsp+20] as the third

	mov     [rsp+8], rbx        ; save rbx

    mov     ebx, 2              ; take as the first
.find_second:
    cmp     [rsp+24], ebx
    jl      .find_end

    mov     eax, 1
.find_third:
    cmp     [rsp+24], eax
    jl      .end_third

    mov     [rsp+4], eax

    mov     edi, DWORD [rsp+24]
    mov     esi, ebx
    mov     edx, eax

    call    is_pythagorean

    test    eax, 0x01
    jz      .post_increment

    lea     rdi, [printf_fmt]
    mov     esi, DWORD [rsp+4]
    mov     edx, ebx
    mov     ecx, DWORD [rsp+24]
    xor     eax, eax
    call    printf wrt ..plt

.post_increment:
    mov     eax, DWORD [rsp+4]
    inc     eax
    jmp     .find_third
.end_third:
    inc     ebx
    jmp     .find_second
.find_end:

    mov     rbx, [rsp+8]
    xor     eax, eax
    add     rsp, 32
    pop     rbp
    ret
