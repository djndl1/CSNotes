    section .data

    word1   db  "REFER",0x00
    word2   db  "REFERE",0x00

    section .text
    global main
    extern strlen

main:
    push    rbp
    mov     rbp, rsp

    lea     rdi, [word1]
    call    is_palindrome

    lea     rdi, [word2]
    call    is_palindrome



    pop     rbp
    ret


is_palindrome:
    push    rbp
    mov     rbp, rsp

    call    strlen

    cmp     rax, 2
    jl      .yes

    sub     rax, 1
    mov     rbx, rax
    lea     rax, BYTE [word1]
    add     rax, rbx

    mov     rsi, rax

.comp_loop:
    cmp     rdi, rsi
    je      .yes

    mov     al, BYTE [rdi]
    mov     dl, BYTE [rsi]
    cmp     al, dl

    jne     .no

    inc     rdi
    dec     rsi

    jmp     .comp_loop

.yes:
    mov     eax, 1
    pop     rbp
    ret

.no:
    xor     eax, eax
    pop     rbp
    ret
