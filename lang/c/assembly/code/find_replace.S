DEFAULT REL
    section .data
amazing_str: db   "amazing", 0x00
incredible_str: db  "incredible", 0x00
printout_fmt: db  `%s\n`, 0x00
scan_fmt:  db "%s", 0x00

    section .text
    global main

    extern printf
    extern scanf
    extern fgets
    extern stdin


is_amazing:
    push    rbp
    mov     rbp, rsp

    cld
    mov     rcx, 6
    lea     rsi, [amazing_str]

    xor     eax, eax

    repe    cmpsb               ; compare 6 bytes against amazing, 0x00 stops the comparison

    cmp     rcx, 0
    jnz      .return

.true:
    mov     eax, 1
.return:
    pop     rbp
    ret

main:
    push    rbp
    mov     rbp, rsp

    ;; input string starts at rbp-120, output string at rbp-328
    sub     rsp, 128            ; 120 bytes on the stack for the input string

    mov     rdx, [stdin],        ; stdin is the position where the FILE* is located, not the stdin address itself
    mov     esi, 120             ; 120 bytes buffer
    lea     rdi, [rsp+8]
    call    fgets wrt ..plt

    lea     rsi, [rsp+8]        ; recover the buffer as the source in $rsi

    sub     rsp, 216            ; allocate 200 bytes as the output buffer and 16 bytes to save two source/dest pointers
    lea     rdx, [rsp+16]

.find:
    mov     bl, [rsi]
    cmp     bl, 0x00            ; does the source string end here?
    je      .out_of_find


    mov     [rsp], rsi
    mov     [rsp+8], rdx        ; rsi and rdx are volative/caller-saved

    mov     rdi, rsi
    call    is_amazing

    mov     rsi, [rsp]
    mov     rdx, [rsp+8]        ; recover source/dest from the stack

    test    ax, 1               ; check if a replacement is needed
    jne      .replace

    mov     [rdx], bl           ; just copy the current character to the dest buffer
.next_character:
    inc     rsi                 ; to the next character
    inc     rdx
    jmp     .find
.replace:
    mov     rdi, rdx

    mov     [rsp], rsi
    mov     [rsp+8], rdx        ; save two pointers again
    call    replace_with_incredible
    mov     rsi, [rsp]
    mov     rdx, [rsp+8]

    add     rsi, 7              ; `amazing` and `incredible` are of different length
    add     rdx, 10
    jmp     .find
.out_of_find:
    mov     BYTE [rdx], 0x00

    lea     rdi, [printout_fmt]
    lea     rsi, [rsp+16]
    xor     al, al              ; critical, otherwise crash
    call    printf wrt ..plt

    add     rsp, 344            ; 128 + 216

    xor     eax, eax
    pop     rbp
    ret

replace_with_incredible:
    push    rbp
    mov     rbp, rsp

    cld
    mov     rcx, 10
    lea     rsi, [incredible_str]

    repe    movsb


    pop     rbp
    ret
