DEFAULT REL
section .data
    A       dq  1.5, 2.5, 3.5, 4.5, 5.5
    B       dq  5.5, 4.5, 3.5, 2.5, 1.5
    prod    dq  0.0

section .text
global main

main:
    push rbp
    mov rbp, rsp

    xor eax, eax
    xorpd xmm0, xmm0

for:
    cmp eax, 5
    jge end_for

    lea     r8, [A]
    movsd   xmm1, [r8+rax*8]
    lea     r8, [B]
    mulsd   xmm1, [r8+rax*8]
    addsd   xmm0, xmm1

    inc eax

    jmp for
end_for:
    movsd [prod], xmm0

    xor eax, eax

    pop rbp
    ret
