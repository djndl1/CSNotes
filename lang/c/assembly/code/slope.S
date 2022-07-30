segment .data
	xdiff   dd   0
	ydiff   dd   0
	slope   dd   0
	a_x      dd  40
	a_y      dd  50
	b_x      dd 100
	b_y      dd 130

section .text
global main

main:
    push rbp
    mov rbp, rsp

    mov eax, DWORD [b_x]
    sub eax, DWORD [a_x]
    mov [xdiff], eax

    mov eax, DWORD [b_y]
    sub eax, DWORD [a_y]
    mov [ydiff], eax

    xor rdx, rdx
    idiv DWORD [xdiff]

    pop rbp
    ret
