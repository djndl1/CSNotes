  segment .data
A       dd      1,2,3,4,5
B       dd      1,2,3,4,5
C       dd      0

  segment .text
  global main

main:
  push  rbp
  mov   rbp, rsp

  mov   eax, 0
  mov   ebx, 0
for:
  cmp   ebx, 5
  jge   end_for
  mov   ecx, [A+4*ebx]
  imul  ecx, [B+4*ebx]
  add   eax, ecx
  inc   ebx
  jmp   for
end_for:
  mov   [C], eax
  leave
  ret
