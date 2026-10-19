	.text
	.p2align 4
which:
	cmp rbx, [rip + first@GOTPCREL]
	je .Llir_0_2
.Llir_0_1:
	cmp rbx, [rip + second@GOTPCREL]
	jne .Llir_0_4
	jmp .Llir_0_3
.Llir_0_2:
	mov eax, 0x1
	ret
.Llir_0_3:
	mov eax, 0x2
	ret
.Llir_0_4:
	xor eax, eax
	ret
	.section .rodata
	.p2align 3
first:
	.byte 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
	.section .rodata
	.p2align 3
second:
	.byte 0x2, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
	.section .note.GNU-stack
