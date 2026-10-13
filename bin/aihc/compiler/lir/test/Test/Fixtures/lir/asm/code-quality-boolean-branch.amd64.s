	.text
	.p2align 4
one:
	cmp rbx, r12
	jge .Llir_0_2
.Llir_0_1:
	mov eax, 0xb
	ret
.Llir_0_2:
	mov eax, 0x16
	ret
	.text
	.p2align 4
zero:
	test rbx, rbx
	je .Llir_1_2
.Llir_1_1:
	mov eax, 0xb
	ret
.Llir_1_2:
	mov eax, 0x16
	ret
	.section .note.GNU-stack
