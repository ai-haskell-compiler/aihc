	.text
	.p2align 4
allocate:
	mov r11, [rip + info@GOTPCREL]
	mov [r12], r11
	mov [r12 + 8], r15
	mov r11, [rip + info@GOTPCREL]
	mov [r12 + 16], r11
	mov [r12 + 24], r12
	mov [r12 + 32], r15
	lea rdi, [r12 + 16]
	lea r12, [r12 + 40]
	jmp next
	.section .note.GNU-stack
