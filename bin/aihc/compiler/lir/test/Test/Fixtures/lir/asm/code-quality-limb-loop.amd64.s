	.text
	.p2align 4
limb:
	mov rbx, [rbx + 16]
	mov rbx, [rbx + r15*8]
	mov rdx, rbx
	mulx rbx, r13, r13
	add r15, r14
	mov r12, [r12 + 16]
	lea r12, [r12 + r15*8]
	mov r9, [r12]
	add r9, r13
	adc rbx, 0x0
	add rdi, r9
	adc rbx, 0x0
	mov [r12], rdi
	mov rax, rdi
	mov rdx, rbx
	ret
	.section .note.GNU-stack
