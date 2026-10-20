	.text
	.p2align 4
limb:
	mov r9, r15
	shl r9, 3
	mov rbx, [rbx + 16]
	add rbx, r9
	mov rbx, [rbx]
	mov rax, rbx
	mul r13
	mov r13, rax
	mov rbx, rdx
	add r15, r14
	shl r15, 3
	mov r12, [r12 + 16]
	add r12, r15
	mov r9, [r12]
	mov r11, r13
	add r11, r9
	setc r13b
	movzx r13, r13b
	mov r9, r11
	add r11, rdi
	setc r9b
	movzx r9, r9b
	mov rdi, r11
	add rbx, r13
	add rbx, r9
	mov [r12], rdi
	mov rax, rdi
	mov rdx, rbx
	ret
	.section .note.GNU-stack
