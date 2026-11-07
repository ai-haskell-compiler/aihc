	.text
	.p2align 4
mix:
	mov [rsp - 32], rdi
	mov [rsp - 24], rsi
	mov [rsp - 16], rdx
	mov [rsp - 8], rcx
	mov rdi, rbx
	imul rdi, r12
	mov rsi, r13
	imul rsi, r14
	mov rdx, r15
	imul rdx, rbp
	mov rcx, rdi
	add rcx, rax
	sub rsp, 0x20
	mov r11, rbx
	mov rbx, rdi
	mov rdi, r13
	mov r13, rdx
	mov rdx, r15
	mov r15, r11
	mov r11, r12
	mov r12, rsi
	mov rsi, r14
	mov r14, rcx
	mov rcx, rbp
	mov rbp, r11
	mov r11, r8
	mov r8, [rsp]
	mov r10, [rsp + 32]
	mov [rsp], r10
	mov r10, [rsp + 24]
	mov [rsp + 32], r10
	mov [rsp + 24], r11
	mov r11, r9
	mov r9, [rsp + 8]
	mov [rsp + 8], rax
	mov rax, [rsp + 16]
	mov [rsp + 16], r11
	jmp wide
	.text
	.p2align 4
wide:
	add rbx, [rsp + 8]
	mov r10, [rsp]
	mov [rsp + 32], r10
	add rsp, 0x20
	jmp mix
	.section .note.GNU-stack
