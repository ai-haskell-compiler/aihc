	.text
	.p2align 4
spread:
	mov rax, r8
	mov rbx, [rsp + 16]
	mov r12, [rsp + 24]
	mov r13, [rsp + 32]
	mov r8, rsi
	mov rsi, r9
	mov r9, rdi
	mov rdi, [rsp + 8]
	mov r11, rcx
	mov rcx, rdx
	mov rdx, r11
	add r9, r13
	mov [rsp + 8], rdi
	mov [rsp + 16], rbx
	mov [rsp + 24], r12
	mov [rsp + 32], r13
	mov rdi, r9
	mov r9, rsi
	mov rsi, r8
	mov r8, rax
	mov r11, rdx
	mov rdx, rcx
	mov rcx, r11
	jmp spread
	.section .note.GNU-stack
