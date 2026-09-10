	.text
	.p2align 4
wide:
	mov rax, r8
	mov r8, rsi
	mov rsi, r9
	mov r9, rdi
	mov r11, rcx
	mov rcx, rdx
	mov rdx, r11
	mov rdi, r9
	add rdi, r8
	add rdx, rcx
	add rax, rsi
	imul rdx, rdi
	imul r9, rax
	add r9, rdx
	add r9, r8
	add r9, rcx
	mov rax, r9
	ret
	.section .note.GNU-stack
