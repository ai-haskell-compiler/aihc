	.text
	.p2align 4
store_byte:
	movzx rsi, sil
	mov r9, rdi
	mov r8, rsi
	mov rcx, rdx
	mov [r9], r8b
	movzx r9, [r9]
	mov rax, r9
	ret
	.text
	.p2align 4
main:
	push rbp
	mov rbp, rsp
	sub rsp, 0x10
	mov [rsp], 0x0
	lea r9, [rsp]
	mov rdi, r9
	mov esi, 0x25
	mov edx, 0x0
	mov eax, 0x0
	call store_byte
	movzx rax, al
	mov r9, rax
	mov rax, r9
	mov rsp, rbp
	pop rbp
	ret
	.section .note.GNU-stack
