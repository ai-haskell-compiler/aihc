	.text
	.p2align 4
store_byte:
	movzx rsi, sil
	mov [rdi], sil
	movzx rax, [rdi]
	ret
	.text
	.p2align 4
main:
	sub rsp, 0x18
	lea rdi, [rsp]
	mov esi, 0x25
	mov edx, 0x0
	mov eax, 0x0
	call store_byte
	movzx rax, al
	add rsp, 0x18
	ret
	.section .note.GNU-stack
