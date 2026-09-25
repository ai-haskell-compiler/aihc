	.text
	.p2align 4
ident:
	movq r11, xmm0
	mov r11d, r11d
	mov rdi, r11
	mov rax, rdi
	movd xmm0, eax
	ret
	.text
	.p2align 4
main:
	sub rsp, 0x8
	mov r11d, 0x40600000
	movd xmm0, r11d
	mov eax, 0x1
	call ident
	movd eax, xmm0
	mov eax, eax
	add rsp, 0x8
	ret
	.section .note.GNU-stack
