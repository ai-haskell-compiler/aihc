	.text
	.p2align 4
ident:
	movq r11, xmm0
	mov r11d, r11d
	mov r9, r11
	mov rax, r9
	movd xmm0, eax
	ret
	.text
	.p2align 4
main:
	push rbp
	mov rbp, rsp
	mov r11d, 0x40600000
	movd xmm0, r11d
	mov eax, 0x1
	call ident
	movd eax, xmm0
	mov eax, eax
	mov r9, rax
	mov rax, r9
	mov rsp, rbp
	pop rbp
	ret
	.section .note.GNU-stack
