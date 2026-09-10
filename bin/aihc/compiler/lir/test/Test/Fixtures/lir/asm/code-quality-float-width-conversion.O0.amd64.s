	.text
	.p2align 4
main:
	mov r9, rdi
	mov r8, rsi
	movd xmm0, r9d
	cvtss2sd xmm0, xmm0
	movq r9, xmm0
	movq xmm0, r8
	cvtsd2ss xmm0, xmm0
	movd r8d, xmm0
	mov rax, r9
	mov rdx, r8
	ret
	.section .note.GNU-stack
