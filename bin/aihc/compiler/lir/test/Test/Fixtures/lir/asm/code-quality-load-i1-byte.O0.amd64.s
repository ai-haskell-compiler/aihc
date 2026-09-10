	.text
	.p2align 4
main:
	lea r10, [rip + bytes]
	movzx r9, [r10]
	and r9, 0x1
	lea r10, [rip + bytes]
	movzx r8, [r10 + 1]
	and r8, 0x1
	mov rax, r9
	mov rdx, r8
	ret
	.section .rodata
	.p2align 0
bytes:
	.byte 0x2
	.byte 0x3
	.section .note.GNU-stack
