	.text
	.p2align 4
spread:
	add rbx, [rsp + 16]
	jmp spread
	.section .note.GNU-stack
