	.text
	.p2align 4
count:
	test rbx, rbx
	jne .Llir_0_1
	mov rbx, r12
	jmp .Llir_0_2
.Llir_0_1:
	lea r9, [rbx - 1]
	add r12, rbx
	mov rbx, r9
	jmp count
.Llir_0_2:
	jmp done
	.section .note.GNU-stack
