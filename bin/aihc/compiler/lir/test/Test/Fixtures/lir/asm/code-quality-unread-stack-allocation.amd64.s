	.text
	.p2align 4
cons:
	lea r9, [rbx + 24]
	cmp r9, r12
	ja .Llir_0_2
.Llir_0_1:
	mov [rbx + 8], r15
	mov [rbx + 16], rdi
	mov r15, rbx
	mov rbx, r9
	jmp next
.Llir_0_2:
	jmp replay
	.section .note.GNU-stack
