	.text
	.p2align 4
reserve:
	mov r9, [rbx]
	mov r10d, 0x0
	cmp r9, r10
	jne .Llir_0_2
.Llir_0_1:
	sub rsp, 0x18
	lea r13, [rsp]
	mov [r13], r12
	mov rdi, rbx
	mov rsi, r13
	mov eax, 0x0
	call collect
	mov r12, [r13]
	add rsp, 0x18
.Llir_0_2:
	jmp next
	.section .note.GNU-stack
