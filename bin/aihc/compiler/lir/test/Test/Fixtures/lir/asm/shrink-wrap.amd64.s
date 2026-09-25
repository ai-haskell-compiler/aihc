	.text
	.p2align 4
reserve:
	mov rbx, rdi
	mov r9, [rbx]
	mov r10d, 0x0
	cmp r9, r10
	jne .Llir_0_2
.Llir_0_1:
	sub rsp, 0x18
	lea r12, [rsp]
	mov [r12], rsi
	mov rdi, rbx
	mov rsi, r12
	mov eax, 0x0
	call collect
	mov rsi, [r12]
	add rsp, 0x18
.Llir_0_2:
	mov rdi, rbx
	jmp next
	.section .note.GNU-stack
