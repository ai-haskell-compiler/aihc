	.text
	.p2align 4
reserve:
	lea r9, [r12 + 16]
	cmp r9, r13
	ja .Llir_0_1
	mov rdx, r12
	jmp .Llir_0_2
.Llir_0_1:
	sub rsp, 0x8
	mov [rbx + 24], r12
	mov r12, rsi
	mov r13, rdi
	mov rdi, rbx
	mov esi, 0x2
	mov eax, 0x0
	call collect
	mov rsi, r12
	mov rdi, r13
	mov rdx, [rbx + 24]
	mov r13, [rbx + 32]
	add rsp, 0x8
.Llir_0_2:
	lea r12, [rdx + 16]
	mov [rdx + 8], rsi
	jmp next
	.section .note.GNU-stack
