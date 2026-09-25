	.text
	.p2align 4
reserve:
	push rbp
	mov rbp, rsp
	mov r12, rsi
	mov rbx, rdx
	mov r9, [rdi + 24]
	mov r8, [rdi + 32]
	sub r8, r9
	cmp r8, 0x10
	jae .Llir_0_2
.Llir_0_1:
	mov r13, rdi
	mov esi, 0x2
	mov eax, 0x0
	call collect
	mov rdi, r13
.Llir_0_2:
	mov rcx, [rdi + 24]
	lea r9, [rcx + 16]
	mov [rdi + 24], r9
	mov [rcx + 8], rbx
	mov rsi, r12
	mov rdx, rbx
	mov rsp, rbp
	pop rbp
	jmp next
	.section .note.GNU-stack
