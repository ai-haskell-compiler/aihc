	.text
	.p2align 4
return:
	push rbp
	mov rbp, rsp
	sub rsp, 0x10
	mov [rsp], rbx
	mov [rsp + 8], rdi
	mov r9, [r15]
	mov rbx, [r9 + 24]
	xor r10d, r10d
	cmp rbx, r10
	je .Llir_0_4
.Llir_0_1:
	cmp r15, r14
	je .Llir_0_3
.Llir_0_2:
	mov rdi, r15
	xor eax, eax
	call enter_chunk
.Llir_0_3:
	mov r10, rbx
	mov rbx, [rsp]
	mov r13, r15
	xor edi, edi
	mov rsi, [rsp + 8]
	mov rsp, rbp
	pop rbp
	jmp r10
.Llir_0_4:
	mov rbx, [rsp]
	mov rdi, [rsp + 8]
	mov rsp, rbp
	pop rbp
	jmp forward
	.section .note.GNU-stack
