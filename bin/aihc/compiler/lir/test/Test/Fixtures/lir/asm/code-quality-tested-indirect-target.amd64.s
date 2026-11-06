	.text
	.p2align 4
return:
	sub rsp, 0x18
	mov [rsp], r13
	mov r9, [r15]
	mov r13, [r9 + 24]
	test r13, r13
	je .Llir_0_4
.Llir_0_1:
	cmp r15, r14
	je .Llir_0_3
.Llir_0_2:
	mov rdi, r15
	xor eax, eax
	call enter_chunk
.Llir_0_3:
	mov r10, r13
	mov rdi, rbp
	mov r13, r15
	xor ebp, ebp
	add rsp, 0x18
	jmp r10
.Llir_0_4:
	mov r13, [rsp]
	add rsp, 0x18
	jmp forward
	.section .note.GNU-stack
