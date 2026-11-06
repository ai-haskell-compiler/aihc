	.text
	.p2align 4
return:
	sub rsp, 0x18
	mov [rsp], rbx
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
	mov r13, r15
	mov rdi, rbp
	mov rbx, [rsp]
	xor ebp, ebp
	add rsp, 0x18
	jmp r10
.Llir_0_4:
	mov rbx, [rsp]
	add rsp, 0x18
	jmp forward
	.section .note.GNU-stack
