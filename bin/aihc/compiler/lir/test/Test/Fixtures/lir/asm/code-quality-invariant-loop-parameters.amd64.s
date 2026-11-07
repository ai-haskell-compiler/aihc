	.text
	.p2align 4
nested:
	mov [rsp - 24], rbx
	mov [rsp - 16], r12
	mov [rsp - 8], r13
	xor r12d, r12d
	xor ebx, ebx
.Llir_0_1:
	cmp r12, [rsp - 24]
	jge .Llir_0_4
.Llir_0_2:
	mov r13, rbx
	add r13, [rsp - 16]
.Llir_0_3:
	mov r10, [rsp - 8]
	imul r13, r10
	imul r13, r14
	imul r13, r15
	imul r13, rbp
	imul r13, rdi
	imul r13, rsi
	imul r13, rdx
	imul r13, rcx
	imul r13, r8
	imul r13, r9
	imul r13, rax
	mov r10, [rsp + 16]
	imul r13, r10
	mov r10, [rsp + 8]
	imul r13, r10
	add r12, 0x1
	mov rbx, r13
	jmp .Llir_0_1
.Llir_0_4:
	mov rax, rbx
	ret 16
	.section .note.GNU-stack
