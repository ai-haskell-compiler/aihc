	.text
	.p2align 4
immediates:
	mov rax, rbx
	and rax, -0x4
	mov rdx, rbx
	or rdx, 0xff00
	mov r10, 0x5555555555555555
	mov rcx, rbx
	xor rcx, r10
	lea rsi, [r12 + 20480]
	lea rdi, [r12 - 7]
	mov r8, r13
	and r8, -0x10
	cmp r12, -0x1
	setb r9b
	movzx r9, r9b
	cmp r13d, -0x1000
	setae r13b
	movzx r13, r13b
	mov r10, r13
	ret
	.text
	.p2align 4
chunk:
	cmp rbx, 0x1000
	jae .Llir_1_2
.Llir_1_1:
	mov eax, 0x1
	ret
.Llir_1_2:
	mov eax, 0x0
	ret
	.section .note.GNU-stack
