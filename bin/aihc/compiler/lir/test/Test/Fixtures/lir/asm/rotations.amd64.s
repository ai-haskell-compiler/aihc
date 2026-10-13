	.text
	.p2align 4
constant:
	rorx r9d, ebx, 7
	rorx edx, r9d, 23
	mov rax, rbx
	ret
	.text
	.p2align 4
variable:
	mov rcx, r12
	mov rax, rbx
	ror rax, cl
	mov rdx, rax
	rol rdx, cl
	ret
	.text
	.p2align 4
pattern:
	rorx eax, ebx, 7
	ret
	.text
	.p2align 4
sigma:
	rorx r9d, ebx, 7
	ror ebx, 18
	mov rax, r9
	xor rax, rbx
	ret
	.text
	.p2align 4
narrow:
	mov rcx, r12
	mov rax, rbx
	rol al, cl
	mov rcx, r14
	mov rdx, r13
	ror dx, cl
	ret
	.text
	.p2align 4
wide_pattern:
	ror ebx, 7
	mov rax, rbx
	ret
	.text
	.p2align 4
constant64:
	rorx rdx, rbx, 23
	mov rax, rbx
	ret
	.section .note.GNU-stack
