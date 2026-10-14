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
	.text
	.p2align 4
shifted64:
	mov r9, r12
	shl r9, 1
	mov rax, rbx
	and rax, r9
	mov r9, r12
	shr r9, 7
	mov rdx, r9
	xor rdx, rbx
	sar r12, 63
	mov rcx, rbx
	or rcx, r12
	ret
	.text
	.p2align 4
shifted32:
	mov r9, r12
	shl r9, 1
	mov r9d, r9d
	mov rax, r9
	or rax, rbx
	mov r9, r12
	shr r9, 31
	mov r9d, r9d
	mov rdx, rbx
	and rdx, r9
	movsxd r12, r12d
	sar r12, 31
	mov r12d, r12d
	mov rcx, rbx
	xor rcx, r12
	ret
	.text
	.p2align 4
complement:
	andn rax, rbx, r12
	andn edx, r13d, r14d
	ret
	.text
	.p2align 4
nested:
	ror rbx, 3
	andn rax, rbx, r12
	ret
	.text
	.p2align 4
shared_shift:
	mov rax, rbx
	shr rax, 7
	mov rdx, r12
	xor rdx, rax
	ret
	.text
	.p2align 4
variable_shift:
	mov rcx, r13
	and rcx, 0x1f
	shr rbx, cl
	mov ebx, ebx
	mov rax, r12
	and rax, rbx
	ret
	.text
	.p2align 4
partial_complement:
	mov r10d, 0xffffffff
	xor rbx, r10
	mov rax, r12
	and rax, rbx
	ret
	.section .note.GNU-stack
