	.text
	.p2align 4
chain:
	add ebx, r12d
	mov r10d, 0xffffffff
	imul ebx, r10d
	sub ebx, r12d
	mov rax, rbx
	ret
	.text
	.p2align 4
unknown:
	add rbx, r12
	mov eax, ebx
	ret
	.text
	.p2align 4
shared:
	mov rax, rbx
	add rax, r12
	mov edx, eax
	ret
	.text
	.p2align 4
wide:
	mov rax, rbx
	test r12, r12
	je .Llir_trap_0
	xor edx, edx
	div r12
	mov r12, rax
	mov r12d, r12d
	shr rbx, 32
	mov ebx, ebx
	mov rax, r12
	mov rdx, rbx
	ret
	.text
	.p2align 4
sigma:
	rorx r9d, ebx, 2
	rorx r8d, ebx, 13
	ror ebx, 22
	xor r9, r8
	xor r9, rbx
	mov rax, r9
	ret
	.text
	.p2align 4
.Llir_trap_0:
	lea rsi, [rip + .Llir_trap_message_0]
	mov edx, 0x19
	jmp .Llir_trap
	.p2align 4
.Llir_trap:
	mov edi, 0x2
	and rsp, -0x10
	call write
	mov edi, 0x1
	call _exit
	ud2
	.section .rodata
.Llir_trap_message_0:
	.byte 0x69, 0x6e, 0x74, 0x65, 0x67, 0x65, 0x72, 0x20, 0x64, 0x69, 0x76, 0x69, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x62, 0x79, 0x20, 0x7a, 0x65, 0x72, 0x6f, 0xa
	.section .note.GNU-stack
