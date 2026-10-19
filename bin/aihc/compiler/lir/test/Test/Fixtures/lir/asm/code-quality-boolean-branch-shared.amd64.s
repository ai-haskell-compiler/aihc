	.text
	.p2align 4
shared:
	mov rax, rbx
	test rbx, rbx
	jne .Llir_0_2
	mov r9, rax
.Llir_0_1:
	mov rax, r9
	ret
.Llir_0_2:
	ret
	.text
	.p2align 4
partial:
	cmp rbx, 0x1
	je .Llir_1_1
	jmp .Llir_trap_0
.Llir_1_1:
	mov eax, 0xb
	ret
	.text
	.p2align 4
signed:
	neg rbx
	cmp rbx, 0x1
	je .Llir_2_1
	jmp .Llir_2_2
.Llir_2_1:
	mov eax, 0xb
	ret
.Llir_2_2:
	mov eax, 0x16
	ret
	.text
	.p2align 4
integer:
	cmp rbx, 0x1
	je .Llir_3_1
	jmp .Llir_3_2
.Llir_3_1:
	mov eax, 0xb
	ret
.Llir_3_2:
	mov eax, 0x16
	ret
	.text
	.p2align 4
.Llir_trap_0:
	lea rsi, [rip + .Llir_trap_message_0]
	mov edx, 0x1f
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
	.byte 0x73, 0x77, 0x69, 0x74, 0x63, 0x68, 0x20, 0x77, 0x69, 0x74, 0x68, 0x6f, 0x75, 0x74, 0x20, 0x61, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x69, 0x6e, 0x67, 0x20, 0x63, 0x61, 0x73, 0x65, 0xa
	.section .note.GNU-stack
