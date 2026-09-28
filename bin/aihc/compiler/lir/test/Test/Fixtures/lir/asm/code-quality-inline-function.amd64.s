	.text
	.p2align 4
	.globl enter_direct
enter_direct:
	mov r9, [r12]
	and r9, -0x4
	mov r9, [r9]
	mov r12, [r12 + 8]
	mov r10, r9
	test r10, r10
	je .Llir_trap_0
	jmp r10
	.text
	.p2align 4
	.globl enter_inline
enter_inline:
	mov r9, [r12]
	and r9, -0x4
	mov r9, [r9]
	mov r12, [r12 + 8]
	mov r10, r9
	test r10, r10
	je .Llir_trap_0
	jmp r10
	.text
	.p2align 4
.Llir_trap_0:
	lea rsi, [rip + .Llir_trap_message_0]
	mov edx, 0x20
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
	.byte 0x69, 0x6e, 0x64, 0x69, 0x72, 0x65, 0x63, 0x74, 0x20, 0x63, 0x61, 0x6c, 0x6c, 0x20, 0x74, 0x6f, 0x20, 0x61, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0xa
	.section .note.GNU-stack
