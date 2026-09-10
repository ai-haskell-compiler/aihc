	.text
	.p2align 4
sum:
	mov r9, rdi
	mov r8, r9
	mov r9d, 0x0
.Llir_0_1:
	cmp r8, 0x0
	sete cl
	movzx rcx, cl
	test rcx, rcx
	je .Llir_else_0
	jmp .Llir_0_3
.Llir_else_0:
	mov r11, r9
	mov r9, r8
	mov r8, r11
.Llir_0_2:
	lea rcx, [r9 - 1]
	add r9, r8
	mov r8, rcx
	jmp .Llir_0_1
.Llir_0_3:
	mov rax, r9
	ret
	.section .note.GNU-stack
