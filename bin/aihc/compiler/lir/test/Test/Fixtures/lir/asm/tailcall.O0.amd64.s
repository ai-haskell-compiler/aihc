	.text
	.p2align 4
count:
	mov r8, rdi
	mov r9, rsi
	cmp r8, 0x0
	sete cl
	movzx rcx, cl
	test rcx, rcx
	je .Llir_else_0
	jmp .Llir_0_2
.Llir_else_0:
	mov r11, r9
	mov r9, r8
	mov r8, r11
.Llir_0_1:
	lea rcx, [r9 - 1]
	add r9, r8
	mov rdi, rcx
	mov rsi, r9
	jmp count
.Llir_0_2:
	mov rdi, r9
	jmp done
	.section .note.GNU-stack
