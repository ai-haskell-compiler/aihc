	.text
	.p2align 4
update:
	mov r9, [r15]
	and r9, 0x3
	test r9, r9
	jne .Llir_0_2
.Llir_0_1:
	mov r11d, 0x1
	mov [r15 + 8], r11
	jmp .Llir_0_4
.Llir_0_2:
	sub rsp, 0x8
	mov rdi, r15
	xor eax, eax
	call slow
	movzx rax, al
	add rsp, 0x8
.Llir_0_3:
	test rax, rax
	je .Llir_0_5
.Llir_0_4:
	jmp done
.Llir_0_5:
	jmp again
	.section .note.GNU-stack
