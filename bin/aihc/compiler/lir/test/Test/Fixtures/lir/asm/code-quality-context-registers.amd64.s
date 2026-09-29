	.text
	.p2align 4
step:
	sub rsp, 0x8
	lea rdi, [r15 + 1]
	mov eax, 0x0
	call work
	imul r15, rax
	add rsp, 0x8
	cmp r15, 0x64
	jge .Llir_0_2
.Llir_0_1:
	jmp next
.Llir_0_2:
	add r15, -0x64
	jmp next
	.section .note.GNU-stack
