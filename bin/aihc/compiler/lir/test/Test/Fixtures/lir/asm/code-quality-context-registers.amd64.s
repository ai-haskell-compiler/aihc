	.text
	.p2align 4
step:
	push rbp
	mov rbp, rsp
	sub rsp, 0x10
	mov [rsp], rbx
	mov rbx, rdi
	lea rdi, [rbx + 1]
	mov eax, 0x0
	call work
	mov rdi, rax
	imul rdi, rbx
	cmp rdi, 0x64
	jge .Llir_0_2
.Llir_0_1:
	mov rbx, [rsp]
	mov rsp, rbp
	pop rbp
	jmp next
.Llir_0_2:
	add rdi, -0x64
	mov rbx, [rsp]
	mov rsp, rbp
	pop rbp
	jmp next
	.section .note.GNU-stack
