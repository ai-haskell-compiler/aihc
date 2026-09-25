	.text
	.p2align 4
immediates:
	mov rax, rdi
	and rax, -0x4
	mov r9, rdi
	or r9, 0xff00
	mov r10, 0x5555555555555555
	mov rcx, rdi
	xor rcx, r10
	lea r8, [rsi + 20480]
	lea rdi, [rsi - 7]
	mov rbx, rdx
	and rbx, -0x10
	cmp rsi, -0x1
	setb sil
	movzx rsi, sil
	cmp edx, -0x1000
	setae dl
	movzx rdx, dl
	mov r10, rdx
	mov rdx, r9
	mov r9, rsi
	mov rsi, r8
	mov r8, rbx
	ret
	.text
	.p2align 4
chunk:
	cmp rdi, 0x1000
	jae .Llir_1_2
.Llir_1_1:
	mov eax, 0x1
	ret
.Llir_1_2:
	mov eax, 0x0
	ret
	.section .note.GNU-stack
