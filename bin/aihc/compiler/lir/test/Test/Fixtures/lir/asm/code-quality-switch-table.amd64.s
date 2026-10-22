	.text
	.p2align 4
dense:
	cmp rbx, 0x6
	je .Llir_0_4
	jb .Llir_switch_lower_0
	cmp rbx, 0x7
	je .Llir_0_5
	cmp rbx, 0x8
	je .Llir_0_6
	jmp .Llir_0_7
.Llir_switch_lower_0:
	cmp rbx, 0x2
	je .Llir_0_1
	cmp rbx, 0x3
	je .Llir_0_2
	cmp rbx, 0x4
	je .Llir_0_3
	jmp .Llir_0_7
.Llir_0_1:
	mov eax, 0x14
	ret
.Llir_0_2:
	mov eax, 0x1e
	ret
.Llir_0_3:
	mov eax, 0x28
	ret
.Llir_0_4:
	mov eax, 0x3c
	ret
.Llir_0_5:
	mov eax, 0x46
	ret
.Llir_0_6:
	mov eax, 0x50
	ret
.Llir_0_7:
	xor eax, eax
	ret
	.section .note.GNU-stack
