	.text
	.p2align 4
pick:
	cmp rbx, 0x7
	je .Llir_0_4
	jb .Llir_switch_lower_0
	cmp rbx, 0x9
	je .Llir_0_5
	cmp rbx, -0x3
	je .Llir_0_6
	jmp .Llir_0_7
.Llir_switch_lower_0:
	cmp rbx, 0x1
	je .Llir_0_1
	cmp rbx, 0x3
	je .Llir_0_2
	cmp rbx, 0x5
	je .Llir_0_3
	jmp .Llir_0_7
.Llir_0_1:
	mov eax, 0xa
	ret
.Llir_0_2:
	mov eax, 0x1e
	ret
.Llir_0_3:
	mov eax, 0x32
	ret
.Llir_0_4:
	mov eax, 0x46
	ret
.Llir_0_5:
	mov eax, 0x5a
	ret
.Llir_0_6:
	mov eax, 0x64
	ret
.Llir_0_7:
	xor eax, eax
	ret
	.section .note.GNU-stack
