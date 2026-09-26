	.text
	.p2align 4
spread:
	push rbp
	mov rbp, rsp
	sub rsp, 0x70
	mov rax, [rsp + 128]
	mov r10, [rsp + 136]
	mov [rsp], r10
	mov r10, [rsp + 144]
	mov [rsp + 8], r10
	mov r10, [rsp + 152]
	mov [rsp + 16], r10
	mov r10, [rsp + 160]
	mov [rsp + 24], r10
	mov r10, [rsp + 168]
	mov [rsp + 32], r10
	mov r10, [rsp + 176]
	mov [rsp + 40], r10
	mov r10, [rsp + 184]
	mov [rsp + 48], r10
	mov r10, [rsp + 192]
	mov [rsp + 56], r10
	mov r10, [rsp + 200]
	mov [rsp + 64], r10
	mov r10, [rsp + 208]
	mov [rsp + 72], r10
	mov r10, [rsp + 216]
	mov [rsp + 80], r10
	mov r10, [rsp + 224]
	mov [rsp + 88], r10
	mov r10, [rsp + 232]
	mov [rsp + 96], r10
	mov r10, [rsp + 240]
	mov [rsp + 104], r10
	add rbx, r10
	mov r11, [rsp]
	mov [rsp + 136], r11
	mov r11, [rsp + 8]
	mov [rsp + 144], r11
	mov r11, [rsp + 16]
	mov [rsp + 152], r11
	mov r11, [rsp + 24]
	mov [rsp + 160], r11
	mov r11, [rsp + 32]
	mov [rsp + 168], r11
	mov r11, [rsp + 40]
	mov [rsp + 176], r11
	mov r11, [rsp + 48]
	mov [rsp + 184], r11
	mov r11, [rsp + 56]
	mov [rsp + 192], r11
	mov r11, [rsp + 64]
	mov [rsp + 200], r11
	mov r11, [rsp + 72]
	mov [rsp + 208], r11
	mov r11, [rsp + 80]
	mov [rsp + 216], r11
	mov r11, [rsp + 88]
	mov [rsp + 224], r11
	mov r11, [rsp + 96]
	mov [rsp + 232], r11
	mov r11, [rsp + 104]
	mov [rsp + 240], r11
	mov rsp, rbp
	pop rbp
	jmp spread
	.section .note.GNU-stack
