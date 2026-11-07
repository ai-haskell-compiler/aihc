	.text
	.p2align 2
_mix:
	mul x8, x19, x20
	mul x9, x21, x22
	mul x10, x23, x0
	add x11, x8, x7
	mov x15, x19
	mov x19, x8
	mov x8, x4
	mov x4, x0
	mov x0, x20
	mov x20, x9
	mov x9, x5
	mov x5, x1
	mov x1, x21
	mov x21, x10
	mov x10, x6
	mov x6, x2
	mov x2, x22
	mov x22, x11
	mov x11, x7
	mov x7, x3
	mov x3, x23
	mov x23, x15
	b _wide
	.text
	.p2align 2
_wide:
	add x19, x19, x11
	b _mix
