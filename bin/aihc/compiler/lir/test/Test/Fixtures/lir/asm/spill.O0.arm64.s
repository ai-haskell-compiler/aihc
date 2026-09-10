	.text
	.p2align 2
_wide:
	mov x8, x0
	mov x9, x1
	mov x10, x2
	mov x11, x3
	mov x12, x4
	mov x13, x5
	add x0, x8, x9
	add x11, x10, x11
	add x12, x12, x13
	mul x11, x0, x11
	mul x8, x12, x8
	add x8, x11, x8
	add x8, x8, x9
	add x8, x8, x10
	mov x0, x8
	ret
