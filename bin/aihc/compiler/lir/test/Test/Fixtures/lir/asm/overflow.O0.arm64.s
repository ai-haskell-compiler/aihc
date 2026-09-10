	.text
	.p2align 2
_spread:
	mov x8, x0
	mov x9, x1
	mov x10, x2
	mov x11, x3
	mov x12, x4
	mov x13, x5
	mov x0, x6
	mov x1, x7
	ldr x2, [sp]
	ldr x3, [sp, #8]
	add x8, x8, x3
	str x2, [sp]
	str x3, [sp, #8]
	mov x2, x10
	mov x3, x11
	mov x4, x12
	mov x5, x13
	mov x6, x0
	mov x7, x1
	mov x0, x8
	mov x1, x9
	b _spread
