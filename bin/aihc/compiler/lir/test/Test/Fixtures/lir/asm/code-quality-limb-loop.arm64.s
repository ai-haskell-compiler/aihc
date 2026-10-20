	.text
	.p2align 2
_limb:
	ldr x19, [x19, #16]
	ldr x19, [x19, x23, lsl #3]
	umulh x15, x19, x21
	mul x21, x19, x21
	mov x19, x15
	add x23, x23, x22
	ldr x20, [x20, #16]
	add x20, x20, x23, lsl #3
	ldr x8, [x20]
	adds x8, x21, x8
	cinc x19, x19, cs
	adds x0, x8, x0
	cinc x1, x19, cs
	str x0, [x20]
	ret
