	.text
	.p2align 2
_spread:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #16
	ldr x16, [sp, #32]
	str x16, [sp]
	ldr x16, [sp, #40]
	str x16, [sp, #8]
	ldr x17, [sp, #8]
	add x19, x19, x17
	ldr x16, [sp]
	str x16, [sp, #32]
	ldr x16, [sp, #8]
	str x16, [sp, #40]
	mov sp, x29
	ldp x29, x30, [sp], #16
	b _spread
