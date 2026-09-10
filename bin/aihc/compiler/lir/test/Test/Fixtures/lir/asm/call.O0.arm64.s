	.text
	.p2align 2
_twice:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #16
	str x19, [sp]
	str x20, [sp, #8]
	mov x19, x0
	mov x0, x19
	bl _helper
	mov x20, x0
	mov x0, x20
	bl _helper
	mov x8, x0
	add x8, x20, x8
	add x8, x8, x19
	mov x0, x8
	ldr x19, [sp]
	ldr x20, [sp, #8]
	mov sp, x29
	ldp x29, x30, [sp], #16
	ret
