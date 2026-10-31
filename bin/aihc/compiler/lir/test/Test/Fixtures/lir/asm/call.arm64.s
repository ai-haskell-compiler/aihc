	.text
	.p2align 2
_twice:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	str x19, [sp, #-16]!
	str x20, [sp, #8]
	mov x19, x0
	bl _helper
	mov x20, x0
	bl _helper
	add x0, x20, x0
	add x0, x0, x19
	ldp x19, x20, [sp]
	mov sp, x29
	ldp x29, x30, [sp], #16
	ret
