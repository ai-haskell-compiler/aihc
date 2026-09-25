	.text
	.p2align 2
_store_byte:
	and x1, x1, #0xff
	strb w1, [x0]
	ldrb w0, [x0]
	ret
	.text
	.p2align 2
_main:
	stp x29, x30, [sp, #-32]!
	add x0, sp, #16
	mov x1, #37
	mov x2, #0
	bl _store_byte
	and x0, x0, #0xff
	ldp x29, x30, [sp], #32
	ret
