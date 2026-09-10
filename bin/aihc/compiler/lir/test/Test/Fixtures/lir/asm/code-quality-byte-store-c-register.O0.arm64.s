	.text
	.p2align 2
_store_byte:
	and x1, x1, #0xff
	mov x8, x0
	mov x9, x1
	mov x10, x2
	strb w9, [x8]
	ldrb w8, [x8]
	mov x0, x8
	ret
	.text
	.p2align 2
_main:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #16
	str xzr, [sp]
	add x8, sp, #0
	mov x0, x8
	mov x1, #37
	mov x2, #0
	bl _store_byte
	and x0, x0, #0xff
	mov x8, x0
	mov x0, x8
	mov sp, x29
	ldp x29, x30, [sp], #16
	ret
