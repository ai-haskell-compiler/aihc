	.text
	.p2align 2
_ident:
	fmov x16, s0
	and x16, x16, #0xffffffff
	mov x0, x16
	fmov s0, x0
	ret
	.text
	.p2align 2
_main:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	ldr x16, =0x40600000
	fmov s0, x16
	bl _ident
	fmov x0, s0
	and x0, x0, #0xffffffff
	mov sp, x29
	ldp x29, x30, [sp], #16
	ret
