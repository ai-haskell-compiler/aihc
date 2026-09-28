	.text
	.p2align 2
_reserve:
	ldr x8, [x19]
	mov x17, xzr
	cmp x8, x17
	b.ne .Llir_0_2
.Llir_0_1:
	stp x29, x30, [sp, #-32]!
	add x21, sp, #16
	str x20, [x21]
	mov x0, x19
	mov x1, x21
	bl _collect
	ldr x20, [x21]
	ldp x29, x30, [sp], #32
.Llir_0_2:
	b _next
