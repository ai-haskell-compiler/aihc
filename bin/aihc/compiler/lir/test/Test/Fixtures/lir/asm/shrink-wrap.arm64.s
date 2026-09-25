	.text
	.p2align 2
_reserve:
	mov x19, x0
	ldr x8, [x19]
	mov x17, xzr
	cmp x8, x17
	b.ne .Llir_0_2
.Llir_0_1:
	stp x29, x30, [sp, #-32]!
	add x20, sp, #16
	str x1, [x20]
	mov x0, x19
	mov x1, x20
	bl _collect
	ldr x1, [x20]
	ldp x29, x30, [sp], #32
.Llir_0_2:
	mov x0, x19
	b _next
