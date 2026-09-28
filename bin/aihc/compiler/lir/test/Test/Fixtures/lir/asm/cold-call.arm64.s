	.text
	.p2align 2
_reserve:
	add x8, x20, #16
	cmp x8, x21
	b.hi .Llir_0_1
	mov x2, x20
	b .Llir_0_2
.Llir_0_1:
	stp x29, x30, [sp, #-32]!
	str x20, [x19, #24]
	stp x1, x0, [sp, #16]
	mov x0, x19
	mov x1, #2
	bl _collect
	ldp x1, x0, [sp, #16]
	ldr x2, [x19, #24]
	ldr x21, [x19, #32]
	ldp x29, x30, [sp], #32
.Llir_0_2:
	add x20, x2, #16
	str x1, [x2, #8]
	b _next
