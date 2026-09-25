	.text
	.p2align 2
_reserve:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #16
	ldr x8, [x0, #24]
	ldr x9, [x0, #32]
	sub x9, x9, x8
	cmp x9, #16
	b.cs .Llir_0_2
.Llir_0_1:
	mov x19, x2
	stp x0, x1, [sp]
	mov x1, #2
	bl _collect
	mov x2, x19
	ldp x0, x1, [sp]
.Llir_0_2:
	ldr x3, [x0, #24]
	add x8, x3, #16
	str x8, [x0, #24]
	str x2, [x3, #8]
	mov sp, x29
	ldp x29, x30, [sp], #16
	b _next
