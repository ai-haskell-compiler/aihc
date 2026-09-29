	.text
	.p2align 2
_step:
	stp x29, x30, [sp, #-16]!
	add x0, x23, #1
	bl _work
	mul x23, x0, x23
	ldp x29, x30, [sp], #16
	cmp x23, #100
	b.ge .Llir_0_2
.Llir_0_1:
	b _next
.Llir_0_2:
	sub x23, x23, #100
	b _next
