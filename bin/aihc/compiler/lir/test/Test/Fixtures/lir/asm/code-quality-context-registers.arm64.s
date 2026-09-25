	.text
	.p2align 2
_step:
	mov x24, x0
	stp x29, x30, [sp, #-16]!
	add x0, x24, #1
	bl _work
	mul x0, x0, x24
	ldp x29, x30, [sp], #16
	cmp x0, #100
	b.ge .Llir_0_2
.Llir_0_1:
	b _next
.Llir_0_2:
	sub x0, x0, #100
	b _next
