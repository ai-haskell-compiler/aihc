	.text
	.p2align 2
_step:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	mov x24, x0
	add x0, x24, #1
	bl _work
	mul x0, x0, x24
	cmp x0, #100
	b.ge .Llir_0_2
.Llir_0_1:
	mov sp, x29
	ldp x29, x30, [sp], #16
	b _next
.Llir_0_2:
	sub x0, x0, #100
	mov sp, x29
	ldp x29, x30, [sp], #16
	b _next
