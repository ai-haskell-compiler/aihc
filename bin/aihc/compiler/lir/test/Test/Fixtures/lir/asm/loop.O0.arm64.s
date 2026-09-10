	.text
	.p2align 2
_sum:
	mov x8, x0
	mov x9, x8
	mov x8, #0
.Llir_0_1:
	cmp x9, #0
	cset x10, eq
	cbz x10, .Llir_else_0
	b .Llir_0_3
.Llir_else_0:
	mov x15, x8
	mov x8, x9
	mov x9, x15
.Llir_0_2:
	sub x10, x8, #1
	add x8, x9, x8
	mov x9, x10
	b .Llir_0_1
.Llir_0_3:
	mov x0, x8
	ret
