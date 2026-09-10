	.text
	.p2align 2
_count:
	mov x9, x0
	mov x8, x1
	cmp x9, #0
	cset x10, eq
	cbz x10, .Llir_else_0
	b .Llir_0_2
.Llir_else_0:
	mov x15, x8
	mov x8, x9
	mov x9, x15
.Llir_0_1:
	sub x10, x8, #1
	add x8, x9, x8
	mov x0, x10
	mov x1, x8
	b _count
.Llir_0_2:
	mov x0, x8
	b _done
