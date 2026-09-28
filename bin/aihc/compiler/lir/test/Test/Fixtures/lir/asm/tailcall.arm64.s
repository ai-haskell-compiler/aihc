	.text
	.p2align 2
_count:
	cbnz x19, .Llir_0_1
	mov x19, x20
	b .Llir_0_2
.Llir_0_1:
	sub x8, x19, #1
	add x20, x20, x19
	mov x19, x8
	b _count
.Llir_0_2:
	b _done
