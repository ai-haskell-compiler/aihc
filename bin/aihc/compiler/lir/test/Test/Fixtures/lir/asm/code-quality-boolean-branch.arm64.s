	.text
	.p2align 2
_one:
	cmp x19, x20
	b.ge .Llir_0_2
.Llir_0_1:
	mov x0, #11
	ret
.Llir_0_2:
	mov x0, #22
	ret
	.text
	.p2align 2
_zero:
	cbz x19, .Llir_1_2
.Llir_1_1:
	mov x0, #11
	ret
.Llir_1_2:
	mov x0, #22
	ret
