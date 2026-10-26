	.text
	.p2align 2
_cons:
	add x8, x19, #24
	cmp x8, x20
	b.hi .Llir_0_2
.Llir_0_1:
	str x23, [x19, #8]
	str x0, [x19, #16]
	mov x23, x19
	mov x19, x8
	b _next
.Llir_0_2:
	b _replay
