	.text
	.p2align 2
_return:
	mov x24, x0
	ldr x8, [x23]
	ldr x25, [x8, #24]
	cbz x25, .Llir_0_4
.Llir_0_1:
	cmp x23, x22
	b.eq .Llir_0_3
.Llir_0_2:
	stp x29, x30, [sp, #-16]!
	mov x0, x23
	bl _enter_chunk
	ldp x29, x30, [sp], #16
.Llir_0_3:
	mov x14, x25
	mov x21, x23
	mov x0, xzr
	mov x1, x24
	br x14
.Llir_0_4:
	mov x0, x24
	b _forward
