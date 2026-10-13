	.text
	.p2align 2
_constant:
	ror w8, w19, #7
	ror w1, w8, #23
	mov x0, x19
	ret
	.text
	.p2align 2
_variable:
	ror x0, x19, x20
	sub x17, xzr, x20
	ror x1, x0, x17
	ret
	.text
	.p2align 2
_pattern:
	ror w0, w19, #7
	ret
	.text
	.p2align 2
_sigma:
	ror w8, w19, #7
	eor w0, w8, w19, ror #18
	ret
	.text
	.p2align 2
_narrow:
	mov x16, x19
	lsl x15, x16, #8
	orr x16, x16, x15
	lsl x15, x16, #16
	orr x16, x16, x15
	sub x17, xzr, x20
	ror w0, w16, w17
	and x0, x0, #0xff
	mov x16, x21
	lsl x15, x16, #16
	orr x16, x16, x15
	ror w1, w16, w22
	and x1, x1, #0xffff
	ret
	.text
	.p2align 2
_wide_pattern:
	ror w19, w19, #7
	mov x0, x19
	ret
	.text
	.p2align 2
_constant64:
	ror x1, x19, #23
	mov x0, x19
	ret
