	.text
	.p2align 2
_main:
	mov x8, x0
	mov x9, x1
	fmov s16, w8
	fcvt d16, s16
	fmov x8, d16
	fmov d16, x9
	fcvt s16, d16
	fmov w9, s16
	mov x0, x8
	mov x1, x9
	ret
