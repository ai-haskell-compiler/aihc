	.text
	.p2align 2
_main:
	fmov s16, w19
	fcvt d16, s16
	fmov x0, d16
	fmov d16, x20
	fcvt s16, d16
	fmov w1, s16
	ret
