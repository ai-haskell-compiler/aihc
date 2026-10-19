	.text
	.p2align 2
_allocate:
	adrp x16, _info@GOTPAGE
	ldr x16, [x16, _info@GOTPAGEOFF]
	str x16, [x20]
	str x23, [x20, #8]
	adrp x16, _info@GOTPAGE
	ldr x16, [x16, _info@GOTPAGEOFF]
	str x16, [x20, #16]
	str x20, [x20, #24]
	str x23, [x20, #32]
	add x0, x20, #16
	add x20, x20, #40
	b _next
