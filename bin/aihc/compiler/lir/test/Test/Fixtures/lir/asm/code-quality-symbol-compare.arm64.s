	.text
	.p2align 2
_which:
	adrp x17, _first@GOTPAGE
	ldr x17, [x17, _first@GOTPAGEOFF]
	cmp x19, x17
	b.eq .Llir_0_2
.Llir_0_1:
	adrp x17, _second@GOTPAGE
	ldr x17, [x17, _second@GOTPAGEOFF]
	cmp x19, x17
	b.ne .Llir_0_4
	b .Llir_0_3
.Llir_0_2:
	mov x0, #1
	ret
.Llir_0_3:
	mov x0, #2
	ret
.Llir_0_4:
	mov x0, #0
	ret
	.section __TEXT,__const
	.p2align 3
_first:
	.quad 0x1
	.section __TEXT,__const
	.p2align 3
_second:
	.quad 0x2
