	.text
	.p2align 2
_update:
	ldr x8, [x23]
	and x8, x8, #0x3
	cbnz x8, .Llir_0_2
.Llir_0_1:
	mov x16, #1
	str x16, [x23, #8]
	b .Llir_0_4
.Llir_0_2:
	stp x29, x30, [sp, #-16]!
	mov x0, x23
	bl _slow
	and x0, x0, #0x1
	ldp x29, x30, [sp], #16
.Llir_0_3:
	cbz x0, .Llir_0_5
.Llir_0_4:
	b _done
.Llir_0_5:
	b _again
