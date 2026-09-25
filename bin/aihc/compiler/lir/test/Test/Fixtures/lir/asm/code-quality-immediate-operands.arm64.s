	.text
	.p2align 2
_immediates:
	and x0, x19, #0xfffffffffffffffc
	orr x1, x19, #0xff00
	eor x2, x19, #0x5555555555555555
	add x3, x20, #20480
	sub x4, x20, #7
	and w5, w21, #0xfffffff0
	cmn x20, #1
	cset x6, cc
	cmn w21, #4096
	cset x7, cs
	ret
	.text
	.p2align 2
_chunk:
	cmp x19, #4096
	b.cs .Llir_1_2
.Llir_1_1:
	mov x0, #1
	ret
.Llir_1_2:
	mov x0, #0
	ret
