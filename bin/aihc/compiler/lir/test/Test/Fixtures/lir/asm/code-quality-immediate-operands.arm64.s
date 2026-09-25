	.text
	.p2align 2
_immediates:
	and x8, x0, #0xfffffffffffffffc
	orr x9, x0, #0xff00
	eor x0, x0, #0x5555555555555555
	add x3, x1, #20480
	sub x4, x1, #7
	and w5, w2, #0xfffffff0
	cmn x1, #1
	cset x6, cc
	cmn w2, #4096
	cset x7, cs
	mov x1, x9
	mov x2, x0
	mov x0, x8
	ret
	.text
	.p2align 2
_chunk:
	cmp x0, #4096
	b.cs .Llir_1_2
.Llir_1_1:
	mov x0, #1
	ret
.Llir_1_2:
	mov x0, #0
	ret
