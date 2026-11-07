	.text
	.p2align 2
_nested:
	mov x11, #0
	mov x10, #0
.Llir_0_1:
	cmp x11, x19
	b.ge .Llir_0_4
.Llir_0_2:
	add x12, x10, x20
.Llir_0_3:
	mul x12, x12, x21
	mul x12, x12, x22
	mul x12, x12, x23
	mul x12, x12, x0
	mul x12, x12, x1
	mul x12, x12, x2
	mul x12, x12, x3
	mul x12, x12, x4
	mul x12, x12, x5
	mul x12, x12, x6
	mul x12, x12, x7
	mul x12, x12, x8
	mul x12, x12, x9
	add x11, x11, #1
	mov x10, x12
	b .Llir_0_1
.Llir_0_4:
	mov x0, x10
	ret
