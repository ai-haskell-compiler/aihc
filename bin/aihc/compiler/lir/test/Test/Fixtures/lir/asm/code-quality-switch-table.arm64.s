	.text
	.p2align 2
_dense:
	sub x16, x19, #2
	cmp x16, #6
	b.hi .Llir_0_7
	adr x17, .Llir_switch_table_0
	add x17, x17, x16, lsl #2
	br x17
.Llir_switch_table_0:
	b .Llir_0_1
	b .Llir_0_2
	b .Llir_0_3
	b .Llir_0_7
	b .Llir_0_4
	b .Llir_0_5
	b .Llir_0_6
	b .Llir_0_7
.Llir_0_1:
	mov x0, #20
	ret
.Llir_0_2:
	mov x0, #30
	ret
.Llir_0_3:
	mov x0, #40
	ret
.Llir_0_4:
	mov x0, #60
	ret
.Llir_0_5:
	mov x0, #70
	ret
.Llir_0_6:
	mov x0, #80
	ret
.Llir_0_7:
	mov x0, #0
	ret
