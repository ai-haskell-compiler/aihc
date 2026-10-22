	.text
	.p2align 2
_pick:
	cmp x19, #7
	b.eq .Llir_0_4
	b.cc .Llir_switch_lower_0
	cmp x19, #9
	b.eq .Llir_0_5
	cmn x19, #3
	b.eq .Llir_0_6
	b .Llir_0_7
.Llir_switch_lower_0:
	cmp x19, #1
	b.eq .Llir_0_1
	cmp x19, #3
	b.eq .Llir_0_2
	cmp x19, #5
	b.eq .Llir_0_3
	b .Llir_0_7
.Llir_0_1:
	mov x0, #10
	ret
.Llir_0_2:
	mov x0, #30
	ret
.Llir_0_3:
	mov x0, #50
	ret
.Llir_0_4:
	mov x0, #70
	ret
.Llir_0_5:
	mov x0, #90
	ret
.Llir_0_6:
	mov x0, #100
	ret
.Llir_0_7:
	mov x0, #0
	ret
