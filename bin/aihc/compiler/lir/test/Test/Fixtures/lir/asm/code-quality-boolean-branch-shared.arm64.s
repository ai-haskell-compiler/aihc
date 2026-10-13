	.text
	.p2align 2
_shared:
	mov x0, x19
	cbz x19, .Llir_else_0
	b .Llir_0_2
.Llir_else_0:
	mov x8, x0
.Llir_0_1:
	mov x0, x8
	ret
.Llir_0_2:
	ret
	.text
	.p2align 2
_partial:
	cmp x19, #1
	b.eq .Llir_1_1
	b .Llir_trap_1_0
.Llir_1_1:
	mov x0, #11
	ret
.Llir_trap_1_0:
	b .Llir_trap_0
	.text
	.p2align 2
_signed:
	sub x19, xzr, x19
	cmp x19, #1
	b.eq .Llir_2_1
	b .Llir_2_2
.Llir_2_1:
	mov x0, #11
	ret
.Llir_2_2:
	mov x0, #22
	ret
	.text
	.p2align 2
_integer:
	cmp x19, #1
	b.eq .Llir_3_1
	b .Llir_3_2
.Llir_3_1:
	mov x0, #11
	ret
.Llir_3_2:
	mov x0, #22
	ret
	.text
	.p2align 2
.Llir_trap_0:
	adrp x0, .Llir_trap_message_0@GOTPAGE
	ldr x0, [x0, .Llir_trap_message_0@GOTPAGEOFF]
	mov x1, #31
	b .Llir_trap
	.p2align 2
.Llir_trap:
	mov x2, x1
	mov x1, x0
	mov x0, #2
	bl _write
	mov x0, #1
	bl __exit
	brk #0
	.section __TEXT,__const
.Llir_trap_message_0:
	.byte 0x73, 0x77, 0x69, 0x74, 0x63, 0x68, 0x20, 0x77, 0x69, 0x74, 0x68, 0x6f, 0x75, 0x74, 0x20, 0x61, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x69, 0x6e, 0x67, 0x20, 0x63, 0x61, 0x73, 0x65, 0xa
