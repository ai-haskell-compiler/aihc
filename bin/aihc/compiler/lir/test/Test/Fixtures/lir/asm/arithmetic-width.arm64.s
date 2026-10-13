	.text
	.p2align 2
_chain:
	add w19, w19, w20
	ldr x17, =0xffffffff
	mul w19, w19, w17
	sub w19, w19, w20
	mov x0, x19
	ret
	.text
	.p2align 2
_unknown:
	add x19, x19, x20
	and x0, x19, #0xffffffff
	ret
	.text
	.p2align 2
_shared:
	add x0, x19, x20
	and x1, x0, #0xffffffff
	ret
	.text
	.p2align 2
_wide:
	cbz x20, .Llir_trap_3_0
	udiv x20, x19, x20
	and x0, x20, #0xffffffff
	lsr x19, x19, #32
	and x1, x19, #0xffffffff
	ret
.Llir_trap_3_0:
	b .Llir_trap_0
	.text
	.p2align 2
_sigma:
	ror w8, w19, #2
	eor w8, w8, w19, ror #13
	eor w8, w8, w19, ror #22
	mov x0, x8
	ret
	.text
	.p2align 2
.Llir_trap_0:
	adrp x0, .Llir_trap_message_0@GOTPAGE
	ldr x0, [x0, .Llir_trap_message_0@GOTPAGEOFF]
	mov x1, #25
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
	.byte 0x69, 0x6e, 0x74, 0x65, 0x67, 0x65, 0x72, 0x20, 0x64, 0x69, 0x76, 0x69, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x62, 0x79, 0x20, 0x7a, 0x65, 0x72, 0x6f, 0xa
