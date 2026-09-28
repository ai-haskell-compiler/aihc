	.text
	.p2align 2
_enter:
	add x22, x0, #0
	mov x8, x0
	orr x8, x8, #0xfff
	add x23, x8, #1
	ldr x8, [x0]
	ldr x8, [x8, #24]
	mov x14, x8
	cbz x14, .Llir_trap_0_0
	mov x2, x1
	mov x1, xzr
	br x14
.Llir_trap_0_0:
	b .Llir_trap_0
	.text
	.p2align 2
.Llir_trap_0:
	adrp x0, .Llir_trap_message_0@PAGE
	add x0, x0, .Llir_trap_message_0@PAGEOFF
	mov x1, #32
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
	.byte 0x69, 0x6e, 0x64, 0x69, 0x72, 0x65, 0x63, 0x74, 0x20, 0x63, 0x61, 0x6c, 0x6c, 0x20, 0x74, 0x6f, 0x20, 0x61, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0xa
