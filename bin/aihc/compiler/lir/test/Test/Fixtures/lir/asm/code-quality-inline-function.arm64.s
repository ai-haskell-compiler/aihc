	.text
	.p2align 2
	.globl _enter_direct
_enter_direct:
	ldr x8, [x1]
	and x8, x8, #0xfffffffffffffffc
	ldr x8, [x8]
	ldr x1, [x1, #8]
	mov x14, x8
	cbz x14, .Llir_trap_0_0
	br x14
.Llir_trap_0_0:
	b .Llir_trap_0
	.text
	.p2align 2
	.globl _enter_inline
_enter_inline:
	ldr x8, [x1]
	and x8, x8, #0xfffffffffffffffc
	ldr x8, [x8]
	ldr x1, [x1, #8]
	mov x14, x8
	cbz x14, .Llir_trap_1_0
	br x14
.Llir_trap_1_0:
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
