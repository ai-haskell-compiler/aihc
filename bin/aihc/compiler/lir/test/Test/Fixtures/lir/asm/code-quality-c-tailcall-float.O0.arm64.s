	.text
	.p2align 2
_double:
	mov x8, x0
	fmov x16, d0
	mov x9, x16
	cmp x8, #42
	cset x8, eq
	mov x17, #0
	cmp x8, #0
	csel x8, x9, x17, ne
	mov x0, x8
	fmov d0, x0
	ret
	.text
	.p2align 2
_single:
	mov x8, x0
	fmov x16, s0
	and x16, x16, #0xffffffff
	mov x9, x16
	cmp x8, #19
	cset x8, eq
	mov x17, #0
	cmp x8, #0
	csel x8, x9, x17, ne
	mov x0, x8
	fmov s0, x0
	ret
	.text
	.p2align 2
_forward:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	mov x8, x0
	fmov x16, d0
	mov x9, x16
	fmov d0, x9
	mov x0, x8
	bl _double
	fmov x0, d0
	mov x8, x0
	ldr x17, =0x4014000000000000
	fmov d16, x8
	fmov d17, x17
	fadd d16, d16, d17
	fmov x8, d16
	fmov d0, x8
	mov x0, #42
	mov sp, x29
	ldp x29, x30, [sp], #16
	b _double
	.text
	.p2align 2
_indirect:
	mov x9, x0
	mov x8, x1
	fmov x16, s0
	and x16, x16, #0xffffffff
	mov x10, x16
	mov x14, x9
	cbz x14, .Llir_trap_0
	ldr x16, =0x40600000
	fmov s0, x16
	mov x0, #19
	br x14
	.text
	.p2align 2
_main:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	ldr x16, =0x4000000000000000
	fmov d0, x16
	mov x0, #42
	bl _forward
	fmov x0, d0
	mov x19, x0
	adrp x17, _table@PAGE
	add x17, x17, _table@PAGEOFF
	ldr x8, [x17]
	ldr x16, =0x3f800000
	fmov s0, x16
	mov x0, x8
	mov x1, #8
	bl _indirect
	fmov x0, s0
	and x0, x0, #0xffffffff
	mov x8, x0
	mov x0, x19
	mov x1, x8
	mov sp, x29
	ldp x29, x30, [sp], #16
	ret
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
	.section __TEXT,__const
	.p2align 3
_table:
	.quad _single
