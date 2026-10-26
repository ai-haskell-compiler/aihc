	.text
	.p2align 2
_pick:
	cbnz x23, .Llir_0_2
.Llir_0_1:
	stp x29, x30, [sp, #-16]!
	bl _report
	ldp x29, x30, [sp], #16
	b .Llir_trap_0_0
.Llir_0_2:
	add x23, x0, x23
	b _next
.Llir_trap_0_0:
	b .Llir_trap_0
	.text
	.p2align 2
.Llir_trap_0:
	adrp x0, .Llir_trap_message_0@GOTPAGE
	ldr x0, [x0, .Llir_trap_message_0@GOTPAGEOFF]
	mov x1, #29
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
	.byte 0x6e, 0x6f, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x69, 0x6e, 0x67, 0x20, 0x63, 0x61, 0x73, 0x65, 0x20, 0x61, 0x6c, 0x74, 0x65, 0x72, 0x6e, 0x61, 0x74, 0x69, 0x76, 0x65, 0xa
