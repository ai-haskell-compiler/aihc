	.text
	.p2align 2
_fields:
	stp w20, w21, [x19]
	ldp w0, w1, [x19]
	ret
	.text
	.p2align 2
_atomic_fields:
	str x20, [x19]
	str x21, [x19, #8]
	ldr x0, [x19]
	ldr x1, [x19, #8]
	ret
	.text
	.p2align 2
_offset:
	ldr x0, [x19, #296]
	ret
	.text
	.p2align 2
_advance:
	str w20, [x19, #16]!
	b _sink
	.text
	.p2align 2
_multiply_add:
	madd w19, w19, w20, w21
	b _sink_i32
	.text
	.p2align 2
_multiply_subtract:
	msub w19, w19, w20, w21
	b _sink_i32
	.text
	.p2align 2
_keep_product:
	mul x1, x19, x20
	add x0, x1, x21
	ret
	.text
	.p2align 2
_pair_limit:
	stp w20, w21, [x19, #252]
	str w20, [x19, #260]
	str w21, [x19, #264]
	ret
	.text
	.p2align 2
_clear:
	str xzr, [x19]
	str wzr, [x19, #8]
	ret
	.text
	.p2align 2
_zero:
	cbz x19, .Llir_9_1
	b .Llir_9_2
.Llir_9_1:
	mov x0, #1
	ret
.Llir_9_2:
	mov x0, #0
	ret
	.text
	.p2align 2
_failure:
.Llir_trap_10_0:
	b .Llir_trap_0
	.text
	.p2align 2
.Llir_trap_0:
	adrp x0, .Llir_trap_message_0@GOTPAGE
	ldr x0, [x0, .Llir_trap_message_0@GOTPAGEOFF]
	mov x1, #8
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
	.byte 0x66, 0x61, 0x69, 0x6c, 0x75, 0x72, 0x65, 0xa
