	.text
	.p2align 2
_main:
	adrp x17, _bytes@PAGE
	add x17, x17, _bytes@PAGEOFF
	ldrb w8, [x17]
	and x8, x8, #0x1
	adrp x17, _bytes@PAGE
	add x17, x17, _bytes@PAGEOFF
	ldrb w9, [x17, #1]
	and x9, x9, #0x1
	mov x0, x8
	mov x1, x9
	ret
	.section __TEXT,__const
	.p2align 0
_bytes:
	.byte 0x2
	.byte 0x3
