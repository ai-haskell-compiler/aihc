# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lstore_byte (i32, i32, i64) -> (i32)
	.functype	.Lmain () -> (i32)
	.globaltype	__stack_pointer, i32
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lstore_byte,"",@
	.type	.Lstore_byte,@function
.Lstore_byte:
	.functype	.Lstore_byte (i32, i32, i64) -> (i32)
	.local	i32, i32, i32, i64, f32, f64, i32
# entry
	local.get	0
	local.get	1
	i32.store8	0
	local.get	0
	i32.load8_u	0
	local.set	9
	local.get	9
	return
	end_function

	.section	.text..Lmain,"",@
	.type	.Lmain,@function
.Lmain:
	.functype	.Lmain () -> (i32)
	.local	i32, i32, i32, i64, f32, f64, i32, i32
# entry
	global.get	__stack_pointer
	i32.const	16
	i32.sub
	local.tee	1
	global.set	__stack_pointer
	local.get	1
	local.set	6
	local.get	6
	i32.const	37
	i64.const	0
	call	.Lstore_byte
	local.set	7
	local.get	1
	i32.const	16
	i32.add
	global.set	__stack_pointer
	local.get	7
	return
	end_function

	.no_dead_strip	__indirect_function_table

