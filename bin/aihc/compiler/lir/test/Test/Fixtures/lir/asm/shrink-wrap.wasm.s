# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	collect (i32, i32) -> ()
	.functype	next (i32, i32) -> ()
	.functype	.Lreserve (i32, i32) -> ()
	.globaltype	__stack_pointer, i32
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lreserve,"",@
	.type	.Lreserve,@function
.Lreserve:
	.functype	.Lreserve (i32, i32) -> ()
	.local	i32, i32, i32, i64, f32, f64, i32, i32, i32, i32, i32
	global.get	__stack_pointer
	i32.const	16
	i32.sub
	local.tee	3
	global.set	__stack_pointer
	block
# entry
	local.get	3
	i32.const	0
	i32.add
	local.set	8
	local.get	0
	i32.load	0
	local.set	9
	local.get	9
	i32.const	0
	i32.eq
	local.set	10
	local.get	10
	if
# collect
	local.get	8
	local.get	1
	i32.store	0
	local.get	0
	local.get	8
	call	collect
	local.get	8
	i32.load	0
	local.set	11
	local.get	11
	local.set	12
	br	1
	end_if
	local.get	1
	local.set	12
	br	0
	end_block
# ready
	local.get	3
	i32.const	16
	i32.add
	global.set	__stack_pointer
	local.get	0
	local.get	12
	return_call	next
	end_function

	.no_dead_strip	__indirect_function_table

