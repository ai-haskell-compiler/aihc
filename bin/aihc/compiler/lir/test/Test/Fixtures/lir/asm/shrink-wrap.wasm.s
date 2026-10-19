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
	.local	i32, i32
	block
# entry
	local.get	0
	i32.load	0
	i32.const	0
	i32.eq
	if
# collect
	global.get	__stack_pointer
	i32.const	16
	i32.sub
	local.tee	3
	global.set	__stack_pointer
	local.get	3
	local.tee	2
	local.get	1
	i32.store	0
	local.get	0
	local.get	2
	call	collect
	local.get	3
	i32.const	16
	i32.add
	global.set	__stack_pointer
	local.get	2
	i32.load	0
	local.set	1
	br	1
	end_if
	br	0
	end_block
# ready
	local.get	0
	local.get	1
	return_call	next
	end_function

	.no_dead_strip	__indirect_function_table

