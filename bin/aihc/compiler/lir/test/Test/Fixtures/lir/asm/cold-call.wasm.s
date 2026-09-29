# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	collect (i32, i64) -> ()
	.functype	next (i32, i32, i32, i32, i32, i32, i32, i32) -> ()
	.functype	.Lreserve (i32, i32, i32, i32, i32, i32, i32) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lreserve,"",@
	.type	.Lreserve,@function
.Lreserve:
	.functype	.Lreserve (i32, i32, i32, i32, i32, i32, i32) -> ()
	.local	i32, i32, i32, i64, f32, f64, i32, i32, i32, i32, i32, i32, i32
	block
# entry
	local.get	1
	i64.const	16
	i32.wrap_i64
	i32.add
	local.set	13
	local.get	13
	local.get	2
	i32.le_u
	local.set	14
	local.get	14
	if
	local.get	1
	local.get	2
	local.set	18
	local.set	17
	br	1
	end_if
# collect
	local.get	0
	local.get	1
	i32.store	24
	local.get	0
	i64.const	2
	call	collect
	local.get	0
	i32.load	24
	local.set	15
	local.get	0
	i32.load	32
	local.set	16
	local.get	15
	local.get	16
	local.set	18
	local.set	17
	br	0
	end_block
# reserved
	local.get	17
	i64.const	16
	i32.wrap_i64
	i32.add
	local.set	19
	local.get	17
	local.get	6
	i32.store	8
	local.get	0
	local.get	19
	local.get	18
	local.get	3
	local.get	4
	local.get	5
	local.get	6
	local.get	17
	return_call	next
	end_function

	.no_dead_strip	__indirect_function_table

