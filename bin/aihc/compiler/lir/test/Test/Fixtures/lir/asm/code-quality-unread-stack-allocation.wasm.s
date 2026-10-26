# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	next (i32, i32, i32, i32, i32, i32) -> ()
	.functype	replay (i32, i32, i32, i32, i32, i32) -> ()
	.functype	.Lcons (i32, i32, i32, i32, i32, i32) -> ()
	.globaltype	__stack_pointer, i32
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lcons,"",@
	.type	.Lcons,@function
.Lcons:
	.functype	.Lcons (i32, i32, i32, i32, i32, i32) -> ()
	.local	i32
# entry
	local.get	0
	i32.const	24
	i32.add
	local.tee	6
	local.get	1
	i32.le_u
	if
# allocate
	local.get	0
	local.get	4
	i32.store	8
	local.get	0
	local.get	5
	i32.store	16
	local.get	6
	local.get	1
	local.get	2
	local.get	3
	local.get	0
	local.get	5
	return_call	next
	end_if
# collect
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	local.get	5
	return_call	replay
	end_function

	.no_dead_strip	__indirect_function_table

