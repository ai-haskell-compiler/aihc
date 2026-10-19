# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	next (i32, i32, i32, i32, i32, i32) -> ()
	.functype	.Lallocate (i32, i32, i32, i32, i32) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lallocate,"",@
	.type	.Lallocate,@function
.Lallocate:
	.functype	.Lallocate (i32, i32, i32, i32, i32) -> ()
	.local	i32, i32, i32, i64, f32, f64, i32, i32
# entry
	local.get	1
	i32.const	info
	i32.store	0
	local.get	1
	local.get	4
	i32.store	8
	local.get	1
	i32.const	info
	i32.store	16
	local.get	1
	local.get	1
	i32.store	24
	local.get	1
	local.get	4
	i32.store	32
	local.get	1
	i64.const	16
	i32.wrap_i64
	i32.add
	local.set	11
	local.get	1
	i64.const	40
	i32.wrap_i64
	i32.add
	local.set	12
	local.get	0
	local.get	12
	local.get	2
	local.get	3
	local.get	4
	local.get	11
	return_call	next
	end_function

	.no_dead_strip	__indirect_function_table

