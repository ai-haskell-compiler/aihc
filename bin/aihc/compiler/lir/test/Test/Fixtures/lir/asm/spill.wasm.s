# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lwide (i64, i64, i64, i64, i64, i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lwide,"",@
	.type	.Lwide,@function
.Lwide:
	.functype	.Lwide (i64, i64, i64, i64, i64, i64) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i64, i64, i64, i64, i64, i64
# entry
	local.get	0
	local.get	1
	i64.add
	local.set	12
	local.get	2
	local.get	3
	i64.add
	local.set	13
	local.get	4
	local.get	5
	i64.add
	local.set	14
	local.get	12
	local.get	13
	i64.mul
	local.set	15
	local.get	14
	local.get	0
	i64.mul
	local.set	16
	local.get	15
	local.get	16
	i64.add
	local.set	17
	local.get	17
	local.get	1
	i64.add
	local.set	18
	local.get	18
	local.get	2
	i64.add
	local.set	19
	local.get	19
	return
	end_function

	.no_dead_strip	__indirect_function_table

