# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lwide (i64, i64, i64, i64, i64, i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lwide,"",@
	.type	.Lwide,@function
.Lwide:
	.functype	.Lwide (i64, i64, i64, i64, i64, i64) -> (i64)
# entry
	local.get	0
	local.get	1
	i64.add
	local.get	2
	local.get	3
	i64.add
	i64.mul
	local.get	4
	local.get	5
	i64.add
	local.get	0
	i64.mul
	i64.add
	local.get	1
	i64.add
	local.get	2
	i64.add
	return
	end_function

	.no_dead_strip	__indirect_function_table

