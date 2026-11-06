# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lelement (i32, i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lelement,"",@
	.type	.Lelement,@function
.Lelement:
	.functype	.Lelement (i32, i64) -> (i64)
# entry
	local.get	0
	i32.const	16
	i32.add
	local.get	1
	i64.const	3
	i64.shl
	i32.wrap_i64
	i32.add
	i64.load	0
	return
	end_function

	.no_dead_strip	__indirect_function_table

