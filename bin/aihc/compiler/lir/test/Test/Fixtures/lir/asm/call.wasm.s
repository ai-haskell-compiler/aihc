# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	helper (i64) -> (i64)
	.functype	.Ltwice (i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Ltwice,"",@
	.type	.Ltwice,@function
.Ltwice:
	.functype	.Ltwice (i64) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i64, i64
# entry
	local.get	0
	call	helper
	local.set	7
	local.get	7
	call	helper
	local.set	8
	local.get	7
	local.get	8
	i64.add
	local.set	9
	local.get	9
	local.get	0
	i64.add
	local.set	10
	local.get	10
	return
	end_function

	.no_dead_strip	__indirect_function_table

