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
	.local	i64, i64
# entry
	local.get	0
	call	helper
	local.tee	1
	call	helper
	local.set	2
	local.get	1
	local.get	2
	i64.add
	local.get	0
	i64.add
	return
	end_function

	.no_dead_strip	__indirect_function_table

