# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lmix (i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> ()
	.functype	.Lwide (i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lmix,"",@
	.type	.Lmix,@function
.Lmix:
	.functype	.Lmix (i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> ()
	.local	i64
# entry
	local.get	0
	local.get	1
	i64.mul
	local.tee	13
	local.get	2
	local.get	3
	i64.mul
	local.get	4
	local.get	5
	i64.mul
	local.get	13
	local.get	12
	i64.add
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	local.get	5
	local.get	6
	local.get	7
	local.get	8
	local.get	9
	local.get	10
	local.get	11
	local.get	12
	return_call	.Lwide
	end_function

	.section	.text..Lwide,"",@
	.type	.Lwide,@function
.Lwide:
	.functype	.Lwide (i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> ()
# entry
	local.get	0
	local.get	16
	i64.add
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	local.get	5
	local.get	6
	local.get	7
	local.get	8
	local.get	9
	local.get	10
	local.get	11
	local.get	12
	return_call	.Lmix
	end_function

	.no_dead_strip	__indirect_function_table

