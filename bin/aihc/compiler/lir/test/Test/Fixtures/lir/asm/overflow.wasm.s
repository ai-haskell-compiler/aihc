# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lspread (i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lspread,"",@
	.type	.Lspread,@function
.Lspread:
	.functype	.Lspread (i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> ()
	.local	i32, i32, i32, i64, f32, f64, i64
# entry
	local.get	0
	local.get	25
	i64.add
	local.set	32
	local.get	32
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
	local.get	13
	local.get	14
	local.get	15
	local.get	16
	local.get	17
	local.get	18
	local.get	19
	local.get	20
	local.get	21
	local.get	22
	local.get	23
	local.get	24
	local.get	25
	return_call	.Lspread
	end_function

	.no_dead_strip	__indirect_function_table

