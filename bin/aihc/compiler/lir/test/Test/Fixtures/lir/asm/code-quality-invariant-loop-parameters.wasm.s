# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lnested (i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lnested,"",@
	.type	.Lnested,@function
.Lnested:
	.functype	.Lnested (i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> (i64)
	.local	i64, i64
# entry
	i64.const	0
	i64.const	0
	local.set	15
	local.set	16
	loop
# outer
	local.get	16
	local.get	0
	i64.ge_s
	if
# finish
	local.get	15
	return
	end_if
# step
	local.get	15
	local.get	1
	i64.add
	local.set	15
# inner
	local.get	16
	i64.const	1
	i64.add
	local.get	15
	local.get	2
	i64.mul
	local.get	3
	i64.mul
	local.get	4
	i64.mul
	local.get	5
	i64.mul
	local.get	6
	i64.mul
	local.get	7
	i64.mul
	local.get	8
	i64.mul
	local.get	9
	i64.mul
	local.get	10
	i64.mul
	local.get	11
	i64.mul
	local.get	12
	i64.mul
	local.get	13
	i64.mul
	local.get	14
	i64.mul
	local.set	15
	local.set	16
	br	0
	end_loop
	unreachable
	end_function

	.no_dead_strip	__indirect_function_table

