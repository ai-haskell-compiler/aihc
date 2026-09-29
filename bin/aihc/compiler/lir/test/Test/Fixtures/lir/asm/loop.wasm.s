# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lsum (i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lsum,"",@
	.type	.Lsum,@function
.Lsum:
	.functype	.Lsum (i64) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i32, i64, i64, i64, i64, i64
# entry
	local.get	0
	i64.const	0
	local.set	8
	local.set	7
	loop
# loop
	local.get	7
	i64.const	0
	i64.eq
	local.set	9
	local.get	9
	if
	local.get	8
	local.set	14
# exit
	local.get	14
	return
	end_if
	local.get	7
	local.get	8
	local.set	11
	local.set	10
# step
	local.get	10
	i64.const	1
	i64.sub
	local.set	12
	local.get	11
	local.get	10
	i64.add
	local.set	13
	local.get	12
	local.get	13
	local.set	8
	local.set	7
	br	0
	end_loop
	unreachable
	end_function

	.no_dead_strip	__indirect_function_table

