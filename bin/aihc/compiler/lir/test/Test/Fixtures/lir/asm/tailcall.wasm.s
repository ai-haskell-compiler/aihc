# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	done (i64) -> ()
	.functype	.Lcount (i64, i64) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lcount,"",@
	.type	.Lcount,@function
.Lcount:
	.functype	.Lcount (i64, i64) -> ()
	.local	i32, i32, i32, i64, f32, f64, i32, i64, i64, i64, i64, i64
# entry
	local.get	0
	i64.const	0
	i64.eq
	local.set	8
	local.get	8
	if
	local.get	1
	local.set	13
# finish
	local.get	13
	return_call	done
	end_if
	local.get	0
	local.get	1
	local.set	10
	local.set	9
# again
	local.get	9
	i64.const	1
	i64.sub
	local.set	11
	local.get	10
	local.get	9
	i64.add
	local.set	12
	local.get	11
	local.get	12
	return_call	.Lcount
	end_function

	.no_dead_strip	__indirect_function_table

