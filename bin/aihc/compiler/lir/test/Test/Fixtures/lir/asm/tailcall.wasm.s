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
# entry
	local.get	0
	i64.const	0
	i64.eq
	if
# finish
	local.get	1
	return_call	done
	end_if
# again
	local.get	0
	i64.const	1
	i64.sub
	local.get	1
	local.get	0
	i64.add
	return_call	.Lcount
	end_function

	.no_dead_strip	__indirect_function_table

