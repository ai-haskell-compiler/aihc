# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Ldense (i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Ldense,"",@
	.type	.Ldense,@function
.Ldense:
	.functype	.Ldense (i64) -> (i64)
# entry
	local.get	0
	i64.const	2
	i64.eq
	if
# two
	i64.const	20
	return
	end_if
	local.get	0
	i64.const	3
	i64.eq
	if
# three
	i64.const	30
	return
	end_if
	local.get	0
	i64.const	4
	i64.eq
	if
# four
	i64.const	40
	return
	end_if
	local.get	0
	i64.const	6
	i64.eq
	if
# six
	i64.const	60
	return
	end_if
	local.get	0
	i64.const	7
	i64.eq
	if
# seven
	i64.const	70
	return
	end_if
	local.get	0
	i64.const	8
	i64.eq
	if
# eight
	i64.const	80
	return
	end_if
# other
	i64.const	0
	return
	end_function

	.no_dead_strip	__indirect_function_table

