# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lpick (i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lpick,"",@
	.type	.Lpick,@function
.Lpick:
	.functype	.Lpick (i64) -> (i64)
# entry
	local.get	0
	i64.const	7
	i64.eq
	if
# seven
	i64.const	70
	return
	end_if
	local.get	0
	i64.const	1
	i64.eq
	if
# one
	i64.const	10
	return
	end_if
	local.get	0
	i64.const	-3
	i64.eq
	if
# negative
	i64.const	100
	return
	end_if
	local.get	0
	i64.const	5
	i64.eq
	if
# five
	i64.const	50
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
	i64.const	9
	i64.eq
	if
# nine
	i64.const	90
	return
	end_if
# other
	i64.const	0
	return
	end_function

	.no_dead_strip	__indirect_function_table

