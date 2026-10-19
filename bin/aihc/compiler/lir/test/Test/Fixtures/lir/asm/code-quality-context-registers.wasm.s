# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	work (i64) -> (i64)
	.functype	next (i32, i32, i32, i32, i64) -> ()
	.functype	.Lstep (i32, i32, i32, i32, i64) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lstep,"",@
	.type	.Lstep,@function
.Lstep:
	.functype	.Lstep (i32, i32, i32, i32, i64) -> ()
# entry
	local.get	4
	i64.const	1
	i64.add
	call	work
	local.get	4
	i64.mul
	local.tee	4
	i64.const	100
	i64.lt_s
	if
# small
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	return_call	next
	end_if
# large
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	i64.const	100
	i64.sub
	return_call	next
	end_function

	.no_dead_strip	__indirect_function_table

