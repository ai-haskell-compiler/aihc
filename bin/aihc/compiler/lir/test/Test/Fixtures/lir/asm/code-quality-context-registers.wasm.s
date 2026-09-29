# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	work (i64) -> (i64)
	.functype	next (i32, i32, i32, i32, i32, i64) -> ()
	.functype	.Lstep (i32, i32, i32, i32, i32, i64) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lstep,"",@
	.type	.Lstep,@function
.Lstep:
	.functype	.Lstep (i32, i32, i32, i32, i32, i64) -> ()
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i64, i32, i64
# entry
	local.get	5
	i64.const	1
	i64.add
	local.set	12
	local.get	12
	call	work
	local.set	13
	local.get	13
	local.get	5
	i64.mul
	local.set	14
	local.get	14
	i64.const	100
	i64.lt_s
	local.set	15
	local.get	15
	if
# small
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	local.get	14
	return_call	next
	end_if
# large
	local.get	14
	i64.const	100
	i64.sub
	local.set	16
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	local.get	16
	return_call	next
	end_function

	.no_dead_strip	__indirect_function_table

