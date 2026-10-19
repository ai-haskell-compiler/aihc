# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lsum (i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lsum,"",@
	.type	.Lsum,@function
.Lsum:
	.functype	.Lsum (i64) -> (i64)
	.local	i64
# entry
	i64.const	0
	local.set	1
	loop
# loop
	local.get	0
	i64.const	0
	i64.eq
	if
# exit
	local.get	1
	return
	end_if
# step
	local.get	0
	i64.const	1
	i64.sub
	local.get	1
	local.get	0
	i64.add
	local.set	1
	local.set	0
	br	0
	end_loop
	unreachable
	end_function

	.no_dead_strip	__indirect_function_table

