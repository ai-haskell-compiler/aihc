# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	slow (i32) -> (i32)
	.functype	done (i32, i32, i32, i32, i32) -> ()
	.functype	again (i32, i32, i32, i32, i32) -> ()
	.functype	.Lupdate (i32, i32, i32, i32, i32) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lupdate,"",@
	.type	.Lupdate,@function
.Lupdate:
	.functype	.Lupdate (i32, i32, i32, i32, i32) -> ()
	.local	i32
	block
# entry
	local.get	4
	i64.load	0
	i64.const	3
	i64.and
	i64.const	0
	i64.eq
	if
# inl0$fast
	local.get	4
	i64.const	1
	i64.store	8
	br	1
	end_if
# inl0$other
	local.get	4
	call	slow
	local.set	5
# inl0$return
	local.get	5
	if
	br	1
	end_if
# retry
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	return_call	again
	end_block
# finished
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	return_call	done
	end_function

	.no_dead_strip	__indirect_function_table

