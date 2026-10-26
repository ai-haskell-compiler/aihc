# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	forward (i32, i32, i32, i32, i32, i32) -> ()
	.functype	enter_chunk (i32) -> ()
	.functype	.Lreturn (i32, i32, i32, i32, i32, i32) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lreturn,"",@
	.type	.Lreturn,@function
.Lreturn:
	.functype	.Lreturn (i32, i32, i32, i32, i32, i32) -> ()
	.local	i32
# entry
	local.get	4
	i32.load	0
	i32.load	24
	local.tee	6
	i32.const	0
	i32.eq
	if
# slow
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	4
	local.get	5
	return_call	forward
	end_if
	block
# check
	local.get	4
	local.get	3
	i32.eq
	if
	br	1
	end_if
# chunk
	local.get	4
	call	enter_chunk
	br	0
	end_block
# enter
	local.get	6
	i32.eqz
	if
	i32.const	.Llir_trap_0
	i64.const	31
	call	aihc_lir_trap
	unreachable
	end_if
	local.get	0
	local.get	1
	local.get	4
	local.get	3
	local.get	4
	i32.const	0
	local.get	5
	local.get	6
	return_call_indirect	__indirect_function_table, (i32, i32, i32, i32, i32, i32, i32) -> ()
	end_function

	.type	.Llir_trap_0,@object
	.section	.rodata..Llir_trap_0,"",@
.Llir_trap_0:
	.ascii	"indirect call to a non-function"
	.size	.Llir_trap_0, 31

	.no_dead_strip	__indirect_function_table

