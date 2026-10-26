# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	report () -> ()
	.functype	next (i32, i32, i32, i32, i64, i64) -> ()
	.functype	.Lpick (i32, i32, i32, i32, i64, i64) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lpick,"",@
	.type	.Lpick,@function
.Lpick:
	.functype	.Lpick (i32, i32, i32, i32, i64, i64) -> ()
# entry
	local.get	4
	i64.const	0
	i64.eq
	if
# missing
	call	report
	i32.const	.Llir_trap_0
	i64.const	28
	call	aihc_lir_trap
	unreachable
	end_if
# found
	local.get	0
	local.get	1
	local.get	2
	local.get	3
	local.get	5
	local.get	4
	i64.add
	local.get	5
	return_call	next
	end_function

	.type	.Llir_trap_0,@object
	.section	.rodata..Llir_trap_0,"",@
.Llir_trap_0:
	.ascii	"no matching case alternative"
	.size	.Llir_trap_0, 28

	.no_dead_strip	__indirect_function_table

