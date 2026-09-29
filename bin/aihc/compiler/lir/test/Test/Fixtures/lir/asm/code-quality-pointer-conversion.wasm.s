# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lenter (i32, i32, i32, i32, i32, i32, i32) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lenter,"",@
	.type	.Lenter,@function
.Lenter:
	.functype	.Lenter (i32, i32, i32, i32, i32, i32, i32) -> ()
	.local	i32, i32, i32, i64, f32, f64, i32, i64, i64, i64, i32, i32, i32
# entry
	local.get	5
	i64.const	0
	i32.wrap_i64
	i32.add
	local.set	13
	local.get	5
	i64.extend_i32_u
	local.set	14
	local.get	14
	i64.const	4095
	i64.or
	local.set	15
	local.get	15
	i64.const	1
	i64.add
	local.set	16
	local.get	16
	i32.wrap_i64
	local.set	17
	local.get	5
	i32.load	0
	local.set	18
	local.get	18
	i32.load	24
	local.set	19
	local.get	19
	i32.eqz
	if
	i32.const	.Llir_trap_0
	i64.const	31
	call	aihc_lir_trap
	unreachable
	end_if
	local.get	0
	local.get	1
	local.get	2
	local.get	13
	local.get	17
	local.get	5
	i32.const	0
	local.get	6
	local.get	19
	return_call_indirect	__indirect_function_table, (i32, i32, i32, i32, i32, i32, i32, i32) -> ()
	end_function

	.type	.Llir_trap_0,@object
	.section	.rodata..Llir_trap_0,"",@
.Llir_trap_0:
	.ascii	"indirect call to a non-function"
	.size	.Llir_trap_0, 31

	.no_dead_strip	__indirect_function_table

