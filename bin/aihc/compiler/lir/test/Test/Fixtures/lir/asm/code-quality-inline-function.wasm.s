# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	enter_direct (i32, i32, i32) -> ()
	.functype	enter_inline (i32, i32, i32) -> ()
	.tabletype	__indirect_function_table, funcref

	.section	.text.enter_direct,"",@
	.type	enter_direct,@function
	.hidden	enter_direct
	.globl	enter_direct
enter_direct:
	.functype	enter_direct (i32, i32, i32) -> ()
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i32, i32, i64, i32
# entry
	local.get	1
	i64.load	0
	local.set	9
	local.get	9
	i64.const	-4
	i64.and
	local.set	10
	local.get	10
	i32.wrap_i64
	local.set	11
	local.get	11
	i32.load	0
	local.set	12
	local.get	1
	i64.load	8
	local.set	13
	local.get	13
	i32.wrap_i64
	local.set	14
	local.get	12
	i32.eqz
	if
	i32.const	.Llir_trap_0
	i64.const	31
	call	aihc_lir_trap
	unreachable
	end_if
	local.get	0
	local.get	14
	local.get	12
	return_call_indirect	__indirect_function_table, (i32, i32) -> ()
	end_function

	.section	.text.enter_inline,"",@
	.type	enter_inline,@function
	.hidden	enter_inline
	.globl	enter_inline
enter_inline:
	.functype	enter_inline (i32, i32, i32) -> ()
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i32, i32, i64, i32
# entry
	local.get	1
	i64.load	0
	local.set	9
	local.get	9
	i64.const	-4
	i64.and
	local.set	10
	local.get	10
	i32.wrap_i64
	local.set	11
	local.get	11
	i32.load	0
	local.set	12
	local.get	1
	i64.load	8
	local.set	13
	local.get	13
	i32.wrap_i64
	local.set	14
	local.get	12
	i32.eqz
	if
	i32.const	.Llir_trap_0
	i64.const	31
	call	aihc_lir_trap
	unreachable
	end_if
	local.get	0
	local.get	14
	local.get	12
	return_call_indirect	__indirect_function_table, (i32, i32) -> ()
	end_function

	.type	.Llir_trap_0,@object
	.section	.rodata..Llir_trap_0,"",@
.Llir_trap_0:
	.ascii	"indirect call to a non-function"
	.size	.Llir_trap_0, 31

	.no_dead_strip	__indirect_function_table

