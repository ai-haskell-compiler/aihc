# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lmain () -> (i32, i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lmain,"",@
	.type	.Lmain,@function
.Lmain:
	.functype	.Lmain () -> (i32, i64)
	.local	i32, i32, i32, i64, f32, f64, i32, i64
# entry
	i32.const	255
	i32.eqz
	if
	i32.const	.Llir_trap_0
	i64.const	24
	call	aihc_lir_trap
	unreachable
	end_if
	i32.const	128
	i32.extend8_s
	i32.const	255
	i32.extend8_s
	i32.rem_s
	i32.const	255
	i32.and
	local.set	6
	i64.const	-1
	i64.eqz
	if
	i32.const	.Llir_trap_0
	i64.const	24
	call	aihc_lir_trap
	unreachable
	end_if
	i64.const	-9223372036854775808
	i64.const	-1
	i64.rem_s
	local.set	7
	local.get	6
	local.get	7
	return
	end_function

	.type	.Llir_trap_0,@object
	.section	.rodata..Llir_trap_0,"",@
.Llir_trap_0:
	.ascii	"integer division by zero"
	.size	.Llir_trap_0, 24

	.no_dead_strip	__indirect_function_table

