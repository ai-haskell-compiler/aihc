# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lmain (f32, f64) -> (f64, f32)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lmain,"",@
	.type	.Lmain,@function
.Lmain:
	.functype	.Lmain (f32, f64) -> (f64, f32)
	.local	i32, i32, i32, i64, f32, f64, f64, f32
# entry
	local.get	0
	f64.promote_f32
	local.set	8
	local.get	1
	f32.demote_f64
	local.set	9
	local.get	8
	local.get	9
	return
	end_function

	.no_dead_strip	__indirect_function_table

