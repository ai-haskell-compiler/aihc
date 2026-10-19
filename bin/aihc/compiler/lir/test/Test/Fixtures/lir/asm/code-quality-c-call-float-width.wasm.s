# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lident (f32) -> (f32)
	.functype	.Lmain () -> (f32)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lident,"",@
	.type	.Lident,@function
.Lident:
	.functype	.Lident (f32) -> (f32)
# entry
	local.get	0
	return
	end_function

	.section	.text..Lmain,"",@
	.type	.Lmain,@function
.Lmain:
	.functype	.Lmain () -> (f32)
# entry
	i32.const	1080033280
	f32.reinterpret_i32
	call	.Lident
	return
	end_function

	.no_dead_strip	__indirect_function_table

