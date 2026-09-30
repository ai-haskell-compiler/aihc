# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lmain () -> (i32, i32)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lmain,"",@
	.type	.Lmain,@function
.Lmain:
	.functype	.Lmain () -> (i32, i32)
	.local	i32, i32, i32, i64, f32, f64, i32, i32
# entry
	i32.const	.Lbytes
	i32.load8_u	0
	i32.const	1
	i32.and
	local.set	6
	i32.const	.Lbytes
	i32.load8_u	1
	i32.const	1
	i32.and
	local.set	7
	local.get	6
	local.get	7
	return
	end_function

	.type	.Lbytes,@object
	.section	.rodata..Lbytes,"",@
.Lbytes:
	.int8	2
	.int8	3
	.size	.Lbytes, 2

	.no_dead_strip	__indirect_function_table

