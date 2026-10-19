# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lwhich (i32) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lwhich,"",@
	.type	.Lwhich,@function
.Lwhich:
	.functype	.Lwhich (i32) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i32, i32
# entry
	local.get	0
	i32.const	.Lfirst
	i32.eq
	local.set	7
	local.get	7
	if
# one
	i64.const	1
	return
	end_if
# other
	local.get	0
	i32.const	.Lsecond
	i32.eq
	local.set	8
	local.get	8
	if
# two
	i64.const	2
	return
	end_if
# none
	i64.const	0
	return
	end_function

	.type	.Lfirst,@object
	.section	.rodata..Lfirst,"",@
	.p2align	3, 0x0
.Lfirst:
	.int64	1
	.size	.Lfirst, 8

	.type	.Lsecond,@object
	.section	.rodata..Lsecond,"",@
	.p2align	3, 0x0
.Lsecond:
	.int64	2
	.size	.Lsecond, 8

	.no_dead_strip	__indirect_function_table

