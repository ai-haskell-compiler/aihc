# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lone (i64, i64) -> (i64)
	.functype	.Lzero (i32) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lone,"",@
	.type	.Lone,@function
.Lone:
	.functype	.Lone (i64, i64) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i32
# entry
	local.get	0
	local.get	1
	i64.lt_s
	local.set	8
	local.get	8
	if
# yes
	i64.const	11
	return
	end_if
# no
	i64.const	22
	return
	end_function

	.section	.text..Lzero,"",@
	.type	.Lzero,@function
.Lzero:
	.functype	.Lzero (i32) -> (i64)
	.local	i32, i32, i32, i64, f32, f64
# entry
	local.get	0
	if
# yes
	i64.const	11
	return
	end_if
# no
	i64.const	22
	return
	end_function

	.no_dead_strip	__indirect_function_table

