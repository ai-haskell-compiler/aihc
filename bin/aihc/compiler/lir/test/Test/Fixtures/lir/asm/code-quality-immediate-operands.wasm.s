# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Limmediates (i64, i64, i32) -> (i64, i64, i64, i64, i64, i32, i32, i32)
	.functype	.Lchunk (i64) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Limmediates,"",@
	.type	.Limmediates,@function
.Limmediates:
	.functype	.Limmediates (i64, i64, i32) -> (i64, i64, i64, i64, i64, i32, i32, i32)
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i64, i64, i64, i32, i32, i32
# entry
	local.get	0
	i64.const	-4
	i64.and
	local.set	9
	local.get	0
	i64.const	65280
	i64.or
	local.set	10
	local.get	0
	i64.const	6148914691236517205
	i64.xor
	local.set	11
	local.get	1
	i64.const	20480
	i64.add
	local.set	12
	local.get	1
	i64.const	-7
	i64.add
	local.set	13
	local.get	2
	i32.const	-16
	i32.and
	local.set	14
	local.get	1
	i64.const	-1
	i64.lt_u
	local.set	15
	local.get	2
	i32.const	-4096
	i32.ge_u
	local.set	16
	local.get	9
	local.get	10
	local.get	11
	local.get	12
	local.get	13
	local.get	14
	local.get	15
	local.get	16
	return
	end_function

	.section	.text..Lchunk,"",@
	.type	.Lchunk,@function
.Lchunk:
	.functype	.Lchunk (i64) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i32
# entry
	local.get	0
	i64.const	4096
	i64.lt_u
	local.set	7
	local.get	7
	if
# grow
	i64.const	1
	return
	end_if
# done
	i64.const	0
	return
	end_function

	.no_dead_strip	__indirect_function_table

