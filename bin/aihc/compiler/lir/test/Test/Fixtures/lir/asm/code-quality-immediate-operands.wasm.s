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
# entry
	local.get	0
	i64.const	-4
	i64.and
	local.get	0
	i64.const	65280
	i64.or
	local.get	0
	i64.const	6148914691236517205
	i64.xor
	local.get	1
	i64.const	20480
	i64.add
	local.get	1
	i64.const	-7
	i64.add
	local.get	2
	i32.const	-16
	i32.and
	local.get	1
	i64.const	-1
	i64.lt_u
	local.get	2
	i32.const	-4096
	i32.ge_u
	return
	end_function

	.section	.text..Lchunk,"",@
	.type	.Lchunk,@function
.Lchunk:
	.functype	.Lchunk (i64) -> (i64)
# entry
	local.get	0
	i64.const	4096
	i64.lt_u
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

