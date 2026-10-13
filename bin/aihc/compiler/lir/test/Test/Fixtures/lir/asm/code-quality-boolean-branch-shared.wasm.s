# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lshared (i32) -> (i64)
	.functype	.Lpartial (i32) -> (i64)
	.functype	.Lsigned (i32) -> (i64)
	.functype	.Linteger (i32) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lshared,"",@
	.type	.Lshared,@function
.Lshared:
	.functype	.Lshared (i32) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i64, i64
# entry
	local.get	0
	i64.extend_i32_u
	local.set	7
	local.get	0
	if
# yes
	local.get	7
	return
	end_if
	local.get	7
	local.set	8
# no
	local.get	8
	return
	end_function

	.section	.text..Lpartial,"",@
	.type	.Lpartial,@function
.Lpartial:
	.functype	.Lpartial (i32) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i64
# entry
	local.get	0
	i64.extend_i32_u
	local.set	7
	local.get	7
	i64.const	1
	i64.eq
	if
# yes
	i64.const	11
	return
	end_if
	i32.const	.Llir_trap_0
	i64.const	30
	call	aihc_lir_trap
	unreachable
	end_function

	.section	.text..Lsigned,"",@
	.type	.Lsigned,@function
.Lsigned:
	.functype	.Lsigned (i32) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i64
# entry
	local.get	0
	i32.const	31
	i32.shl
	i32.const	31
	i32.shr_s
	i64.extend_i32_s
	local.set	7
	local.get	7
	i64.const	1
	i64.eq
	if
# yes
	i64.const	11
	return
	end_if
# no
	i64.const	22
	return
	end_function

	.section	.text..Linteger,"",@
	.type	.Linteger,@function
.Linteger:
	.functype	.Linteger (i32) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i64
# entry
	local.get	0
	i64.extend_i32_u
	local.set	7
	local.get	7
	i64.const	1
	i64.eq
	if
# yes
	i64.const	11
	return
	end_if
# no
	i64.const	22
	return
	end_function

	.type	.Llir_trap_0,@object
	.section	.rodata..Llir_trap_0,"",@
.Llir_trap_0:
	.ascii	"switch without a matching case"
	.size	.Llir_trap_0, 30

	.no_dead_strip	__indirect_function_table

