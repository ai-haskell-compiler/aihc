# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Llimb (i32, i32, i64, i64, i64, i64) -> (i64, i64)
	.functype	.Laihc_lir_wasm_mul_wide_u64 (i64, i64) -> (i64, i64)
	.functype	.Laihc_lir_wasm_mul_wide_s64 (i64, i64) -> (i64, i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Llimb,"",@
	.type	.Llimb,@function
.Llimb:
	.functype	.Llimb (i32, i32, i64, i64, i64, i64) -> (i64, i64)
	.local	i32, i64, i64
# entry
	local.get	0
	i32.load	16
	local.get	4
	i64.const	3
	i64.shl
	i32.wrap_i64
	i32.add
	i64.load	0
	local.get	2
	call	.Laihc_lir_wasm_mul_wide_u64
	local.set	8
	local.set	7
	local.get	1
	i32.load	16
	local.get	4
	local.get	3
	i64.add
	i64.const	3
	i64.shl
	i32.wrap_i64
	i32.add
	local.set	0
	local.get	7
	local.get	0
	i64.load	0
	i64.add
	local.tee	2
	local.get	7
	i64.lt_u
	local.set	1
	local.get	2
	local.get	5
	i64.add
	local.tee	3
	local.get	2
	i64.lt_u
	local.set	6
	local.get	0
	local.get	3
	i64.store	0
	local.get	3
	local.get	8
	local.get	1
	i64.extend_i32_u
	i64.add
	local.get	6
	i64.extend_i32_u
	i64.add
	return
	end_function

	.section	.text..Laihc_lir_wasm_mul_wide_u64,"",@
	.type	.Laihc_lir_wasm_mul_wide_u64,@function
.Laihc_lir_wasm_mul_wide_u64:
	.functype	.Laihc_lir_wasm_mul_wide_u64 (i64, i64) -> (i64, i64)
	.local	i64, i64, i64, i64
# entry
	local.get	0
	i64.const	4294967295
	i64.and
	local.set	2
	local.get	0
	i64.const	32
	i64.shr_u
	local.set	0
	local.get	1
	i64.const	4294967295
	i64.and
	local.set	3
	local.get	1
	i64.const	32
	i64.shr_u
	local.set	1
	local.get	2
	local.get	3
	i64.mul
	local.set	4
	local.get	2
	local.get	1
	i64.mul
	local.set	2
	local.get	0
	local.get	3
	i64.mul
	local.set	3
	local.get	4
	i64.const	32
	i64.shr_u
	local.get	2
	i64.const	4294967295
	i64.and
	i64.add
	local.get	3
	i64.const	4294967295
	i64.and
	i64.add
	local.set	5
	local.get	4
	i64.const	4294967295
	i64.and
	local.get	5
	i64.const	32
	i64.shl
	i64.or
	local.get	0
	local.get	1
	i64.mul
	local.get	2
	i64.const	32
	i64.shr_u
	i64.add
	local.get	3
	i64.const	32
	i64.shr_u
	i64.add
	local.get	5
	i64.const	32
	i64.shr_u
	i64.add
	i64.const	0
	i64.add
	return
	end_function

	.section	.text..Laihc_lir_wasm_mul_wide_s64,"",@
	.type	.Laihc_lir_wasm_mul_wide_s64,@function
.Laihc_lir_wasm_mul_wide_s64:
	.functype	.Laihc_lir_wasm_mul_wide_s64 (i64, i64) -> (i64, i64)
	.local	i64, i64, i64, i64, i64, i64
# entry
	local.get	0
	i64.const	4294967295
	i64.and
	local.set	2
	local.get	0
	i64.const	32
	i64.shr_u
	local.set	4
	local.get	1
	i64.const	4294967295
	i64.and
	local.set	3
	local.get	1
	i64.const	32
	i64.shr_u
	local.set	5
	local.get	2
	local.get	3
	i64.mul
	local.set	6
	local.get	2
	local.get	5
	i64.mul
	local.set	2
	local.get	4
	local.get	3
	i64.mul
	local.set	3
	local.get	6
	i64.const	32
	i64.shr_u
	local.get	2
	i64.const	4294967295
	i64.and
	i64.add
	local.get	3
	i64.const	4294967295
	i64.and
	i64.add
	local.set	7
	local.get	6
	i64.const	4294967295
	i64.and
	local.get	7
	i64.const	32
	i64.shl
	i64.or
	local.get	4
	local.get	5
	i64.mul
	local.get	2
	i64.const	32
	i64.shr_u
	i64.add
	local.get	3
	i64.const	32
	i64.shr_u
	i64.add
	local.get	7
	i64.const	32
	i64.shr_u
	i64.add
	local.get	0
	i64.const	63
	i64.shr_s
	local.get	1
	i64.and
	i64.sub
	local.get	1
	i64.const	63
	i64.shr_s
	local.get	0
	i64.and
	i64.sub
	return
	end_function

	.no_dead_strip	__indirect_function_table

