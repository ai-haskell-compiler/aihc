# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lchain (i32, i32) -> (i64)
	.functype	.Lunknown (i64, i64) -> (i32)
	.functype	.Lshared (i64, i64) -> (i64, i32)
	.functype	.Lwide (i64, i64) -> (i32, i32)
	.functype	.Lsigma (i32) -> (i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lchain,"",@
	.type	.Lchain,@function
.Lchain:
	.functype	.Lchain (i32, i32) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i32, i32, i32, i64
# entry
	local.get	0
	local.get	1
	i32.add
	local.set	8
	local.get	8
	i32.const	-1
	i32.mul
	local.set	9
	local.get	9
	local.get	1
	i32.sub
	local.set	10
	local.get	10
	i64.extend_i32_u
	local.set	11
	local.get	11
	return
	end_function

	.section	.text..Lunknown,"",@
	.type	.Lunknown,@function
.Lunknown:
	.functype	.Lunknown (i64, i64) -> (i32)
	.local	i32, i32, i32, i64, f32, f64, i64, i32
# entry
	local.get	0
	local.get	1
	i64.add
	local.set	8
	local.get	8
	i32.wrap_i64
	local.set	9
	local.get	9
	return
	end_function

	.section	.text..Lshared,"",@
	.type	.Lshared,@function
.Lshared:
	.functype	.Lshared (i64, i64) -> (i64, i32)
	.local	i32, i32, i32, i64, f32, f64, i64, i32
# entry
	local.get	0
	local.get	1
	i64.add
	local.set	8
	local.get	8
	i32.wrap_i64
	local.set	9
	local.get	8
	local.get	9
	return
	end_function

	.section	.text..Lwide,"",@
	.type	.Lwide,@function
.Lwide:
	.functype	.Lwide (i64, i64) -> (i32, i32)
	.local	i32, i32, i32, i64, f32, f64, i64, i32, i64, i32
# entry
	local.get	1
	i64.eqz
	if
	i32.const	.Llir_trap_0
	i64.const	24
	call	aihc_lir_trap
	unreachable
	end_if
	local.get	0
	local.get	1
	i64.div_u
	local.set	8
	local.get	8
	i32.wrap_i64
	local.set	9
	local.get	0
	i64.const	32
	i64.shr_u
	local.set	10
	local.get	10
	i32.wrap_i64
	local.set	11
	local.get	9
	local.get	11
	return
	end_function

	.section	.text..Lsigma,"",@
	.type	.Lsigma,@function
.Lsigma:
	.functype	.Lsigma (i32) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i32, i32, i32, i32, i32, i64
# entry
	local.get	0
	i32.const	2
	i32.rotr
	local.set	7
	local.get	0
	i32.const	13
	i32.rotr
	local.set	8
	local.get	0
	i32.const	22
	i32.rotr
	local.set	9
	local.get	7
	local.get	8
	i32.xor
	local.set	10
	local.get	10
	local.get	9
	i32.xor
	local.set	11
	local.get	11
	i64.extend_i32_u
	local.set	12
	local.get	12
	return
	end_function

	.type	.Llir_trap_0,@object
	.section	.rodata..Llir_trap_0,"",@
.Llir_trap_0:
	.ascii	"integer division by zero"
	.size	.Llir_trap_0, 24

	.no_dead_strip	__indirect_function_table

