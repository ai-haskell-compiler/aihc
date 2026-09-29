# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Ldouble (f64, i64) -> (f64)
	.functype	.Lsingle (i64, f32) -> (f32)
	.functype	.Lforward (f64, i64) -> (f64)
	.functype	.Lindirect (i32, f32, i64) -> (f32)
	.functype	.Lmain () -> (f64, f32)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Ldouble,"",@
	.type	.Ldouble,@function
.Ldouble:
	.functype	.Ldouble (f64, i64) -> (f64)
	.local	i32, i32, i32, i64, f32, f64, i32, f64
# entry
	local.get	1
	i64.const	42
	i64.eq
	local.set	8
	local.get	0
	i64.const	0
	f64.reinterpret_i64
	local.get	8
	f64.select
	local.set	9
	local.get	9
	return
	end_function

	.section	.text..Lsingle,"",@
	.type	.Lsingle,@function
.Lsingle:
	.functype	.Lsingle (i64, f32) -> (f32)
	.local	i32, i32, i32, i64, f32, f64, i32, f32
# entry
	local.get	0
	i64.const	19
	i64.eq
	local.set	8
	local.get	1
	i32.const	0
	f32.reinterpret_i32
	local.get	8
	f32.select
	local.set	9
	local.get	9
	return
	end_function

	.section	.text..Lforward,"",@
	.type	.Lforward,@function
.Lforward:
	.functype	.Lforward (f64, i64) -> (f64)
	.local	i32, i32, i32, i64, f32, f64, f64, f64
# entry
	local.get	0
	local.get	1
	call	.Ldouble
	local.set	8
	local.get	8
	i64.const	4617315517961601024
	f64.reinterpret_i64
	f64.add
	local.set	9
	local.get	9
	i64.const	42
	return_call	.Ldouble
	end_function

	.section	.text..Lindirect,"",@
	.type	.Lindirect,@function
.Lindirect:
	.functype	.Lindirect (i32, f32, i64) -> (f32)
	.local	i32, i32, i32, i64, f32, f64
# entry
	local.get	0
	i32.eqz
	if
	i32.const	.Llir_trap_0
	i64.const	31
	call	aihc_lir_trap
	unreachable
	end_if
	i64.const	19
	i32.const	1080033280
	f32.reinterpret_i32
	local.get	0
	return_call_indirect	__indirect_function_table, (i64, f32) -> (f32)
	end_function

	.section	.text..Lmain,"",@
	.type	.Lmain,@function
.Lmain:
	.functype	.Lmain () -> (f64, f32)
	.local	i32, i32, i32, i64, f32, f64, f64, i32, f32
# entry
	i64.const	4611686018427387904
	f64.reinterpret_i64
	i64.const	42
	call	.Lforward
	local.set	6
	i32.const	.Ltable
	i32.load	0
	local.set	7
	local.get	7
	i32.const	1065353216
	f32.reinterpret_i32
	i64.const	8
	call	.Lindirect
	local.set	8
	local.get	6
	local.get	8
	return
	end_function

	.type	.Ltable,@object
	.section	.rodata..Ltable,"",@
	.p2align	3, 0x0
.Ltable:
	.int32	.Lsingle
	.size	.Ltable, 4

	.type	.Llir_trap_0,@object
	.section	.rodata..Llir_trap_0,"",@
.Llir_trap_0:
	.ascii	"indirect call to a non-function"
	.size	.Llir_trap_0, 31

	.no_dead_strip	__indirect_function_table

