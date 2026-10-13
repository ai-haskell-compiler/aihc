# Lir module compiled by Aihc.Wasm.Lir.
	.text
	.functype	aihc_lir_trap (i32, i64) -> ()
	.functype	.Lconstant (i32) -> (i32, i32)
	.functype	.Lvariable (i64, i64) -> (i64, i64)
	.functype	.Lpattern (i32) -> (i32)
	.functype	.Lsigma (i32) -> (i32)
	.functype	.Lnarrow (i32, i32, i32, i32) -> (i32, i32)
	.functype	.Lwide_pattern (i32) -> (i64)
	.functype	.Lconstant64 (i64) -> (i64, i64)
	.tabletype	__indirect_function_table, funcref

	.section	.text..Lconstant,"",@
	.type	.Lconstant,@function
.Lconstant:
	.functype	.Lconstant (i32) -> (i32, i32)
	.local	i32, i32, i32, i64, f32, f64, i32, i32
# entry
	local.get	0
	i32.const	7
	i32.rotr
	local.set	7
	local.get	7
	i32.const	9
	i32.rotl
	local.set	8
	local.get	0
	local.get	8
	return
	end_function

	.section	.text..Lvariable,"",@
	.type	.Lvariable,@function
.Lvariable:
	.functype	.Lvariable (i64, i64) -> (i64, i64)
	.local	i32, i32, i32, i64, f32, f64, i64, i64
# entry
	local.get	0
	local.get	1
	i64.rotr
	local.set	8
	local.get	8
	local.get	1
	i64.rotl
	local.set	9
	local.get	8
	local.get	9
	return
	end_function

	.section	.text..Lpattern,"",@
	.type	.Lpattern,@function
.Lpattern:
	.functype	.Lpattern (i32) -> (i32)
	.local	i32, i32, i32, i64, f32, f64, i32
# entry
	local.get	0
	i32.const	7
	i32.rotr
	local.set	7
	local.get	7
	return
	end_function

	.section	.text..Lsigma,"",@
	.type	.Lsigma,@function
.Lsigma:
	.functype	.Lsigma (i32) -> (i32)
	.local	i32, i32, i32, i64, f32, f64, i32, i32, i32
# entry
	local.get	0
	i32.const	7
	i32.rotr
	local.set	7
	local.get	0
	i32.const	18
	i32.rotr
	local.set	8
	local.get	7
	local.get	8
	i32.xor
	local.set	9
	local.get	9
	return
	end_function

	.section	.text..Lnarrow,"",@
	.type	.Lnarrow,@function
.Lnarrow:
	.functype	.Lnarrow (i32, i32, i32, i32) -> (i32, i32)
	.local	i32, i32, i32, i64, f32, f64, i32, i32
# entry
	local.get	0
	local.get	1
	i32.const	7
	i32.and
	i32.shl
	local.get	0
	i32.const	8
	local.get	1
	i32.const	7
	i32.and
	i32.sub
	i32.shr_u
	i32.or
	i32.const	255
	i32.and
	local.set	10
	local.get	2
	local.get	3
	i32.const	15
	i32.and
	i32.shr_u
	local.get	2
	i32.const	16
	local.get	3
	i32.const	15
	i32.and
	i32.sub
	i32.shl
	i32.or
	i32.const	65535
	i32.and
	local.set	11
	local.get	10
	local.get	11
	return
	end_function

	.section	.text..Lwide_pattern,"",@
	.type	.Lwide_pattern,@function
.Lwide_pattern:
	.functype	.Lwide_pattern (i32) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i32, i64
# entry
	local.get	0
	i32.const	7
	i32.rotr
	local.set	7
	local.get	7
	i64.extend_i32_u
	local.set	8
	local.get	8
	return
	end_function

	.section	.text..Lconstant64,"",@
	.type	.Lconstant64,@function
.Lconstant64:
	.functype	.Lconstant64 (i64) -> (i64, i64)
	.local	i32, i32, i32, i64, f32, f64, i64
# entry
	local.get	0
	i64.const	41
	i64.rotl
	local.set	7
	local.get	0
	local.get	7
	return
	end_function

	.no_dead_strip	__indirect_function_table

