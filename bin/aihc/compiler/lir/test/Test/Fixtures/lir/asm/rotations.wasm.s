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
	.functype	.Lshifted64 (i64, i64) -> (i64, i64, i64)
	.functype	.Lshifted32 (i32, i32) -> (i32, i32, i32)
	.functype	.Lcomplement (i64, i64, i32, i32) -> (i64, i32)
	.functype	.Lnested (i64, i64) -> (i64)
	.functype	.Lshared_shift (i64, i64) -> (i64, i64)
	.functype	.Lvariable_shift (i32, i32, i32) -> (i32)
	.functype	.Lpartial_complement (i64, i64) -> (i64)
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

	.section	.text..Lshifted64,"",@
	.type	.Lshifted64,@function
.Lshifted64:
	.functype	.Lshifted64 (i64, i64) -> (i64, i64, i64)
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i64, i64, i64, i64
# entry
	local.get	1
	i64.const	65
	i64.shl
	local.set	8
	local.get	0
	local.get	8
	i64.and
	local.set	9
	local.get	1
	i64.const	7
	i64.shr_u
	local.set	10
	local.get	10
	local.get	0
	i64.xor
	local.set	11
	local.get	1
	i64.const	63
	i64.shr_s
	local.set	12
	local.get	0
	local.get	12
	i64.or
	local.set	13
	local.get	9
	local.get	11
	local.get	13
	return
	end_function

	.section	.text..Lshifted32,"",@
	.type	.Lshifted32,@function
.Lshifted32:
	.functype	.Lshifted32 (i32, i32) -> (i32, i32, i32)
	.local	i32, i32, i32, i64, f32, f64, i32, i32, i32, i32, i32, i32
# entry
	local.get	1
	i32.const	33
	i32.shl
	local.set	8
	local.get	8
	local.get	0
	i32.or
	local.set	9
	local.get	1
	i32.const	-1
	i32.shr_u
	local.set	10
	local.get	0
	local.get	10
	i32.and
	local.set	11
	local.get	1
	i32.const	31
	i32.shr_s
	local.set	12
	local.get	0
	local.get	12
	i32.xor
	local.set	13
	local.get	9
	local.get	11
	local.get	13
	return
	end_function

	.section	.text..Lcomplement,"",@
	.type	.Lcomplement,@function
.Lcomplement:
	.functype	.Lcomplement (i64, i64, i32, i32) -> (i64, i32)
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i32, i32
# entry
	local.get	0
	i64.const	-1
	i64.xor
	local.set	10
	local.get	1
	local.get	10
	i64.and
	local.set	11
	i32.const	-1
	local.get	2
	i32.xor
	local.set	12
	local.get	12
	local.get	3
	i32.and
	local.set	13
	local.get	11
	local.get	13
	return
	end_function

	.section	.text..Lnested,"",@
	.type	.Lnested,@function
.Lnested:
	.functype	.Lnested (i64, i64) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i64, i64, i64
# entry
	local.get	0
	i64.const	3
	i64.rotr
	local.set	8
	local.get	8
	i64.const	-1
	i64.xor
	local.set	9
	local.get	1
	local.get	9
	i64.and
	local.set	10
	local.get	10
	return
	end_function

	.section	.text..Lshared_shift,"",@
	.type	.Lshared_shift,@function
.Lshared_shift:
	.functype	.Lshared_shift (i64, i64) -> (i64, i64)
	.local	i32, i32, i32, i64, f32, f64, i64, i64
# entry
	local.get	0
	i64.const	7
	i64.shr_u
	local.set	8
	local.get	1
	local.get	8
	i64.xor
	local.set	9
	local.get	8
	local.get	9
	return
	end_function

	.section	.text..Lvariable_shift,"",@
	.type	.Lvariable_shift,@function
.Lvariable_shift:
	.functype	.Lvariable_shift (i32, i32, i32) -> (i32)
	.local	i32, i32, i32, i64, f32, f64, i32, i32
# entry
	local.get	0
	local.get	2
	i32.shr_u
	local.set	9
	local.get	1
	local.get	9
	i32.and
	local.set	10
	local.get	10
	return
	end_function

	.section	.text..Lpartial_complement,"",@
	.type	.Lpartial_complement,@function
.Lpartial_complement:
	.functype	.Lpartial_complement (i64, i64) -> (i64)
	.local	i32, i32, i32, i64, f32, f64, i64, i64
# entry
	local.get	0
	i64.const	4294967295
	i64.xor
	local.set	8
	local.get	1
	local.get	8
	i64.and
	local.set	9
	local.get	9
	return
	end_function

	.no_dead_strip	__indirect_function_table

