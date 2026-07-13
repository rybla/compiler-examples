(module
  (rec
    (type $tm (;0;) (struct (field i32) (field (ref null $tm)) (field (ref null $tm))))
    (type $bigint (;1;) (struct (field i64) (field i64) (field i64) (field i64) (field i64) (field i64) (field i64) (field i64) (field i64) (field i64)))
    (type $main_type (;2;) (func (result i64)))
    (type $pair_type (;3;) (func (param i64 i64) (result i64)))
    (type $bigint_from_i64_type (;4;) (func (param i64) (result (ref null $bigint))))
    (type $bigint_to_i64_type (;5;) (func (param (ref null $bigint)) (result i64)))
    (type $bigint_add_type (;6;) (func (param (ref null $bigint) (ref null $bigint)) (result (ref null $bigint))))
    (type $bigint_mul_type (;7;) (func (param (ref null $bigint) (ref null $bigint)) (result (ref null $bigint))))
    (type $bigint_div2_type (;8;) (func (param (ref null $bigint)) (result (ref null $bigint))))
    (type $bigint_pair_type (;9;) (func (param (ref null $bigint) (ref null $bigint)) (result (ref null $bigint))))
    (type $godel_type (;10;) (func (param (ref null $tm)) (result (ref null $bigint))))
    (type $step_type (;11;) (func (param (ref null $tm)) (result (ref null $tm))))
    (type $deepStep_type (;12;) (func (param (ref null $tm)) (result (ref null $tm))))
    (type $evaluate_type (;13;) (func (param (ref null $tm)) (result (ref null $tm))))
  )
  (export "main" (func $main))
  (func $pair (;0;) (type $pair_type) (param i64 i64) (result i64)
    (local i64)
    local.get 0
    local.get 1
    i64.add
    local.set 2
    local.get 2
    i64.const 1
    i64.and
    i64.eqz
    if (result i64) ;; label = @1
      local.get 2
      i64.const 2
      i64.div_u
      local.get 2
      i64.const 1
      i64.add
      i64.mul
    else
      local.get 2
      local.get 2
      i64.const 1
      i64.add
      i64.const 2
      i64.div_u
      i64.mul
    end
    local.get 1
    i64.add
  )
  (func $bigint_from_i64 (;1;) (type $bigint_from_i64_type) (param i64) (result (ref null $bigint))
    local.get 0
    i64.const 4294967295
    i64.and
    local.get 0
    i64.const 32
    i64.shr_u
    i64.const 0
    i64.const 0
    i64.const 0
    i64.const 0
    i64.const 0
    i64.const 0
    i64.const 0
    i64.const 0
    struct.new $bigint
  )
  (func $bigint_to_i64 (;2;) (type $bigint_to_i64_type) (param (ref null $bigint)) (result i64)
    local.get 0
    struct.get $bigint 0
    local.get 0
    struct.get $bigint 1
    i64.const 32
    i64.shl
    i64.add
  )
  (func $bigint_add (;3;) (type $bigint_add_type) (param (ref null $bigint) (ref null $bigint)) (result (ref null $bigint))
    (local i64 i64)
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 0
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 1
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 2
    local.get 1
    struct.get $bigint 2
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 3
    local.get 1
    struct.get $bigint 3
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 4
    local.get 1
    struct.get $bigint 4
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 5
    local.get 1
    struct.get $bigint 5
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 6
    local.get 1
    struct.get $bigint 6
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 7
    local.get 1
    struct.get $bigint 7
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 8
    local.get 1
    struct.get $bigint 8
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 9
    local.get 1
    struct.get $bigint 9
    i64.add
    local.get 2
    i64.add
    local.tee 3
    i64.const 4294967295
    i64.and
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    struct.new $bigint
  )
  (func $bigint_div2 (;4;) (type $bigint_div2_type) (param (ref null $bigint)) (result (ref null $bigint))
    (local i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64)
    i64.const 0
    local.set 1
    local.get 0
    struct.get $bigint 9
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 12
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 0
    struct.get $bigint 8
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 11
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 0
    struct.get $bigint 7
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 10
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 0
    struct.get $bigint 6
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 9
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 0
    struct.get $bigint 5
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 8
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 0
    struct.get $bigint 4
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 7
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 0
    struct.get $bigint 3
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 6
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 0
    struct.get $bigint 2
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 5
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 0
    struct.get $bigint 1
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 4
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 0
    struct.get $bigint 0
    local.get 1
    i64.const 32
    i64.shl
    i64.add
    local.tee 2
    i64.const 1
    i64.shr_u
    local.set 3
    local.get 2
    i64.const 1
    i64.and
    local.set 1
    local.get 3
    local.get 4
    local.get 5
    local.get 6
    local.get 7
    local.get 8
    local.get 9
    local.get 10
    local.get 11
    local.get 12
    struct.new $bigint
  )
  (func $bigint_mul (;5;) (type $bigint_mul_type) (param (ref null $bigint) (ref null $bigint)) (result (ref null $bigint))
    (local i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64)
    i64.const 0
    local.set 4
    i64.const 0
    local.set 5
    i64.const 0
    local.set 6
    i64.const 0
    local.set 7
    i64.const 0
    local.set 8
    i64.const 0
    local.set 9
    i64.const 0
    local.set 10
    i64.const 0
    local.set 11
    i64.const 0
    local.set 12
    i64.const 0
    local.set 13
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 4
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 4
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 1
    i64.mul
    local.get 5
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 5
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 2
    i64.mul
    local.get 6
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 6
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 3
    i64.mul
    local.get 7
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 7
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 4
    i64.mul
    local.get 8
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 8
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 5
    i64.mul
    local.get 9
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 9
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 6
    i64.mul
    local.get 10
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 10
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 7
    i64.mul
    local.get 11
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 11
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 8
    i64.mul
    local.get 12
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 12
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 0
    local.get 1
    struct.get $bigint 9
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 5
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 5
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 1
    i64.mul
    local.get 6
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 6
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 2
    i64.mul
    local.get 7
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 7
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 3
    i64.mul
    local.get 8
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 8
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 4
    i64.mul
    local.get 9
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 9
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 5
    i64.mul
    local.get 10
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 10
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 6
    i64.mul
    local.get 11
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 11
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 7
    i64.mul
    local.get 12
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 12
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 1
    local.get 1
    struct.get $bigint 8
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 2
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 6
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 6
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 2
    local.get 1
    struct.get $bigint 1
    i64.mul
    local.get 7
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 7
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 2
    local.get 1
    struct.get $bigint 2
    i64.mul
    local.get 8
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 8
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 2
    local.get 1
    struct.get $bigint 3
    i64.mul
    local.get 9
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 9
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 2
    local.get 1
    struct.get $bigint 4
    i64.mul
    local.get 10
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 10
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 2
    local.get 1
    struct.get $bigint 5
    i64.mul
    local.get 11
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 11
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 2
    local.get 1
    struct.get $bigint 6
    i64.mul
    local.get 12
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 12
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 2
    local.get 1
    struct.get $bigint 7
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 3
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 7
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 7
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 3
    local.get 1
    struct.get $bigint 1
    i64.mul
    local.get 8
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 8
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 3
    local.get 1
    struct.get $bigint 2
    i64.mul
    local.get 9
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 9
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 3
    local.get 1
    struct.get $bigint 3
    i64.mul
    local.get 10
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 10
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 3
    local.get 1
    struct.get $bigint 4
    i64.mul
    local.get 11
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 11
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 3
    local.get 1
    struct.get $bigint 5
    i64.mul
    local.get 12
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 12
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 3
    local.get 1
    struct.get $bigint 6
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 4
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 8
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 8
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 4
    local.get 1
    struct.get $bigint 1
    i64.mul
    local.get 9
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 9
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 4
    local.get 1
    struct.get $bigint 2
    i64.mul
    local.get 10
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 10
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 4
    local.get 1
    struct.get $bigint 3
    i64.mul
    local.get 11
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 11
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 4
    local.get 1
    struct.get $bigint 4
    i64.mul
    local.get 12
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 12
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 4
    local.get 1
    struct.get $bigint 5
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 5
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 9
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 9
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 5
    local.get 1
    struct.get $bigint 1
    i64.mul
    local.get 10
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 10
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 5
    local.get 1
    struct.get $bigint 2
    i64.mul
    local.get 11
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 11
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 5
    local.get 1
    struct.get $bigint 3
    i64.mul
    local.get 12
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 12
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 5
    local.get 1
    struct.get $bigint 4
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 6
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 10
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 10
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 6
    local.get 1
    struct.get $bigint 1
    i64.mul
    local.get 11
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 11
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 6
    local.get 1
    struct.get $bigint 2
    i64.mul
    local.get 12
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 12
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 6
    local.get 1
    struct.get $bigint 3
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 7
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 11
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 11
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 7
    local.get 1
    struct.get $bigint 1
    i64.mul
    local.get 12
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 12
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 7
    local.get 1
    struct.get $bigint 2
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 8
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 12
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 12
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 0
    struct.get $bigint 8
    local.get 1
    struct.get $bigint 1
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    i64.const 0
    local.set 2
    local.get 0
    struct.get $bigint 9
    local.get 1
    struct.get $bigint 0
    i64.mul
    local.get 13
    i64.add
    local.get 2
    i64.add
    local.set 3
    local.get 3
    i64.const 4294967295
    i64.and
    local.set 13
    local.get 3
    i64.const 32
    i64.shr_u
    local.set 2
    local.get 4
    local.get 5
    local.get 6
    local.get 7
    local.get 8
    local.get 9
    local.get 10
    local.get 11
    local.get 12
    local.get 13
    struct.new $bigint
  )
  (func $bigint_pair (;6;) (type $bigint_pair_type) (param (ref null $bigint) (ref null $bigint)) (result (ref null $bigint))
    (local (ref null $bigint) (ref null $bigint))
    local.get 0
    local.get 1
    call $bigint_add
    local.set 2
    local.get 2
    struct.get $bigint 0
    i64.const 1
    i64.and
    i64.eqz
    if (result (ref null $bigint)) ;; label = @1
      local.get 2
      call $bigint_div2
      local.get 2
      i64.const 1
      call $bigint_from_i64
      call $bigint_add
      call $bigint_mul
    else
      local.get 2
      local.get 2
      i64.const 1
      call $bigint_from_i64
      call $bigint_add
      call $bigint_div2
      call $bigint_mul
    end
    local.get 1
    call $bigint_add
  )
  (func $godel (;7;) (type $godel_type) (param (ref null $tm)) (result (ref null $bigint))
    (local i32 (ref null $bigint) (ref null $bigint) (ref null $bigint) (ref null $bigint))
    local.get 0
    ref.is_null
    if (result (ref null $bigint)) ;; label = @1
      i64.const 0
      call $bigint_from_i64
    else
      local.get 0
      struct.get $tm 0
      local.tee 1
      i32.const 6
      i32.lt_u
      if (result (ref null $bigint)) ;; label = @2
        local.get 1
        i64.extend_i32_u
        call $bigint_from_i64
      else
        local.get 0
        struct.get $tm 1
        call $godel
        local.set 2
        local.get 0
        struct.get $tm 2
        call $godel
        local.set 3
        local.get 2
        local.get 3
        call $bigint_pair
        local.set 4
        i64.const 6
        call $bigint_from_i64
        local.get 4
        call $bigint_pair
      end
    end
  )
  (func $step (;8;) (type $step_type) (param (ref null $tm)) (result (ref null $tm))
    (local (ref null $tm) (ref null $tm) (ref null $tm) (ref null $tm) (ref null $tm) (ref null $tm) (ref null $tm))
    ref.null $tm
    local.set 7
    block $done
      local.get 0
      ref.is_null
      br_if $done
      local.get 0
      struct.get $tm 0
      i32.const 6
      i32.ne
      br_if $done
      local.get 0
      struct.get $tm 2
      local.set 6
      local.get 0
      struct.get $tm 1
      local.set 1
      local.get 1
      ref.is_null
      br_if $done
      local.get 1
      struct.get $tm 0
      i32.const 3
      i32.eq
      if ;; label = @2
        local.get 6
        local.set 7
        br $done
      end
      local.get 1
      struct.get $tm 0
      i32.const 6
      i32.ne
      br_if $done
      local.get 1
      struct.get $tm 2
      local.set 5
      local.get 1
      struct.get $tm 1
      local.set 2
      local.get 2
      ref.is_null
      br_if $done
      local.get 2
      struct.get $tm 0
      i32.const 2
      i32.eq
      if ;; label = @2
        local.get 5
        local.set 7
        br $done
      end
      local.get 2
      struct.get $tm 0
      i32.const 6
      i32.ne
      br_if $done
      local.get 2
      struct.get $tm 2
      local.set 4
      local.get 2
      struct.get $tm 1
      local.set 3
      local.get 3
      ref.is_null
      br_if $done
      local.get 3
      struct.get $tm 0
      i32.const 1
      i32.eq
      if ;; label = @2
        i32.const 6
        i32.const 6
        local.get 4
        local.get 5
        struct.new $tm
        local.get 6
        struct.new $tm
        local.set 7
        br $done
      end
      local.get 3
      struct.get $tm 0
      i32.const 4
      i32.eq
      if ;; label = @2
        i32.const 6
        i32.const 6
        local.get 4
        local.get 5
        struct.new $tm
        local.get 6
        struct.new $tm
        local.set 7
        br $done
      end
      local.get 3
      struct.get $tm 0
      i32.const 5
      i32.eq
      if ;; label = @2
        i32.const 6
        i32.const 6
        local.get 4
        local.get 6
        struct.new $tm
        local.get 5
        struct.new $tm
        local.set 7
        br $done
      end
    end
    local.get 7
  )
  (func $deepStep (;9;) (type $deepStep_type) (param (ref null $tm)) (result (ref null $tm))
    (local (ref null $tm) (ref null $tm) (ref null $tm))
    ref.null $tm
    local.set 3
    block $done
      local.get 0
      ref.is_null
      br_if $done
      local.get 0
      struct.get $tm 0
      i32.const 6
      i32.ne
      br_if $done
      local.get 0
      call $step
      local.tee 3
      ref.is_null
      i32.eqz
      br_if $done
      local.get 0
      struct.get $tm 1
      local.set 1
      local.get 0
      struct.get $tm 2
      call $deepStep
      local.tee 3
      ref.is_null
      i32.eqz
      if ;; label = @2
        i32.const 6
        local.get 1
        local.get 3
        struct.new $tm
        local.set 3
        br $done
      end
      local.get 0
      struct.get $tm 2
      local.set 2
      local.get 0
      struct.get $tm 1
      call $deepStep
      local.tee 3
      ref.is_null
      i32.eqz
      if ;; label = @2
        i32.const 6
        local.get 3
        local.get 2
        struct.new $tm
        local.set 3
        br $done
      end
      ref.null $tm
      local.set 3
    end
    local.get 3
  )
  (func $evaluate (;10;) (type $evaluate_type) (param (ref null $tm)) (result (ref null $tm))
    (local (ref null $tm) i32)
    i32.const 1000
    local.set 2
    block $done
      loop $loop
        local.get 2
        i32.eqz
        br_if $done
        local.get 2
        i32.const 1
        i32.sub
        local.set 2
        local.get 0
        call $deepStep
        local.tee 1
        ref.is_null
        br_if $done
        local.get 1
        local.set 0
        br $loop
      end
    end
    local.get 0
  )
  (func $main (;11;) (type $main_type) (result i64)
    i32.const 6
    i32.const 6
    i32.const 6
    i32.const 1
    ref.null $tm
    ref.null $tm
    struct.new $tm
    i32.const 4
    ref.null $tm
    ref.null $tm
    struct.new $tm
    struct.new $tm
    i32.const 3
    ref.null $tm
    ref.null $tm
    struct.new $tm
    struct.new $tm
    i32.const 3
    ref.null $tm
    ref.null $tm
    struct.new $tm
    struct.new $tm
    call $evaluate
    call $godel
    call $bigint_to_i64
  )
)
