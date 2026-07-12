(module
  (rec
    (type $main_type (;0;) (func (result i64)))
  )
  (export "main" (func $main))
  (func $main (;0;) (type $main_type) (result i64)
    i64.const 3
    i64.const 4
    i64.add
    i64.const 5
    i64.mul
  )
)
