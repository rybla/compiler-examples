(module
  (rec
    (type $main_type (;0;) (sub (func (result i32))))
  )
  (export "main" (func $main))
  (func $main (;0;) (type $main_type) (result i32)
    i32.const 3
    i32.const 4
    i32.add
    i32.const 5
    i32.mul
  )
)
