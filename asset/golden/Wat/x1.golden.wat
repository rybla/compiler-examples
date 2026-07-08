(module
  (rec
    (type $return_default_type (;0;) (sub (func (result i32))))
  )
  (export "return_default" (func $return_default))
  (func $return_default (;0;) (type $return_default_type) (result i32)
    (local $x i32)
    local.get $x
  )
)
