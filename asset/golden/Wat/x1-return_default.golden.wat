(module
  (rec
    (type $return_default_type (;0;) (func (result i32)))
  )
  (rec
    (type $main_type (;1;) (func (result i32)))
  )
  (export "main" (func $main))
  (func $return_default (;0;) (type $return_default_type) (result i32)
    (local $x i32)
    local.get $x
  )
  (func $main (;1;) (type $main_type) (result i32)
    call $return_default
  )
)
