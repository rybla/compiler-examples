(module
  (export "return_default" (func $return_default))
  (type $return_default_type (func (result i32)))
  (func $return_default (type $return_default_type) (local $x i32) (local.get $x))
)