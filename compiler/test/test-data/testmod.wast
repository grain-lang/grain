(module
  (type $type0 (func (param i32)))
  (type $type1 (func))
  (import "imports" "imported_func" (func $import0 (param i32)))
  (export "exported_func" (func $func1))
  (func $func1
    (i32.const 42)
    (call $import0)))
