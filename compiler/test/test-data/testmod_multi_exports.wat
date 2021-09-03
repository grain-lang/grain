(module
  (type $type0 (func (param i32)))
  (type $type1 (func))
  (import "imports" "imported_func" (func $import0 (param i32)))
  (import "imports" "mem" (memory $0 0))
  (export "exported_func" (func $func1))
  (export "exported_func2" (func $func2))
  (export "exported_glob" (global $glob1))
  (export "exported_glob2" (global $glob2))
  (export "memory" (memory $0))
  (func $func1
    (i32.const 42)
    (call $import0))
  (func $func2
    (i32.const 43)
    (call $import0))
  (global $glob1 i32 (i32.const 44))
  (global $glob2 i32 (i32.const 45)))
