open Mashtree;
open Binaryen;
open Grain_utils;
open Grain_typed;

module StringSet: Set.S with type elt = string;

/** Environment */

type codegen_env = {
  name: option(string),
  dep_id: int,
  func_debug_idx: int,
  num_args: int,
  num_closure_args: int,
  stack_size,
  /* Allocated closures which need backpatching */
  backpatches: ref(list((Expression.t, closure_data))),
  foreign_import_resolutions: ref(StringSet.t),
  global_import_resolutions: Hashtbl.t(string, string),
  func_import_resolutions: Hashtbl.t(string, string),
  compilation_mode: Config.compilation_mode,
  types: core_reftypes,
}

and core_reftypes = {
  grain_value: Type.t,
  grain_compound_value: Type.t,
  grain_tuple: Type.t,
  grain_array: Type.t,
  grain_record: Type.t,
  grain_variant: Type.t,
  grain_closure: Type.t,
  grain_closure_full: Heap_type.t => Heap_type.t,
  grain_string: Type.t,
  grain_bytes: Type.t,
  grain_number: Type.t,
  grain_int64: Type.t,
  grain_float64: Type.t,
  grain_rational: Type.t,
  grain_big_int: Type.t,
  grain_int32: Type.t,
  grain_float32: Type.t,
  grain_uint32: Type.t,
  grain_uint64: Type.t,
};

let grain_main: string;
let grain_start: string;
let grain_env_name: string;
let grain_global_function_table: string;
let grain_memory: string;

let ref_any: unit => Type.t;
let ref_i31: unit => Type.t;

let wasm_type: Types.allocation_type => Type.t;

let encoded_int32: int => int;
let const_int32: int => Literal.t;
let const_int64: int => Literal.t;
let const_float32: float => Literal.t;
let const_float64: float => Literal.t;

/* These are like the above 'const' functions, but take inputs
   of the underlying types instead */
let wrap_int32: int32 => Literal.t;
let wrap_int64: int64 => Literal.t;
let wrap_float32: float => Literal.t;
let wrap_float64: float => Literal.t;

let compile_const: (Module.t, constant) => Expression.t;

let const_true: Module.t => Expression.t;
let const_false: Module.t => Expression.t;
let const_void: Module.t => Expression.t;
let const_ref_0: Module.t => Expression.t;

let build_func_type: (array(Type.t), Type.t) => Heap_type.t;
let build_basic_func_type: int => Heap_type.t;
let build_array_type:
  (~packed_type: Packed_type.t=?, ~mutable_: bool=?, Type.t) => Type.t;

let store:
  (
    ~ty: Type.t=?,
    ~align: int=?,
    ~offset: int=?,
    ~sz: int=?,
    Module.t,
    Expression.t,
    Expression.t
  ) =>
  Expression.t;

let load:
  (
    ~ty: Type.t=?,
    ~align: int=?,
    ~offset: int=?,
    ~sz: int=?,
    ~signed: bool=?,
    Module.t,
    Expression.t
  ) =>
  Expression.t;

let write_universal_exports:
  (
    ~env: codegen_env,
    Module.t,
    Cmi_format.cmi_infos,
    list((int, list(export))),
    (int, string) => string
  ) =>
  unit;
