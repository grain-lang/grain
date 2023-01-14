open Mashtree;
open Binaryen;
open Grain_typed;

let grain_main: string;
let grain_start: string;
let grain_env_name: string;
let grain_global_function_table: string;
let grain_memory: string;

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

let compile_const: constant => Literal.t;

let const_true: unit => Literal.t;
let const_false: unit => Literal.t;
let const_void: unit => Literal.t;

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

let is_grain_env: string => bool;

let get_exported_names:
  (
    ~function_names: Hashtbl.t(string, string)=?,
    ~global_names: Hashtbl.t(string, string)=?,
    Module.t
  ) =>
  Hashtbl.t(string, string);

let write_universal_exports:
  (Module.t, Cmi_format.cmi_infos, Hashtbl.t(string, string)) => unit;

let compiling_wasi_polyfill: option(string) => bool;
