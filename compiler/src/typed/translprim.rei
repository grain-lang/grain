open Typedtree;

type primitive_constant =
  | HeapBase
  | HeapStart
  | HeapTypeMetadata;

type primitive =
  | PrimitiveConstant(primitive_constant)
  | Primitive0(prim0)
  | Primitive1(prim1)
  | Primitive2(prim2)
  | PrimitiveN(primn);

module PrimMap: Hashtbl.S with type key = string;

let prim_map: PrimMap.t(primitive);

let transl_prim:
  (Env.t, value_description) =>
  (list(value_binding), Env.t, Typedtree.attributes);
