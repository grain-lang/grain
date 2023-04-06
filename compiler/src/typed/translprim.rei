open Typedtree;

type primitive_constant =
  | HeapTypeMetadata
  | ElideTypeInfo;

type primitive =
  | PrimitiveConstant(primitive_constant)
  | Primitive0(prim0)
  | Primitive1(prim1)
  | Primitive2(prim2)
  | PrimitiveN(primn);

module PrimMap: Hashtbl.S with type key = string;

let prim_map: PrimMap.t(primitive);

let transl_prim:
  (Env.t, Grain_parsing.Parsetree.primitive_description) =>
  (list(value_binding), Ident.t, Types.value_description, Env.t);
