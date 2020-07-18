open Grain_parsing;
open Ast_helper;
open Typedtree;
open Parsetree;

type primitive =
  | Primitive1(prim1)
  | Primitive2(prim2);

module PrimMap =
  Hashtbl.Make({
    type t = string;
    let hash = x => Hashtbl.hash(x);
    let equal = (a, b) => String.compare(a, b) === 0;
  });

let default_loc = default_loc_src^();

let mkident = name =>
  Exp.ident(Location.mkloc(Identifier.IdentName(name), default_loc));
let mkpatvar = name => {
  ppat_desc: PPatVar(Location.mkloc(name, default_loc)),
  ppat_loc: default_loc,
};

let id_a = mkident("a");
let id_b = mkident("b");
let pat_a = mkpatvar("a");
let pat_b = mkpatvar("b");

let prim_map =
  PrimMap.of_seq(
    List.to_seq([
      ("@incr", Primitive1(Incr)),
      ("@decr", Primitive1(Decr)),
      ("@not", Primitive1(Not)),
      ("@box", Primitive1(Box)),
      ("@unbox", Primitive1(Unbox)),
      ("@ignore", Primitive1(Ignore)),
      ("@assert", Primitive1(Assert)),
      ("@fail", Primitive1(FailWith)),
      ("@plus", Primitive2(Plus)),
      ("@minus", Primitive2(Minus)),
      ("@times", Primitive2(Times)),
      ("@divides", Primitive2(Divide)),
      ("@modulo", Primitive2(Mod)),
      ("@less", Primitive2(Less)),
      ("@greater", Primitive2(Greater)),
      ("@lesseq", Primitive2(LessEq)),
      ("@greatereq", Primitive2(GreaterEq)),
      ("@eq", Primitive2(Eq)),
      ("@and", Primitive2(And)),
      ("@or", Primitive2(Or)),
      ("@array.length", Primitive1(ArrayLength)),
      ("@array.make", Primitive2(ArrayMake)),
      ("@array.init", Primitive2(ArrayInit)),
      ("@int64.fromNumber", Primitive1(Int64FromNumber)),
      ("@int64.toNumber", Primitive1(Int64ToNumber)),
      ("@int64.lnot", Primitive1(Int64Lnot)),
      ("@int64.land", Primitive2(Int64Land)),
      ("@int64.lor", Primitive2(Int64Lor)),
      ("@int64.lxor", Primitive2(Int64Lxor)),
      ("@int64.lsl", Primitive2(Int64Lsl)),
      ("@int64.lsr", Primitive2(Int64Lsr)),
      ("@int64.asr", Primitive2(Int64Asr)),
      ("@int64.gt", Primitive2(Int64Gt)),
      ("@int64.gte", Primitive2(Int64Gte)),
      ("@int64.lt", Primitive2(Int64Lt)),
      ("@int64.lte", Primitive2(Int64Lte)),
    ]),
  );

let transl_prim = (env, desc) => {
  let prim =
    try(PrimMap.find(prim_map, List.hd(desc.tvd_prim))) {
    | Not_found => failwith("This primitive does not exist.")
    };

  let value =
    switch (prim) {
    | Primitive1(p) => Exp.lambda([pat_a], Exp.prim1(p, id_a))
    | Primitive2(p) => Exp.lambda([pat_a, pat_b], Exp.prim2(p, id_a, id_b))
    };

  let binds = [
    {
      pvb_pat: {
        ppat_desc: PPatVar(desc.tvd_name),
        ppat_loc: desc.tvd_loc,
      },
      pvb_expr: value,
      pvb_loc: desc.tvd_loc,
    },
  ];
  let mut_flag = desc.tvd_val.val_mutable ? Mutable : Immutable;
  Typecore.type_binding(env, Nonrecursive, mut_flag, binds, None);
};
