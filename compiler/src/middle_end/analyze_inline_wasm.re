open Anftree;
open Anf_iterator;
open Grain_typed;

type inline_type =
  | WasmPrim1(prim1)
  | WasmPrim2(prim2)
  | WasmPrimN(primn);

type analysis +=
  | InlineableWasmTable(Ident.tbl(inline_type));

let inline_wasm_tbl: ref(Ident.tbl(inline_type)) = (
  ref(Ident.empty): ref(Ident.tbl(inline_type))
);

let mod_has_inlineable_wasm = ref(false);

let rec get_inlineable_wasm_tbl = lst =>
  switch (lst) {
  | [] => raise(Not_found)
  | [InlineableWasmTable(t), ..._] => t
  | [_, ...tl] => get_inlineable_wasm_tbl(tl)
  };

let set_inlineable_wasm = (id, inline_type) =>
  inline_wasm_tbl := Ident.add(id, inline_type, inline_wasm_tbl^);

let get_inline_wasm_type = (id: Ident.t): inline_type =>
  Ident.find_same(id, inline_wasm_tbl^);

let has_inline_wasm_type = (id: Ident.t): bool =>
  Ident.find_same_opt(id, inline_wasm_tbl^) |> Option.is_some;

module StringHash =
  Hashtbl.Make({
    type t = string;
    let hash = x => Hashtbl.hash(x);
    let equal = (a, b) => String.compare(a, b) === 0;
  });

let primitive_map =
  StringHash.of_seq(
    List.to_seq([
      ("ofGrain", WasmPrim1(WasmOfGrain)),
      ("toGrain", WasmPrim1(WasmToGrain)),
      ("load_i32", WasmPrim2(WasmLoadI32)),
      ("store_i32", WasmPrimN(WasmStoreI32)),
      ("clz_i32", WasmPrim1(WasmUnaryI32({op: "clz_i32", boolean: false}))),
      ("ctz_i32", WasmPrim1(WasmUnaryI32({op: "ctz_i32", boolean: false}))),
      (
        "popcnt_i32",
        WasmPrim1(WasmUnaryI32({op: "popcnt_i32", boolean: false})),
      ),
      (
        "eq_z_i32",
        WasmPrim1(WasmUnaryI32({op: "eq_z_i32", boolean: true})),
      ),
      (
        "add_i32",
        WasmPrim2(WasmBinaryI32({op: "add_i32", boolean: false})),
      ),
      (
        "sub_i32",
        WasmPrim2(WasmBinaryI32({op: "sub_i32", boolean: false})),
      ),
      (
        "mul_i32",
        WasmPrim2(WasmBinaryI32({op: "mul_i32", boolean: false})),
      ),
      (
        "div_s_i32",
        WasmPrim2(WasmBinaryI32({op: "div_s_i32", boolean: false})),
      ),
      (
        "div_u_i32",
        WasmPrim2(WasmBinaryI32({op: "div_u_i32", boolean: false})),
      ),
      (
        "rem_s_i32",
        WasmPrim2(WasmBinaryI32({op: "rem_s_i32", boolean: false})),
      ),
      (
        "rem_u_i32",
        WasmPrim2(WasmBinaryI32({op: "rem_u_i32", boolean: false})),
      ),
      (
        "and_i32",
        WasmPrim2(WasmBinaryI32({op: "and_i32", boolean: false})),
      ),
      ("or_i32", WasmPrim2(WasmBinaryI32({op: "or_i32", boolean: false}))),
      (
        "xor_i32",
        WasmPrim2(WasmBinaryI32({op: "xor_i32", boolean: false})),
      ),
      (
        "shl_i32",
        WasmPrim2(WasmBinaryI32({op: "shl_i32", boolean: false})),
      ),
      (
        "shr_u_i32",
        WasmPrim2(WasmBinaryI32({op: "shr_u_i32", boolean: false})),
      ),
      (
        "shr_s_i32",
        WasmPrim2(WasmBinaryI32({op: "shr_s_i32", boolean: false})),
      ),
      (
        "rot_l_i32",
        WasmPrim2(WasmBinaryI32({op: "rot_l_i32", boolean: false})),
      ),
      (
        "rot_r_i32",
        WasmPrim2(WasmBinaryI32({op: "rot_r_i32", boolean: false})),
      ),
      ("eq_i32", WasmPrim2(WasmBinaryI32({op: "eq_i32", boolean: true}))),
      ("ne_i32", WasmPrim2(WasmBinaryI32({op: "ne_i32", boolean: true}))),
      (
        "lt_s_i32",
        WasmPrim2(WasmBinaryI32({op: "lt_s_i32", boolean: true})),
      ),
      (
        "lt_u_i32",
        WasmPrim2(WasmBinaryI32({op: "lt_u_i32", boolean: true})),
      ),
      (
        "le_s_i32",
        WasmPrim2(WasmBinaryI32({op: "le_s_i32", boolean: true})),
      ),
      (
        "le_u_i32",
        WasmPrim2(WasmBinaryI32({op: "le_u_i32", boolean: true})),
      ),
      (
        "gt_s_i32",
        WasmPrim2(WasmBinaryI32({op: "gt_s_i32", boolean: true})),
      ),
      (
        "gt_u_i32",
        WasmPrim2(WasmBinaryI32({op: "gt_u_i32", boolean: true})),
      ),
      (
        "ge_s_i32",
        WasmPrim2(WasmBinaryI32({op: "ge_s_i32", boolean: true})),
      ),
      (
        "ge_u_i32",
        WasmPrim2(WasmBinaryI32({op: "ge_u_i32", boolean: true})),
      ),
    ]),
  );

let get_primitive = id => StringHash.find_opt(primitive_map, id);

let analyze = ({imports, body, analyses}) => {
  inline_wasm_tbl := Ident.empty;
  mod_has_inlineable_wasm := false;
  let process_import = ({imp_use_id, imp_desc}) => {
    switch (imp_desc) {
    | GrainValue("wasm", name) =>
      mod_has_inlineable_wasm := true;
      switch (get_primitive(name)) {
      | Some(prim) => set_inlineable_wasm(imp_use_id, prim)
      | None => ()
      };
    | GrainValue(_)
    | WasmFunction(_)
    | JSFunction(_) => ()
    };
  };
  List.iter(process_import, imports);
  analyses := [InlineableWasmTable(inline_wasm_tbl^), ...analyses^];
};
