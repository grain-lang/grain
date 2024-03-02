open Anftree;
open Anf_iterator;
open Grain_typed;

let manual_call_tbl: ref(Ident.tbl(unit)) = (
  ref(Ident.empty): ref(Ident.tbl(unit))
);

let set_manual_call = id =>
  manual_call_tbl := Ident.add(id, (), manual_call_tbl^);

let is_manual_memory_management_call = id =>
  Option.is_some(Ident.find_same_opt(id, manual_call_tbl^));

let mod_has_manual_memory_management = ref(false);

let analyze = ({imports, body, analyses}) => {
  manual_call_tbl := Ident.empty;
  mod_has_manual_memory_management := false;
  let process_import = ({imp_use_id, imp_desc}) => {
    switch (imp_desc) {
    | GrainValue("runtime/unsafe/memory.gr", "incRef" | "decRef") =>
      mod_has_manual_memory_management := true;
      set_manual_call(imp_use_id);
    | GrainValue(_)
    | WasmFunction(_)
    | WasmValue(_) => ()
    };
  };
  let root_gc_disabled =
    Grain_utils.Config.with_config(Grain_utils.Config.root_config^, () =>
      Grain_utils.Config.no_gc^
    );
  if (root_gc_disabled) {
    List.iter(process_import, imports.specs);
  };
};
