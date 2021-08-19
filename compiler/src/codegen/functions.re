open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_iterator;
open Grain_typed;

// Map of known function identifiers to number of times the identifier is
// used in a first-class way. If a function is not used in a first-class way,
// i.e. it has a count of 0 in this table, it does not need be included in a
// WebAssembly table.
let known_functions = Ident_tbl.create(50);
let clear_known_functions = () => Ident_tbl.clear(known_functions);
let register_function = id => Ident_tbl.add(known_functions, id, 0);

let record_usage = id => {
  switch (Ident_tbl.find_opt(known_functions, id)) {
  | Some(n) => Ident_tbl.replace(known_functions, id, n + 1)
  | None => ()
  };
};

// Since the iterator will mark every occurence of an identifier as used, this
// function performs the inverse for direct function calls. e.g. in `f(x)`,
// the identifier `f` was used (+1) but we'll call `f` directly (-1).
let record_non_first_class_usage = id => {
  switch (Ident_tbl.find_opt(known_functions, id)) {
  | Some(n) => Ident_tbl.replace(known_functions, id, n - 1)
  | None => ()
  };
};

let known_function = f =>
  switch (f.imm_desc) {
  | ImmId(id) =>
    if (Ident_tbl.mem(known_functions, id)) {
      Some(Ident.unique_name(id));
    } else {
      None;
    }
  | ImmConst(_)
  | ImmTrap => failwith("Impossible: function application of non-function")
  };

let has_first_class_usage = f =>
  switch (f.imm_desc) {
  | ImmId(id) =>
    switch (Ident_tbl.find_opt(known_functions, id)) {
    | Some(n) when n == 0 => false
    | _ => true
    }
  | ImmConst(_)
  | ImmTrap => failwith("Impossible: function application of non-function")
  };

module Preprocess: Grain_middle_end.Anf_iterator.IterArgument = {
  include Grain_middle_end.Anf_iterator.DefaultIterArgument;

  let enter_comp_expression = (~id=?, {comp_desc: desc}) =>
    switch (desc) {
    | CLambda(_) =>
      switch (id) {
      | Some(id) => register_function(id)
      | _ => ()
      }
    | CApp(({imm_desc: ImmId(id)}, _), _, _)
        when Ident_tbl.mem(known_functions, id) =>
      record_non_first_class_usage(id)
    | _ => ()
    };

  let enter_imm_expression = ({imm_desc: desc}) =>
    switch (desc) {
    | ImmId(id) when Ident_tbl.mem(known_functions, id) => record_usage(id)
    | _ => ()
    };
};

let process_import =
  fun
  | {imp_desc: GrainFunction(_), imp_use_id} => register_function(imp_use_id)
  | _ => ();

module Preprocessor = Grain_middle_end.Anf_iterator.MakeIter(Preprocess);

let preprocess = anfprog => {
  clear_known_functions();
  List.iter(process_import, anfprog.imports);
  Preprocessor.iter_anf_program(anfprog);
};
