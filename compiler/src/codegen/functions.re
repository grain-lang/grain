open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_iterator;
open Grain_typed;

let known_functions = Ident_tbl.create(50);
let register_function = id => Ident_tbl.add(known_functions, id, ());
let clear_known_functions = () => Ident_tbl.clear(known_functions);
let is_known = f =>
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

module Preprocess: Grain_middle_end.Anf_iterator.IterArgument = {
  include Grain_middle_end.Anf_iterator.DefaultIterArgument;

  let enter_comp_expression = (~id=?, {comp_desc: desc}) =>
    switch (desc) {
    | CLambda(_) =>
      switch (id) {
      | Some(id) => register_function(id)
      | None => ()
      }
    | _ => ()
    };
};

module Preprocessor = Grain_middle_end.Anf_iterator.MakeIter(Preprocess);

let preprocess = anfprog => {
  clear_known_functions();
  Preprocessor.iter_anf_program(anfprog);
};
