open Anftree;
open Grain_typed;
open Types;

module ExpressionHash = {
  type t = comp_expression_desc;
  let compare_lists = (l1, l2) =>
    List.length(l1) === List.length(l2)
    && List.fold_left2(
         (is_equal, {imm_desc: exp1}, {imm_desc: exp2}) =>
           is_equal && exp1 == exp2,
         true,
         l1,
         l2,
       );
  let equal = (i, j) =>
    switch (i, j) {
    | (CImmExpr({imm_desc: desc1}), CImmExpr({imm_desc: desc2})) =>
      desc1 == desc2
    | (
        [@implicit_arity] CPrim1(p1, {imm_desc: desc1}),
        [@implicit_arity] CPrim1(p2, {imm_desc: desc2}),
      ) =>
      desc1 == desc2 && p1 == p2
    | (
        [@implicit_arity] CPrim2(p1, {imm_desc: x1}, {imm_desc: y1}),
        [@implicit_arity] CPrim2(p2, {imm_desc: x2}, {imm_desc: y2}),
      ) =>
      p1 == p2 && x1 == x2 && y1 == y2
    | (CTuple(exps1), CTuple(exps2)) => compare_lists(exps1, exps2)
    | (
        [@implicit_arity] CAdt({imm_desc: ttag1}, {imm_desc: vtag1}, elts1),
        [@implicit_arity] CAdt({imm_desc: ttag2}, {imm_desc: vtag2}, elts2),
      ) =>
      ttag1 == ttag2 && vtag1 == vtag2 && compare_lists(elts1, elts2)
    | (
        [@implicit_arity] CApp({imm_desc: desc1}, args1),
        [@implicit_arity] CApp({imm_desc: desc2}, args2),
      ) =>
      desc1 == desc2 && compare_lists(args1, args2)
    | (
        [@implicit_arity] CAppBuiltin(_module1, name1, args1),
        [@implicit_arity] CAppBuiltin(_module2, name2, args2),
      ) =>
      _module1 == _module2 && name1 == name2 && compare_lists(args1, args2)
    | (CString(string1), CString(string2)) => string1 == string2
    | _ => false
    };
  let hash = i =>
    switch (i) {
    | CImmExpr({imm_desc: desc}) =>
      Hashtbl.hash("CImmExpr") lxor Hashtbl.hash(desc)
    | [@implicit_arity] CPrim1(p, {imm_desc: desc}) =>
      Hashtbl.hash("CPrim1") lxor Hashtbl.hash(p) lxor Hashtbl.hash(desc)
    | [@implicit_arity] CPrim2(p, {imm_desc: x}, {imm_desc: y}) =>
      Hashtbl.hash("CPrim2")
      lxor Hashtbl.hash(p)
      lxor Hashtbl.hash(x)
      lxor Hashtbl.hash(y)
    | CTuple(exps) =>
      Hashtbl.hash("CTuple")
      lxor List.fold_left(
             (hash, {imm_desc}) => hash lxor Hashtbl.hash(imm_desc),
             0,
             exps,
           )
    | [@implicit_arity] CAdt({imm_desc: ttag}, {imm_desc: vtag}, elts) =>
      Hashtbl.hash("CAdt")
      lxor Hashtbl.hash(ttag)
      lxor Hashtbl.hash(vtag)
      lxor List.fold_left(
             (hash, {imm_desc}) => hash lxor Hashtbl.hash(imm_desc),
             0,
             elts,
           )
    | [@implicit_arity] CApp({imm_desc: desc}, args) =>
      Hashtbl.hash("CApp")
      lxor List.fold_left(
             (hash, {imm_desc}) => hash lxor Hashtbl.hash(imm_desc),
             Hashtbl.hash(desc),
             args,
           )
    | [@implicit_arity] CAppBuiltin(_module, name, args) =>
      Hashtbl.hash("CAppBuiltin")
      lxor Hashtbl.hash(_module)
      lxor List.fold_left(
             (hash, {imm_desc}) => hash lxor Hashtbl.hash(imm_desc),
             Hashtbl.hash(name),
             args,
           )
    | CString(string) => Hashtbl.hash("CString") lxor Hashtbl.hash(string)
    | _ => Hashtbl.hash(i)
    };
};

module ExpressionHashtbl = Hashtbl.Make(ExpressionHash);

let rewrite_rules = ref(Ident.empty: Ident.tbl(Ident.t));
let known_expressions = ExpressionHashtbl.create(50);

let create_rewrite_rule = (a, b) =>
  rewrite_rules := Ident.add(a, b, rewrite_rules^);

let get_rewrite_rule = id =>
  try(Ident.find_same(id, rewrite_rules^)) {
  | Not_found => id
  };

let push_expression = (e, id) =>
  ExpressionHashtbl.add(known_expressions, e, id);

let pop_expression = e => ExpressionHashtbl.remove(known_expressions, e);

let get_known_expression = e =>
  ExpressionHashtbl.find_opt(known_expressions, e);

let get_comp_purity = c =>
  Option.value(~default=false) @@ Analyze_purity.comp_expression_purity(c);

module CSEArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let enter_anf_expression = ({anf_desc: desc} as a) => {
    switch (desc) {
    | [@implicit_arity] AELet(_, _, binds, _) =>
      List.iter(
        ((id, {comp_desc} as c)) =>
          switch (get_known_expression(comp_desc)) {
          | Some(known_id) => create_rewrite_rule(id, known_id)
          | None =>
            if (get_comp_purity(c)) {
              push_expression(comp_desc, id);
            }
          },
        binds,
      )
    | _ => ()
    };
    a;
  };

  let leave_imm_expression = ({imm_desc: desc} as i) =>
    switch (desc) {
    | ImmId(id) => {...i, imm_desc: ImmId(get_rewrite_rule(id))}
    | _ => i
    };

  let leave_anf_expression = ({anf_desc: desc} as a) => {
    switch (desc) {
    | [@implicit_arity] AELet(_, _, binds, _) =>
      List.iter(
        ((id, {comp_desc})) =>
          switch (get_known_expression(comp_desc)) {
          | Some(known_id) when id == known_id => pop_expression(comp_desc)
          | _ => ()
          },
        binds,
      )
    | _ => ()
    };
    a;
  };
};

module CSEMapper = Anf_mapper.MakeMap(CSEArg);

let optimize = anfprog => {
  /* Reset state */
  rewrite_rules := Ident.empty;
  ExpressionHashtbl.reset(known_expressions);
  CSEMapper.map_anf_program(anfprog);
};
