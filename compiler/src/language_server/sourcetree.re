open Grain_parsing;
open Grain_typed;
open Grain_diagnostics.Segment_tree;

/*
   This module implements a data structure used for efficient querying of
   Typedtree nodes given a location.

   The current Sourcetree implementation returns _all_ nodes at a given point,
   sorted by specificity. In the future this may change to return just the most
   specific node if performance is a concern.
 */

module type Sourcetree = {
  include SegmentTree with type point = Protocol.position;
  type node =
    | Value(Env.t, Types.type_expr, Location.t)
    | Type(Typedtree.core_type)
    | Pattern(Typedtree.pattern)
    | Declaration(Ident.t, Types.type_declaration, Location.t)
    | Module(Path.t, Types.module_declaration, Location.t);

  type sourcetree = t(node);

  let from_program: Typedtree.typed_program => t(node);
};

module Sourcetree: Sourcetree = {
  include Make({
    type t = Protocol.position;
    type segment = (Protocol.position, Protocol.position);

    let center = (x, y) => {
      Protocol.(
        if (x.line == y.line) {
          {line: x.line, character: (x.character + y.character) / 2};
        } else {
          {
            line: (x.line + y.line) / 2,
            // It's difficult to choose a character that represets the
            // "center" of two positions since we don't know how many
            // total characters are in the range. For simplicity, we
            // choose the character at the start of `x`.
            character: x.character,
          };
        }
      );
    };

    let compare = (x, y) => {
      Protocol.(
        if (x.line == y.line) {
          Stdlib.compare(x.character, y.character);
        } else {
          Stdlib.compare(x.line, y.line);
        }
      );
    };

    let compare_segments = ((x1, x2), (y1, y2)) => {
      Protocol.(
        if (x2.line - x1.line == y2.line - y1.line) {
          Stdlib.compare(
            x2.character - x1.character,
            y2.character - y1.character,
          );
        } else {
          Stdlib.compare(x2.line - x1.line, y2.line - y1.line);
        }
      );
    };
  });

  type node =
    | Value(Env.t, Types.type_expr, Location.t)
    | Type(Typedtree.core_type)
    | Pattern(Typedtree.pattern)
    | Declaration(Ident.t, Types.type_declaration, Location.t)
    | Module(Path.t, Types.module_declaration, Location.t);

  type sourcetree = t(node);

  let loc_to_interval = loc => {
    open Location;
    open Protocol;
    let pos_start = {
      line: loc.loc_start.pos_lnum - 1,
      character: loc.loc_start.pos_cnum - loc.loc_start.pos_bol,
    };
    let pos_end = {
      line: loc.loc_end.pos_lnum - 1,
      character: loc.loc_end.pos_cnum - loc.loc_end.pos_bol,
    };
    (pos_start, pos_end);
  };

  let from_program = program => {
    let segments = ref([]);
    open Typedtree;
    open Protocol;
    module Iterator =
      TypedtreeIter.MakeIterator({
        include TypedtreeIter.DefaultIteratorArgument;
        let process_value_description = (id, instance_ty, ty) => {
          // Never consider special idents when deciding to display generalized
          // types, i.e. always display instance types for lists
          Parsetree.(
            Identifier.(
              switch (id) {
              | {txt: IdentName({txt: "[]" | "[...]"})} => instance_ty
              | _ => ty
              }
            )
          );
        };
        let enter_expression = exp => {
          Parsetree.(
            Path.(
              switch (exp.exp_desc) {
              | TExpIdent(
                  PExternal(path, _),
                  {txt: IdentExternal(IdentName({loc}), _)},
                  desc,
                ) =>
                segments :=
                  [
                    (
                      loc_to_interval(loc),
                      Module(
                        path,
                        Env.find_module(path, None, exp.exp_env),
                        loc,
                      ),
                    ),
                    (
                      loc_to_interval(exp.exp_loc),
                      Value(exp.exp_env, desc.val_type, exp.exp_loc),
                    ),
                    ...segments^,
                  ]
              | TExpIdent(_, id, desc) =>
                segments :=
                  [
                    (
                      loc_to_interval(exp.exp_loc),
                      Value(
                        exp.exp_env,
                        process_value_description(
                          id,
                          exp.exp_type,
                          desc.val_type,
                        ),
                        exp.exp_loc,
                      ),
                    ),
                    ...segments^,
                  ]
              | TExpUse(module_, items) =>
                segments :=
                  [
                    (
                      loc_to_interval(module_.loc),
                      Module(
                        module_.txt,
                        Env.find_module(module_.txt, None, exp.exp_env),
                        module_.loc,
                      ),
                    ),
                    (
                      loc_to_interval(exp.exp_loc),
                      Value(exp.exp_env, exp.exp_type, exp.exp_loc),
                    ),
                    ...switch (items) {
                       | TUseAll => []
                       | TUseItems(items) =>
                         List.map(
                           item => {
                             switch (item) {
                             | Types.TUseType({name, declaration, loc}) => (
                                 loc_to_interval(loc),
                                 Declaration(
                                   Ident.create(name),
                                   declaration,
                                   loc,
                                 ),
                               )
                             | TUseModule({name, declaration, loc}) => (
                                 loc_to_interval(loc),
                                 Module(
                                   PIdent(Ident.create(name)),
                                   declaration,
                                   loc,
                                 ),
                               )
                             | TUseValue({value, loc}) => (
                                 loc_to_interval(loc),
                                 Value(exp.exp_env, value.val_type, loc),
                               )
                             }
                           },
                           items,
                         )
                       },
                  ]
                  @ segments^
              | _ =>
                segments :=
                  [
                    (
                      loc_to_interval(exp.exp_loc),
                      Value(exp.exp_env, exp.exp_type, exp.exp_loc),
                    ),
                    ...segments^,
                  ]
              }
            )
          );
        };
        let enter_pattern = pat => {
          segments :=
            [(loc_to_interval(pat.pat_loc), Pattern(pat)), ...segments^];
        };
        let enter_core_type = ty => {
          segments :=
            [(loc_to_interval(ty.ctyp_loc), Type(ty)), ...segments^];
        };
        let enter_data_declaration = decl => {
          segments :=
            [
              (
                loc_to_interval(decl.data_loc),
                Declaration(decl.data_id, decl.data_type, decl.data_loc),
              ),
              ...segments^,
            ];
        };
      });
    Iterator.iter_typed_program(program);
    create(segments^);
  };
};
