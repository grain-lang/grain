open Grain_parsing;
open Grain_typed;

/*
   This module implements a data structure used for efficient querying of
   Typedtree nodes given a location. The underlying data type is a Segment Tree,
   a balanced binary tree which gives us O(log n) lookups of matching intervals.
   You can learn more about Segment Trees here:

   https://en.wikipedia.org/wiki/Segment_tree

   The SegmentTree implementation is generic for future use cases where we might
   need one, though for now as this is the only use case it's kept here.

   The current Sourcetree implementation returns _all_ nodes at a given point,
   sorted by specificity. In the future this may change to return just the most
   specific node if performance is a concern.
 */

module type OrderableSegment = {
  include Set.OrderedType;

  type segment = (t, t);
  let center: (t, t) => t;
  let compare_segments: (segment, segment) => int;
};

module type SegmentTree = {
  type t('a);
  type point;
  type segment = (point, point);

  let create: list((segment, 'a)) => t('a);
  let query: (point, t('a)) => list('a);
};

module Make = (Ord: OrderableSegment) => {
  type point = Ord.t;
  type segment = Ord.segment;

  type t('a) =
    | Leaf
    | Node({
        center: point,
        data: list((segment, 'a)),
        left: t('a),
        right: t('a),
      });

  let center = segments => {
    let rec center = (left_acc, right_acc, segments) => {
      switch (segments) {
      | [((first, last), _), ...rest] =>
        let left =
          if (Ord.compare(first, left_acc) < 0) {
            first;
          } else {
            left_acc;
          };
        let right =
          if (Ord.compare(last, right_acc) > 0) {
            last;
          } else {
            right_acc;
          };
        center(left, right, rest);
      | [] => Ord.center(left_acc, right_acc)
      };
    };
    switch (segments) {
    | [((first, last), _), ...rest] => center(first, last, rest)
    | [] => failwith("no segments provided")
    };
  };

  let rec create = segments => {
    switch (segments) {
    | [] => Leaf
    | segments =>
      let pivot = center(segments);
      let rec split = (left_acc, center_acc, right_acc, segments) => {
        switch (segments) {
        | [] => (left_acc, center_acc, right_acc)
        | [((first, last), _) as k, ...rest]
            when Ord.compare(last, pivot) < 0 =>
          split([k, ...left_acc], center_acc, right_acc, rest)
        | [((first, last), _) as k, ...rest]
            when Ord.compare(first, pivot) > 0 =>
          split(left_acc, center_acc, [k, ...right_acc], rest)
        | [((first, last), _) as k, ...rest] =>
          split(left_acc, [k, ...center_acc], right_acc, rest)
        };
      };
      let (left, center, right) = split([], [], [], segments);
      Node({
        center: pivot,
        data: center,
        left: create(left),
        right: create(right),
      });
    };
  };

  let in_segment = ((low, high), candidate) => {
    Ord.compare(low, candidate) <= 0 && Ord.compare(candidate, high) < 0;
  };

  let query = (point, tree) => {
    let rec query = (acc, tree) => {
      switch (tree) {
      | Leaf => acc
      | Node({center, data, left, right}) =>
        let results =
          List.filter(
            ((segment, _)) => {in_segment(segment, point)},
            data,
          );
        if (Ord.compare(point, center) < 0) {
          query(results @ acc, left);
        } else {
          query(results @ acc, right);
        };
      };
    };
    query([], tree);
  };

  let query = (x, tree) => {
    query(x, tree)
    |> List.sort(((segment_x, _), (segment_y, _)) =>
         Ord.compare_segments(segment_x, segment_y)
       )
    |> List.map(((_, payload)) => payload);
  };
};

module type Sourcetree = {
  include SegmentTree with type point = Protocol.position;
  type node =
    | Expression(Typedtree.expression, option(Types.value_description))
    | Type(Typedtree.core_type)
    | Pattern(Typedtree.pattern)
    | Declaration(Typedtree.data_declaration)
    | Module(Path.t, Location.t);

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
    | Expression(Typedtree.expression, option(Types.value_description))
    | Type(Typedtree.core_type)
    | Pattern(Typedtree.pattern)
    | Declaration(Typedtree.data_declaration)
    | Module(Path.t, Location.t);

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
        let process_value_description = (id, desc) => {
          // Never consider special idents when deciding to display generalized
          // types, i.e. always display instance types for lists
          Parsetree.(
            Identifier.(
              switch (id) {
              | {txt: IdentName({txt: "[]" | "[...]"})} => None
              | _ => Some(desc)
              }
            )
          );
        };
        let enter_expression = exp => {
          Parsetree.(
            Path.(
              switch (exp.exp_desc) {
              | TExpIdent(
                  PExternal(path, _, _),
                  {txt: IdentExternal(IdentName({loc}), _)},
                  desc,
                ) =>
                segments :=
                  [
                    (loc_to_interval(loc), Module(path, loc)),
                    (
                      loc_to_interval(exp.exp_loc),
                      Expression(exp, Some(desc)),
                    ),
                    ...segments^,
                  ]
              | TExpIdent(_, id, desc) =>
                segments :=
                  [
                    (
                      loc_to_interval(exp.exp_loc),
                      Expression(exp, process_value_description(id, desc)),
                    ),
                    ...segments^,
                  ]
              | _ =>
                segments :=
                  [
                    (loc_to_interval(exp.exp_loc), Expression(exp, None)),
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
              (loc_to_interval(decl.data_loc), Declaration(decl)),
              ...segments^,
            ];
        };
      });
    Iterator.iter_typed_program(program);
    create(segments^);
  };
};
