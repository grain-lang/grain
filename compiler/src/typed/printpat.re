/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Values as patterns pretty printer */
open Grain_parsing;
open Asttypes;
open Typedtree;
open Types;
open Format;

let is_cons =
  fun
  | {cstr_name: "::"} => true
  | _ => false;

let pretty_const = c =>
  switch (c) {
  | Const_int(i) => Printf.sprintf("%d", i)
  | Const_string(s) => Printf.sprintf("%S", s)
  | Const_float(f) => Printf.sprintf("%s", f)
  | Const_int32(i) => Printf.sprintf("%ldl", i)
  | Const_int64(i) => Printf.sprintf("%LdL", i)
  | Const_bool(true) => "true"
  | Const_bool(false) => "false"
  | Const_void => "void"
  };

let rec pretty_val = (ppf, v) =>
  switch (v.pat_extra) {
  | [(cstr, _loc), ...rem] =>
    switch (cstr) {
    | TPatConstraint(_) =>
      fprintf(ppf, "@[(%a : _)@]", pretty_val, {...v, pat_extra: rem})
    }
  | [] =>
    switch (v.pat_desc) {
    | TPatAny => fprintf(ppf, "_")
    | [@implicit_arity] TPatVar(x, _) => fprintf(ppf, "%s", Ident.name(x))
    | TPatTuple(vs) => fprintf(ppf, "@[(%a)@]", pretty_vals(","), vs)
    | [@implicit_arity] TPatRecord(lvs, c) =>
      let filtered_lvs =
        List.filter(
          fun
          | (_, _, {pat_desc: TPatAny}) => false /* do not show lbl=_ */
          | _ => true,
          lvs,
        );
      switch (filtered_lvs) {
      | [] => fprintf(ppf, "_")
      | [(_, lbl, _), ...q] =>
        let elision_mark = ppf =>
          /* we assume that there are no label repetitions here */
          if (Array.length(lbl.lbl_all) > 1 + List.length(q)) {
            fprintf(ppf, ";@ _@ ");
          } else {
            ();
          };
        fprintf(ppf, "@[{%a%t}@]", pretty_lvals, filtered_lvs, elision_mark);
      };
    | TPatConstant(c) => fprintf(ppf, "%s", pretty_const(c))
    | [@implicit_arity] TPatConstruct({txt: id}, _, args) =>
      fprintf(
        ppf,
        "@[%s(%a)@]",
        Identifier.string_of_ident(id),
        pretty_vals(","),
        args,
      )
    | [@implicit_arity] TPatAlias(v, x, _) =>
      fprintf(ppf, "@[(%a@ as %a)@]", pretty_val, v, Ident.print, x)
    | [@implicit_arity] TPatOr(v, w) =>
      fprintf(ppf, "@[(%a|@,%a)@]", pretty_or, v, pretty_or, w)
    }
  }

and pretty_car = (ppf, v) =>
  switch (v.pat_desc) {
  | _ => pretty_val(ppf, v)
  }

and pretty_cdr = (ppf, v) =>
  switch (v.pat_desc) {
  | _ => pretty_val(ppf, v)
  }

and pretty_arg = (ppf, v) =>
  switch (v.pat_desc) {
  | _ => pretty_val(ppf, v)
  }

and pretty_or = (ppf, v) =>
  switch (v.pat_desc) {
  | [@implicit_arity] TPatOr(v, w) =>
    fprintf(ppf, "%a|@,%a", pretty_or, v, pretty_or, w)
  | _ => pretty_val(ppf, v)
  }

and pretty_vals = (sep, ppf) =>
  fun
  | [] => ()
  | [v] => pretty_val(ppf, v)
  | [v, ...vs] =>
    fprintf(ppf, "%a%s@ %a", pretty_val, v, sep, pretty_vals(sep), vs)

and pretty_lvals = ppf =>
  fun
  | [] => ()
  | [(_, lbl, v)] => fprintf(ppf, "%s=%a", lbl.lbl_name, pretty_val, v)
  | [(_, lbl, v), ...rest] =>
    fprintf(
      ppf,
      "%s=%a;@ %a",
      lbl.lbl_name,
      pretty_val,
      v,
      pretty_lvals,
      rest,
    );

let top_pretty = (ppf, v) => fprintf(ppf, "@[%a@]@?", pretty_val, v);

let pretty_pat = p => {
  top_pretty(Format.str_formatter, p);
  prerr_string(Format.flush_str_formatter());
};

type matrix = list(list(pattern));

let pretty_line = fmt =>
  List.iter(p => {
    Format.fprintf(fmt, " <");
    top_pretty(fmt, p);
    Format.fprintf(fmt, ">");
  });

let pretty_matrix = (fmt, pss: matrix) => {
  Format.fprintf(fmt, "begin matrix\n");
  List.iter(
    ps => {
      pretty_line(fmt, ps);
      Format.fprintf(fmt, "\n");
    },
    pss,
  );
  Format.fprintf(fmt, "end matrix\n%!");
};
