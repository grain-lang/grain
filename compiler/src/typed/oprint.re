/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                   Projet Cristal, INRIA Rocquencourt                   */
/*                                                                        */
/*   Copyright 2002 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Format;
open Outcometree;

exception Ellipsis;

let cautious = (f, ppf, arg) =>
  try(f(ppf, arg)) {
  | Ellipsis => fprintf(ppf, "...")
  };

let print_lident = ppf =>
  fun
  | "::" => pp_print_string(ppf, "(::)")
  | s => pp_print_string(ppf, s);

let rec print_ident = ppf =>
  fun
  | Oide_ident(s) => print_lident(ppf, s)
  | [@implicit_arity] Oide_dot(id, s) => {
      print_ident(ppf, id);
      pp_print_char(ppf, '.');
      print_lident(ppf, s);
    };

let parenthesized_ident = name =>
  List.mem(name, ["or", "mod", "land", "lor", "lxor", "lsl", "lsr", "asr"])
  || (
    switch (name.[0]) {
    | 'a'..'z'
    | 'A'..'Z'
    | '\223'..'\246'
    | '\248'..'\255'
    | '_' => false
    | _ => true
    }
  );

let value_ident = (ppf, name) =>
  if (parenthesized_ident(name)) {
    fprintf(ppf, "( %s )", name);
  } else {
    pp_print_string(ppf, name);
  };

/* Values */

let valid_float_lexeme = s => {
  let l = String.length(s);
  let rec loop = i =>
    if (i >= l) {
      s ++ ".";
    } else {
      switch (s.[i]) {
      | '0'..'9'
      | '-' => loop(i + 1)
      | _ => s
      };
    };
  loop(0);
};

let float_repres = f =>
  switch (classify_float(f)) {
  | FP_nan => "nan"
  | FP_infinite =>
    if (f < 0.0) {
      "neg_infinity";
    } else {
      "infinity";
    }
  | _ =>
    let float_val = {
      let s1 = Printf.sprintf("%.12g", f);
      if (f == float_of_string(s1)) {
        s1;
      } else {
        let s2 = Printf.sprintf("%.15g", f);
        if (f == float_of_string(s2)) {
          s2;
        } else {
          Printf.sprintf("%.18g", f);
        };
      };
    };
    valid_float_lexeme(float_val);
  };

let parenthesize_if_neg = (ppf, fmt, v, isneg) => {
  if (isneg) {
    pp_print_char(ppf, '(');
  };
  fprintf(ppf, fmt, v);
  if (isneg) {
    pp_print_char(ppf, ')');
  };
};

let escape_string = s => {
  /* Escape only C0 control characters (bytes <= 0x1F), DEL(0x7F), '\\' and '"' */
  let n = ref(0);
  for (i in 0 to String.length(s) - 1) {
    n :=
      n^
      + (
        switch (String.unsafe_get(s, i)) {
        | '"'
        | '\\'
        | '\n'
        | '\t'
        | '\r'
        | '\b' => 2
        | '\000'..'\031'
        | '\127' => 4
        | _ => 1
        }
      );
  };
  if (n^ == String.length(s)) {
    s;
  } else {
    let s' = Bytes.create(n^);
    n := 0;
    for (i in 0 to String.length(s) - 1) {
      switch (String.unsafe_get(s, i)) {
      | ('"' | '\\') as c =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, c);
      | '\n' =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, 'n');
      | '\t' =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, 't');
      | '\r' =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, 'r');
      | '\b' =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, 'b');
      | ('\000'..'\031' | '\127') as c =>
        let a = Char.code(c);
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, Char.chr(48 + a / 100));
        incr(n);
        Bytes.unsafe_set(s', n^, Char.chr(48 + a / 10 mod 10));
        incr(n);
        Bytes.unsafe_set(s', n^, Char.chr(48 + a mod 10));
      | c => Bytes.unsafe_set(s', n^, c)
      };
      incr(n);
    };
    Bytes.to_string(s');
  };
};

let print_out_string = (ppf, s) => {
  let not_escaped =
    /* let the user dynamically choose if strings should be escaped: */
    switch (Sys.getenv_opt("OCAMLTOP_UTF_8")) {
    | None => true
    | Some(x) =>
      switch (bool_of_string_opt(x)) {
      | None => true
      | Some(f) => f
      }
    };
  if (not_escaped) {
    fprintf(ppf, "\"%s\"", escape_string(s));
  } else {
    fprintf(ppf, "%S", s);
  };
};

let print_out_value = (ppf, tree) => {
  let rec print_tree_1 = ppf =>
    fun
    | [@implicit_arity] Oval_constr(name, [param]) =>
      fprintf(
        ppf,
        "@[<1>%a@ %a@]",
        print_ident,
        name,
        print_constr_param,
        param,
      )
    | [@implicit_arity] Oval_constr(name, [_, ..._] as params) =>
      fprintf(
        ppf,
        "@[<1>%a@ (%a)@]",
        print_ident,
        name,
        print_tree_list(print_tree_1, ","),
        params,
      )
    | [@implicit_arity] Oval_variant(name, Some(param)) =>
      fprintf(ppf, "@[<2>`%s@ %a@]", name, print_constr_param, param)
    | tree => print_simple_tree(ppf, tree)
  and print_constr_param = ppf =>
    fun
    | Oval_int(i) => parenthesize_if_neg(ppf, "%i", i, i < 0)
    | Oval_int32(i) => parenthesize_if_neg(ppf, "%lil", i, i < 0l)
    | Oval_int64(i) => parenthesize_if_neg(ppf, "%LiL", i, i < 0L)
    | Oval_nativeint(i) => parenthesize_if_neg(ppf, "%nin", i, i < 0n)
    | Oval_float(f) =>
      parenthesize_if_neg(ppf, "%s", float_repres(f), f < 0.0)
    | [@implicit_arity] Oval_string(_, _, Ostr_bytes) as tree => {
        pp_print_char(ppf, '(');
        print_simple_tree(ppf, tree);
        pp_print_char(ppf, ')');
      }
    | tree => print_simple_tree(ppf, tree)
  and print_simple_tree = ppf =>
    fun
    | Oval_int(i) => fprintf(ppf, "%i", i)
    | Oval_int32(i) => fprintf(ppf, "%lil", i)
    | Oval_int64(i) => fprintf(ppf, "%LiL", i)
    | Oval_nativeint(i) => fprintf(ppf, "%nin", i)
    | Oval_float(f) => pp_print_string(ppf, float_repres(f))
    | Oval_char(c) => fprintf(ppf, "%C", c)
    | [@implicit_arity] Oval_string(s, maxlen, kind) =>
      try({
        let len = String.length(s);
        let s =
          if (len > maxlen) {
            String.sub(s, 0, maxlen);
          } else {
            s;
          };
        switch (kind) {
        | Ostr_bytes => fprintf(ppf, "Bytes.of_string %S", s)
        | Ostr_string => print_out_string(ppf, s)
        };
        if (len > maxlen) {
          fprintf(ppf, "... (* string length %d; truncated *)", len);
        };
      }) {
      | Invalid_argument(_) /* "String.create" */ =>
        fprintf(ppf, "<huge string>")
      }
    | Oval_list(tl) =>
      fprintf(ppf, "@[<1>[%a]@]", print_tree_list(print_tree_1, ";"), tl)
    | Oval_array(tl) =>
      fprintf(ppf, "@[<2>[|%a|]@]", print_tree_list(print_tree_1, ";"), tl)
    | [@implicit_arity] Oval_constr(name, []) => print_ident(ppf, name)
    | [@implicit_arity] Oval_variant(name, None) => fprintf(ppf, "`%s", name)
    | Oval_stuff(s) => pp_print_string(ppf, s)
    | Oval_record(fel) =>
      fprintf(ppf, "@[<1>{%a}@]", cautious(print_fields(true)), fel)
    | Oval_ellipsis => raise(Ellipsis)
    | Oval_printer(f) => f(ppf)
    | Oval_tuple(tree_list) =>
      fprintf(
        ppf,
        "@[<1>(%a)@]",
        print_tree_list(print_tree_1, ","),
        tree_list,
      )
    | tree => fprintf(ppf, "@[<1>(%a)@]", cautious(print_tree_1), tree)
  and print_fields = (first, ppf) =>
    fun
    | [] => ()
    | [(name, tree), ...fields] => {
        if (!first) {
          fprintf(ppf, ";@ ");
        };
        fprintf(
          ppf,
          "@[<1>%a@ =@ %a@]",
          print_ident,
          name,
          cautious(print_tree_1),
          tree,
        );
        print_fields(false, ppf, fields);
      }
  and print_tree_list = (print_item, sep, ppf, tree_list) => {
    let rec print_list = (first, ppf) =>
      fun
      | [] => ()
      | [tree, ...tree_list] => {
          if (!first) {
            fprintf(ppf, "%s@ ", sep);
          };
          print_item(ppf, tree);
          print_list(false, ppf, tree_list);
        };

    cautious(print_list(true), ppf, tree_list);
  };

  cautious(print_tree_1, ppf, tree);
};

let out_value = ref(print_out_value);

/* Types */

let rec print_list_init = (pr, sep, ppf) =>
  fun
  | [] => ()
  | [a, ...l] => {
      sep(ppf);
      pr(ppf, a);
      print_list_init(pr, sep, ppf, l);
    };

let rec print_list = (pr, sep, ppf) =>
  fun
  | [] => ()
  | [a] => pr(ppf, a)
  | [a, ...l] => {
      pr(ppf, a);
      sep(ppf);
      print_list(pr, sep, ppf, l);
    };

let pr_present =
  print_list(
    (ppf, s) => fprintf(ppf, "`%s", s),
    ppf => fprintf(ppf, "@ "),
  );

let pr_vars =
  print_list(
    (ppf, s) => fprintf(ppf, "'%s", s),
    ppf => fprintf(ppf, "@ "),
  );

let rec print_out_type = ppf =>
  fun
  | [@implicit_arity] Otyp_alias(ty, s) =>
    fprintf(ppf, "@[%a@ as '%s@]", print_out_type, ty, s)
  | [@implicit_arity] Otyp_poly(sl, ty) =>
    fprintf(ppf, "@[<hov 2>%a.@ %a@]", pr_vars, sl, print_out_type, ty)
  | ty => print_out_type_1(ppf, ty)

and print_out_type_1 = ppf =>
  fun
  | [@implicit_arity] Otyp_arrow(ty1, ty2) => {
      pp_open_box(ppf, 0);
      fprintf(ppf, "@[<0>%a@]", print_typlist(print_out_type_2, ", "), ty1);
      pp_print_string(ppf, " ->");
      pp_print_space(ppf, ());
      print_out_type_1(ppf, ty2);
      pp_close_box(ppf, ());
    }
  | ty => print_out_type_2(ppf, ty)
and print_out_type_2 = ppf =>
  fun
  | Otyp_tuple(tyl) =>
    fprintf(
      ppf,
      "@[<0>%a@]",
      print_typlist(print_simple_out_type, " *"),
      tyl,
    )
  | ty => print_simple_out_type(ppf, ty)
and print_simple_out_type = ppf =>
  fun
  | [@implicit_arity] Otyp_class(ng, id, tyl) =>
    fprintf(
      ppf,
      "@[%a%s#%a@]",
      print_typargs,
      tyl,
      if (ng) {"_"} else {""},
      print_ident,
      id,
    )
  | [@implicit_arity] Otyp_constr(id, tyl) => {
      pp_open_box(ppf, 0);
      print_ident(ppf, id);
      print_typargs(ppf, tyl);
      pp_close_box(ppf, ());
    }
  | [@implicit_arity] Otyp_object(fields, rest) =>
    fprintf(ppf, "@[<2>< %a >@]", print_fields(rest), fields)
  | Otyp_stuff(s) => pp_print_string(ppf, s)
  | [@implicit_arity] Otyp_var(ng, s) =>
    fprintf(ppf, "'%s%s", if (ng) {"_"} else {""}, s)
  | [@implicit_arity] Otyp_variant(non_gen, row_fields, closed, tags) => {
      let print_present = ppf => (
        fun
        | None
        | Some([]) => ()
        | Some(l) => fprintf(ppf, "@;<1 -2>> @[<hov>%a@]", pr_present, l)
      );

      let print_fields = ppf => (
        fun
        | Ovar_fields(fields) =>
          print_list(
            print_row_field,
            ppf => fprintf(ppf, "@;<1 -2>| "),
            ppf,
            fields,
          )
        | Ovar_typ(typ) => print_simple_out_type(ppf, typ)
      );

      fprintf(
        ppf,
        "%s[%s@[<hv>@[<hv>%a@]%a ]@]",
        if (non_gen) {"_"} else {""},
        if (closed) {
          if (tags == None) {
            " ";
          } else {
            "< ";
          };
        } else if (tags == None) {
          "> ";
        } else {
          "? ";
        },
        print_fields,
        row_fields,
        print_present,
        tags,
      );
    }
  | (Otyp_alias(_) | Otyp_poly(_) | Otyp_arrow(_) | Otyp_tuple(_)) as ty => {
      pp_open_box(ppf, 1);
      pp_print_char(ppf, '(');
      print_out_type(ppf, ty);
      pp_print_char(ppf, ')');
      pp_close_box(ppf, ());
    }
  | Otyp_abstract
  | Otyp_open
  | Otyp_sum(_)
  | [@implicit_arity] Otyp_manifest(_, _) => ()
  | Otyp_record(lbls) => print_record_decl(ppf, lbls)
  | [@implicit_arity] Otyp_module(p, n, tyl) => {
      fprintf(ppf, "@[<1>(module %s", p);
      let first = ref(true);
      List.iter2(
        (s, t) => {
          let sep =
            if (first^) {
              first := false;
              "with";
            } else {
              "and";
            };
          fprintf(ppf, " %s type %s = %a", sep, s, print_out_type, t);
        },
        n,
        tyl,
      );
      fprintf(ppf, ")@]");
    }
  | [@implicit_arity] Otyp_attribute(t, attr) =>
    fprintf(ppf, "@[<1>(%a [@@%s])@]", print_out_type, t, attr.oattr_name)
and print_record_decl = (ppf, lbls) =>
  fprintf(
    ppf,
    "{%a@;<1 -2>}",
    print_list_init(print_out_label, ppf => fprintf(ppf, "@ ")),
    lbls,
  )
and print_fields = (rest, ppf) =>
  fun
  | [] =>
    switch (rest) {
    | Some(non_gen) => fprintf(ppf, "%s..", if (non_gen) {"_"} else {""})
    | None => ()
    }
  | [(s, t)] => {
      fprintf(ppf, "%s : %a", s, print_out_type, t);
      switch (rest) {
      | Some(_) => fprintf(ppf, ";@ ")
      | None => ()
      };
      print_fields(rest, ppf, []);
    }
  | [(s, t), ...l] =>
    fprintf(ppf, "%s : %a;@ %a", s, print_out_type, t, print_fields(rest), l)
and print_row_field = (ppf, (l, opt_amp, tyl)) => {
  let pr_of = ppf =>
    if (opt_amp) {
      fprintf(ppf, " of@ &@ ");
    } else if (tyl != []) {
      fprintf(ppf, " of@ ");
    } else {
      fprintf(ppf, "");
    };

  fprintf(
    ppf,
    "@[<hv 2>`%s%t%a@]",
    l,
    pr_of,
    print_typlist(print_out_type, " &"),
    tyl,
  );
}
and print_typlist = (print_elem, sep, ppf) =>
  fun
  | [] => ()
  | [ty] => print_elem(ppf, ty)
  | [ty, ...tyl] => {
      print_elem(ppf, ty);
      pp_print_string(ppf, sep);
      pp_print_space(ppf, ());
      print_typlist(print_elem, sep, ppf, tyl);
    }
and print_typargs = ppf =>
  fun
  | [] => ()
  | [ty1] => {
      print_simple_out_type(ppf, ty1);
      pp_print_space(ppf, ());
    }
  | tyl => {
      pp_open_box(ppf, 1);
      pp_print_char(ppf, '<');
      print_typlist(print_out_type, ",", ppf, tyl);
      pp_print_char(ppf, '>');
      pp_close_box(ppf, ());
      pp_print_space(ppf, ());
    }
and print_out_label = (ppf, (name, mut, arg)) =>
  fprintf(
    ppf,
    "@[<2>%s%s :@ %a@];",
    if (mut) {"mutable "} else {""},
    name,
    print_out_type,
    arg,
  );

let out_type = ref(print_out_type);

let type_parameter = (ppf, (ty, (co, cn))) =>
  fprintf(
    ppf,
    "%s%s",
    if (!cn) {
      "+";
    } else if (!co) {
      "-";
    } else {
      "";
    },
    if (ty == "_") {
      ty;
    } else {
      "'" ++ ty;
    },
  );

/* Signature */

let out_module_type = ref(_ => failwith("Oprint.out_module_type"));
let out_sig_item = ref(_ => failwith("Oprint.out_sig_item"));
let out_signature = ref(_ => failwith("Oprint.out_signature"));
let out_type_extension = ref(_ => failwith("Oprint.out_type_extension"));

let rec print_out_functor = (funct, ppf, m) =>
  if (funct) {
    fprintf(ppf, "->@ %a", print_out_module_type, m);
  } else {
    print_out_module_type(ppf, m);
  }

and print_out_module_type = ppf =>
  fun
  | Omty_abstract => ()
  | Omty_ident(id) => fprintf(ppf, "%a", print_ident, id)
  | Omty_signature(sg) =>
    fprintf(ppf, "@[<hv 2>sig@ %a@;<1 -2>end@]", out_signature^, sg)
  | Omty_alias(id) => fprintf(ppf, "(module %a)", print_ident, id)
and print_out_signature = ppf =>
  fun
  | [] => ()
  | [item] => out_sig_item^(ppf, item)
  | [item, ...items] =>
    fprintf(ppf, "%a@ %a", out_sig_item^, item, print_out_signature, items)
and print_out_sig_item = ppf =>
  fun
  | [@implicit_arity] Osig_modtype(name, Omty_abstract) =>
    fprintf(ppf, "@[<2>module type %s@]", name)
  | [@implicit_arity] Osig_modtype(name, mty) =>
    fprintf(ppf, "@[<2>module type %s =@ %a@]", name, out_module_type^, mty)
  | [@implicit_arity] Osig_module(name, Omty_alias(id), _) =>
    fprintf(ppf, "@[<2>module %s =@ %a@]", name, print_ident, id)
  | [@implicit_arity] Osig_module(name, mty, rs) =>
    fprintf(
      ppf,
      "@[<2>%s %s :@ %a@]",
      switch (rs) {
      | Orec_not => "module"
      | Orec_first => "module rec"
      | Orec_next => "and"
      },
      name,
      out_module_type^,
      mty,
    )
  | [@implicit_arity] Osig_type(td, rs) =>
    print_out_type_decl(
      switch (rs) {
      | Orec_not => "type nonrec"
      | Orec_first => "type"
      | Orec_next => "and"
      },
      ppf,
      td,
    )
  | Osig_value(vd) => {
      let kwd =
        if (vd.oval_prims == []) {
          "val";
        } else {
          "external";
        };
      let pr_prims = ppf => (
        fun
        | [] => ()
        | [s, ...sl] => {
            fprintf(ppf, "@ = \"%s\"", s);
            List.iter(s => fprintf(ppf, "@ \"%s\"", s), sl);
          }
      );

      fprintf(
        ppf,
        "@[<2>%s %a :@ %a%a%a@]",
        kwd,
        value_ident,
        vd.oval_name,
        out_type^,
        vd.oval_type,
        pr_prims,
        vd.oval_prims,
        ppf => List.iter(a => fprintf(ppf, "@ [@@@@%s]", a.oattr_name)),
        vd.oval_attributes,
      );
    }
  | Osig_ellipsis => fprintf(ppf, "...")

and print_out_type_decl = (kwd, ppf, td) => {
  let print_constraints = ppf =>
    List.iter(
      ((ty1, ty2)) =>
        fprintf(
          ppf,
          "@ @[<2>constraint %a =@ %a@]",
          out_type^,
          ty1,
          out_type^,
          ty2,
        ),
      td.otype_cstrs,
    );

  let type_defined = ppf =>
    switch (td.otype_params) {
    | [] => pp_print_string(ppf, td.otype_name)
    | [param] =>
      fprintf(ppf, "@[%a@ %s@]", type_parameter, param, td.otype_name)
    | _ =>
      fprintf(
        ppf,
        "@[(@[%a)@]@ %s@]",
        print_list(type_parameter, ppf => fprintf(ppf, ",@ ")),
        td.otype_params,
        td.otype_name,
      )
    };

  let print_manifest = ppf =>
    fun
    | [@implicit_arity] Otyp_manifest(ty, _) =>
      fprintf(ppf, " =@ %a", out_type^, ty)
    | _ => ();

  let print_name_params = ppf =>
    fprintf(ppf, "%s %t%a", kwd, type_defined, print_manifest, td.otype_type);

  let ty =
    switch (td.otype_type) {
    | [@implicit_arity] Otyp_manifest(_, ty) => ty
    | _ => td.otype_type
    };

  let print_immediate = ppf =>
    if (td.otype_immediate) {
      fprintf(ppf, " [%@%@immediate]");
    } else {
      ();
    };

  let print_unboxed = ppf =>
    if (td.otype_unboxed) {
      fprintf(ppf, " [%@%@unboxed]");
    } else {
      ();
    };

  let print_out_tkind = ppf =>
    fun
    | Otyp_abstract => ()
    | Otyp_record(lbls) => fprintf(ppf, " = %a", print_record_decl, lbls)
    | Otyp_sum(constrs) =>
      fprintf(
        ppf,
        " =@;<1 2>%a",
        print_list(print_out_constr, ppf => fprintf(ppf, "@ | ")),
        constrs,
      )
    | Otyp_open => fprintf(ppf, " = ..")
    | ty => fprintf(ppf, " =@;<1 2>%a", out_type^, ty);

  fprintf(
    ppf,
    "@[<2>@[<hv 2>%t%a@]%t%t%t@]",
    print_name_params,
    print_out_tkind,
    ty,
    print_constraints,
    print_immediate,
    print_unboxed,
  );
}

and print_out_constr = (ppf, (name, tyl, ret_type_opt)) => {
  let name =
    switch (name) {
    | "::" => "(::)" /* #7200 */
    | s => s
    };

  switch (ret_type_opt) {
  | None =>
    switch (tyl) {
    | [] => pp_print_string(ppf, name)
    | _ =>
      fprintf(
        ppf,
        "@[<2>%s of@ %a@]",
        name,
        print_typlist(print_simple_out_type, " *"),
        tyl,
      )
    }
  | Some(ret_type) =>
    switch (tyl) {
    | [] =>
      fprintf(ppf, "@[<2>%s :@ %a@]", name, print_simple_out_type, ret_type)
    | _ =>
      fprintf(
        ppf,
        "@[<2>%s :@ %a -> %a@]",
        name,
        print_typlist(print_simple_out_type, " *"),
        tyl,
        print_simple_out_type,
        ret_type,
      )
    }
  };
};

let _ = out_module_type := print_out_module_type;
let _ = out_signature := print_out_signature;
let _ = out_sig_item := print_out_sig_item;

/* Phrases */

let print_out_exception = (ppf, exn, outv) =>
  switch (exn) {
  | Sys.Break => fprintf(ppf, "Interrupted.@.")
  | Out_of_memory => fprintf(ppf, "Out of memory during evaluation.@.")
  | Stack_overflow =>
    fprintf(ppf, "Stack overflow during evaluation (looping recursion?).@.")
  | _ => fprintf(ppf, "@[Exception:@ %a.@]@.", out_value^, outv)
  };

let rec print_items = ppf =>
  fun
  | [] => ()
  | [(tree, valopt), ...items] => {
      switch (valopt) {
      | Some(v) =>
        fprintf(ppf, "@[<2>%a =@ %a@]", out_sig_item^, tree, out_value^, v)
      | None => fprintf(ppf, "@[%a@]", out_sig_item^, tree)
      };
      if (items != []) {
        fprintf(ppf, "@ %a", print_items, items);
      };
    };

let print_out_phrase = ppf =>
  fun
  | [@implicit_arity] Ophr_eval(outv, ty) =>
    fprintf(ppf, "@[- : %a@ =@ %a@]@.", out_type^, ty, out_value^, outv)
  | Ophr_signature([]) => ()
  | Ophr_signature(items) => fprintf(ppf, "@[<v>%a@]@.", print_items, items)
  | [@implicit_arity] Ophr_exception(exn, outv) =>
    print_out_exception(ppf, exn, outv);

let out_phrase = ref(print_out_phrase);
