/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt          */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Module [Outcometree]: results displayed by the toplevel */

/* These types represent messages that the toplevel displays as normal
   results or errors. The real displaying is customisable using the hooks:
      [Toploop.print_out_value]
      [Toploop.print_out_type]
      [Toploop.print_out_sig_item]
      [Toploop.print_out_phrase] */

type out_ident =
  | Oide_dot(out_ident, string)
  | Oide_ident(string);

type out_string =
  | Ostr_string
  | Ostr_bytes;

type out_attribute = {oattr_name: string};

type out_value =
  | Oval_array(list(out_value))
  | Oval_char(char)
  | Oval_constr(out_ident, list(out_value))
  | Oval_ellipsis
  | Oval_float(float)
  | Oval_int(int)
  | Oval_int32(int32)
  | Oval_int64(int64)
  | Oval_nativeint(nativeint)
  | Oval_list(list(out_value))
  | Oval_printer(Format.formatter => unit)
  | Oval_record(list((out_ident, out_value)))
  | Oval_string(string, int, out_string) /* string, size-to-print, kind */
  | Oval_stuff(string)
  | Oval_tuple(list(out_value))
  | Oval_variant(string, option(out_value));

type out_type =
  | Otyp_abstract
  | Otyp_open
  | Otyp_alias(out_type, string)
  | Otyp_arrow(list(out_type), out_type)
  | Otyp_class(bool, out_ident, list(out_type))
  | Otyp_constr(out_ident, list(out_type))
  | Otyp_manifest(out_type, out_type)
  | Otyp_object(list((string, out_type)), option(bool))
  | Otyp_record(list((string, bool, out_type)))
  | Otyp_stuff(string)
  | Otyp_sum(list((string, list(out_type), option(out_type))))
  | Otyp_tuple(list(out_type))
  | Otyp_var(bool, string)
  | Otyp_variant(bool, out_variant, bool, option(list(string)))
  | Otyp_poly(list(string), out_type)
  | Otyp_module(string, list(string), list(out_type))
  | Otyp_attribute(out_type, out_attribute)

and out_variant =
  | Ovar_fields(list((string, bool, list(out_type))))
  | Ovar_typ(out_type);

type out_module_type =
  | Omty_abstract
  | Omty_ident(out_ident)
  | Omty_signature(list(out_sig_item))
  | Omty_alias(out_ident)
and out_sig_item =
  | Osig_modtype(string, out_module_type)
  | Osig_module(string, out_module_type, out_rec_status)
  | Osig_type(out_type_decl, out_rec_status)
  | Osig_value(out_val_decl)
  | Osig_ellipsis
and out_type_decl = {
  otype_name: string,
  otype_params: list((string, (bool, bool))),
  otype_type: out_type,
  otype_immediate: bool,
  otype_unboxed: bool,
  otype_cstrs: list((out_type, out_type)),
}
and out_type_extension = {
  otyext_name: string,
  otyext_params: list(string),
  otyext_constructors: list((string, list(out_type), option(out_type))),
}
and out_val_decl = {
  oval_name: string,
  oval_type: out_type,
  oval_prims: list(string),
  oval_attributes: list(out_attribute),
}
and out_rec_status =
  | Orec_not
  | Orec_first
  | Orec_next
and out_ext_status =
  | Oext_first
  | Oext_next
  | Oext_exception;

type out_phrase =
  | Ophr_eval(out_value, out_type)
  | Ophr_signature(list((out_sig_item, option(out_value))))
  | Ophr_exception((exn, out_value));
