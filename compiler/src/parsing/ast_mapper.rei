/* This file is largely copied from OCaml's parsing/ast_mapper.mli.
   The original copyright notice is reproduced below. */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         Alain Frisch, LexiFi                           */
/*                                                                        */
/*   Copyright 2012 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Parsetree;

/** A mapper record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the mapper to be applied to children in the syntax
    tree. */

type mapper = {
  constant: (mapper, constant) => constant,
  expr: (mapper, expression) => expression,
  pat: (mapper, pattern) => pattern,
  typ: (mapper, parsed_type) => parsed_type,
  data: (mapper, data_declaration) => data_declaration,
  constructor: (mapper, constructor_declaration) => constructor_declaration,
  label: (mapper, label_declaration) => label_declaration,
  location: (mapper, Location.t) => Location.t,
  import: (mapper, list(import_declaration)) => list(import_declaration),
  export: (mapper, list(export_declaration)) => list(export_declaration),
  export_all: (mapper, list(export_except)) => list(export_except),
  value_binding: (mapper, value_binding) => value_binding,
  match_branch: (mapper, match_branch) => match_branch,
  value_description: (mapper, value_description) => value_description,
  toplevel: (mapper, toplevel_stmt) => toplevel_stmt,
};

/** A default mapper, which implements a "deep identity" mapping. */

let default_mapper: mapper;
