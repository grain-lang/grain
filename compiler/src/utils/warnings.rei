/* This file largely borrows from OCaml's utils/warnings.mli file. The original copyright is reproduced below: */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          */
/*                                                                        */
/*   Copyright 1998 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

type loc = {
  loc_start: Lexing.position,
  loc_end: Lexing.position,
  loc_ghost: bool,
};

type number_type =
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float32
  | Float64
  | Rational
  | BigInt;

type t =
  | LetRecNonFunction(string)
  | AmbiguousName(list(string), list(string), bool)
  | NotPrincipal(string)
  | NameOutOfScope(string, list(string), bool)
  | StatementType
  | NonreturningStatement
  | AllClausesGuarded
  | PartialMatch(string)
  | FragileMatch(string)
  | UnusedMatch
  | UnusedPat
  | UnusedExtension
  | NonClosedRecordPattern(string)
  | UnreachableCase
  | ShadowConstructor(string)
  | NoCmiFile(string, option(string))
  | FuncWasmUnsafe(string, string, string)
  | FromNumberLiteral(number_type, string, string)
  | UselessRecordSpread
  | PrintUnsafe(string)
  | ToStringUnsafe(string)
  | ArrayIndexNonInteger(string);

let is_active: t => bool;
let is_error: t => bool;

type reporting_information = {
  number: int,
  message: string,
  is_error: bool,
  sub_locs: list((loc, string)),
};

let report: t => [ | `Active(reporting_information) | `Inactive];

exception Errors;

let check_fatal: unit => unit;
let reset_fatal: unit => unit;

type state;
let backup: unit => state;
let restore: state => unit;

// store the warnings for the LSP to report
let add_warning: (loc, t) => unit;
let get_warnings: unit => list((loc, t));
let reset_warnings: unit => unit;

// expose these for the LSP to use to format the warnings
let message: t => string;
let number: t => int;

let with_preserve_warnings: (unit => 'a) => 'a;
