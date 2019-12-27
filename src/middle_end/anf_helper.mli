open Grain_parsing
open Grain_typed
open Types
open Anftree

type str = string loc
type loc = Location.t
type env = Env.t
type ident = Ident.t

module Imm : sig
  val mk : ?loc:loc -> ?env:env -> imm_expression_desc -> imm_expression
  val id : ?loc:loc -> ?env:env -> ident -> imm_expression
  val const : ?loc:loc -> ?env:env -> constant -> imm_expression
end

module Comp : sig
  val mk : ?loc:loc -> ?env:env -> comp_expression_desc -> comp_expression
  val imm : ?loc:loc -> ?env:env -> imm_expression -> comp_expression
  val prim1 : ?loc:loc -> ?env:env -> prim1 -> imm_expression -> comp_expression
  val prim2 : ?loc:loc -> ?env:env -> prim2 -> imm_expression -> imm_expression -> comp_expression
  val assign : ?loc:loc -> ?env:env -> imm_expression -> imm_expression -> comp_expression
  val tuple : ?loc:loc -> ?env:env -> imm_expression list -> comp_expression
  val record : ?loc:loc -> ?env:env -> imm_expression -> (str * imm_expression) list -> comp_expression
  val adt : ?loc:loc -> ?env:env -> imm_expression -> imm_expression -> imm_expression list -> comp_expression
  val tuple_get : ?loc:loc -> ?env:env -> int32 -> imm_expression -> comp_expression
  val tuple_set : ?loc:loc -> ?env:env -> int32 -> imm_expression -> imm_expression -> comp_expression
  val adt_get : ?loc:loc -> ?env:env -> int32 -> imm_expression -> comp_expression
  val adt_get_tag : ?loc:loc -> ?env:env -> imm_expression -> comp_expression
  val record_get : ?loc:loc -> ?env:env -> int32 -> imm_expression -> comp_expression
  val if_ : ?loc:loc -> ?env:env -> imm_expression -> anf_expression -> anf_expression -> comp_expression
  val while_ : ?loc:loc -> ?env:env -> anf_expression -> anf_expression -> comp_expression
  val switch : ?loc:loc -> ?env:env -> imm_expression -> (int * anf_expression) list -> comp_expression
  val app : ?loc:loc -> ?env:env -> imm_expression -> imm_expression list -> comp_expression
  val app_builtin : ?loc:loc -> ?env:env -> string -> string -> imm_expression list -> comp_expression
  val lambda : ?loc:loc -> ?env:env -> ident list -> anf_expression -> comp_expression
  val string : ?loc:loc -> ?env:env -> string -> comp_expression
end

module AExp : sig
  val mk : ?loc:loc -> ?env:env -> anf_expression_desc -> anf_expression
  val let_ : ?loc:loc -> ?env:env -> ?glob:global_flag -> rec_flag -> (ident * comp_expression) list -> anf_expression -> anf_expression
  val seq : ?loc:loc -> ?env:env -> comp_expression -> anf_expression -> anf_expression
  val comp : ?loc:loc -> ?env:env -> comp_expression -> anf_expression
end

module Imp : sig
  val mk : ident -> import_desc -> import_shape -> import_spec
  val grain_value : ident -> string -> string -> import_shape -> import_spec
  val wasm_func : ident -> string -> string -> import_shape -> import_spec
  val js_func : ident -> string -> string -> import_shape -> import_spec
end
