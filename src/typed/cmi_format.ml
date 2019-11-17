(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Sexplib.Conv
open Grain_utils
open Wasm_utils

type pers_flags =
  | Rectypes
  | Opaque
  | Unsafe_string
[@@deriving sexp, yojson]

type error =
    Not_an_interface of string
  | Wrong_version_interface of string * string
  | Corrupted_interface of string

exception Error of error

(* See: https://github.com/janestreet/ppx_sexp_conv/issues/26 *)
type cmi_digest = Digest.t
let cmi_digest_to_yojson d =
  `String (Digest.to_hex d)
let cmi_digest_of_yojson = function
  | (`String s) as d -> begin try
      Result.Ok (Digest.from_hex s)
    with
      | Invalid_argument _ -> Result.Error ("cmi_digest_of_yojson: Invalid Digest: " ^ (Yojson.Safe.to_string d))
    end
  | d -> Result.Error ("cmi_digest_of_yojson: Invalid Digest: " ^ (Yojson.Safe.to_string d))

type cmi_crcs = (string * Digest.t option) list sexp_opaque [@@deriving sexp]
let rec cmi_crcs_of_yojson = [%of_yojson: (string * cmi_digest option) list]
and cmi_crcs_to_yojson = [%to_yojson: (string * cmi_digest option) list]

type cmi_infos = {
    cmi_name : string;
    cmi_sign : Types.signature_item list;
    cmi_crcs : cmi_crcs;
    cmi_flags : pers_flags list;
} [@@deriving sexp, yojson]

let build_full_cmi ~name ~sign ~crcs ~flags =
  let ns_sign = Marshal.to_bytes (name, sign) [] in
  let crc = Digest.bytes ns_sign in
  let crcs = (name, Some crc) :: crcs in
  {
    cmi_name = name;
    cmi_sign = sign;
    cmi_crcs = crcs;
    cmi_flags = flags;
  }

let input_cmi ic =
  match cmi_infos_of_yojson @@ Yojson.Safe.from_channel ic with
  | Result.Ok x -> x
  | Result.Error e -> raise (Invalid_argument e)

let deserialize_cmi bytes =
  match cmi_infos_of_yojson @@ Yojson.Safe.from_string (Bytes.to_string bytes) with
  | Result.Ok x -> x
  | Result.Error e -> raise (Invalid_argument e)

let serialize_cmi ({cmi_name=name; cmi_sign=sign; cmi_crcs=crcs; cmi_flags=flags} as cmi_info) =
  Bytes.of_string @@ Yojson.Safe.to_string @@ cmi_infos_to_yojson cmi_info

module CmiBinarySection = BinarySection(struct
    type t = cmi_infos

    let name = "cmi"

    let deserialize = deserialize_cmi
    let serialize = serialize_cmi
    let accepts_version {major} = major = 1
  end)

let read_cmi filename =
  let ic = open_in_bin filename in
  try
    match CmiBinarySection.load ic with
    | Some(cmi) ->
      close_in ic;
      cmi
    | None -> raise End_of_file
  with End_of_file | Failure _ ->
      close_in ic;
      raise(Error(Corrupted_interface(filename)))
    | Error e ->
      close_in ic;
      raise (Error e)

let output_cmi filename oc cmi =
  (* beware: the provided signature must have been substituted for saving *)
  CmiBinarySection.write cmi oc

(* Error report *)

open Format

let report_error ppf = function
  | Not_an_interface filename ->
      fprintf ppf "%a@ is not a compiled interface"
        Location.print_filename filename
  | Wrong_version_interface (filename, older_newer) ->
      fprintf ppf
        "%a@ is not a compiled interface for this version of OCaml.@.\
         It seems to be for %s version of OCaml."
        Location.print_filename filename older_newer
  | Corrupted_interface filename ->
      fprintf ppf "Corrupted compiled interface@ %a"
        Location.print_filename filename

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
