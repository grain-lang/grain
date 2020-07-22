/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                   Fabrice Le Fessant, INRIA Saclay                     */
/*                                                                        */
/*   Copyright 2012 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Sexplib.Conv;
open Grain_utils;
open Wasm_utils;

[@deriving (sexp, yojson)]
type pers_flags =
  | Rectypes
  | Opaque
  | Unsafe_string;

type error =
  | Not_an_interface(string)
  | Wrong_version_interface(string, string)
  | Corrupted_interface(string);

exception Error(error);

/* See: https://github.com/janestreet/ppx_sexp_conv/issues/26 */
type cmi_digest = Digest.t;
let cmi_digest_to_yojson = d => `String(Digest.to_hex(d));
let cmi_digest_of_yojson =
  fun
  | `String(s) as d =>
    try(Result.Ok(Digest.from_hex(s))) {
    | Invalid_argument(_) =>
      Result.Error(
        "cmi_digest_of_yojson: Invalid Digest: " ++ Yojson.Safe.to_string(d),
      )
    }
  | d =>
    Result.Error(
      "cmi_digest_of_yojson: Invalid Digest: " ++ Yojson.Safe.to_string(d),
    );

[@deriving sexp]
type cmi_crcs = [@sexp.opaque] list((string, option(Digest.t)));
let rec cmi_crcs_of_yojson = [%of_yojson:
  list((string, option(cmi_digest)))
]
and cmi_crcs_to_yojson = [%to_yojson: list((string, option(cmi_digest)))];

[@deriving (sexp, yojson)]
type cmi_infos = {
  cmi_name: string,
  cmi_sign: list(Types.signature_item),
  cmi_crcs,
  cmi_flags: list(pers_flags),
};

let build_full_cmi = (~name, ~sign, ~crcs, ~flags) => {
  let ns_sign = Marshal.to_bytes((name, sign), []);
  let crc = Digest.bytes(ns_sign);
  let crcs = [(name, Some(crc)), ...crcs];
  {cmi_name: name, cmi_sign: sign, cmi_crcs: crcs, cmi_flags: flags};
};

let input_cmi = ic =>
  switch (cmi_infos_of_yojson @@ Yojson.Safe.from_channel(ic)) {
  | Result.Ok(x) => x
  | Result.Error(e) => raise(Invalid_argument(e))
  };

let deserialize_cmi = bytes =>
  switch (
    cmi_infos_of_yojson @@ Yojson.Safe.from_string(Bytes.to_string(bytes))
  ) {
  | Result.Ok(x) => x
  | Result.Error(e) => raise(Invalid_argument(e))
  };

let serialize_cmi =
    (
      {cmi_name: name, cmi_sign: sign, cmi_crcs: crcs, cmi_flags: flags} as cmi_info,
    ) =>
  Bytes.of_string @@ Yojson.Safe.to_string @@ cmi_infos_to_yojson(cmi_info);

module CmiBinarySection =
  BinarySection({
    type t = cmi_infos;

    let name = "cmi";

    let deserialize = deserialize_cmi;
    let serialize = serialize_cmi;
    let accepts_version = ({major}) => major == 1;
  });

let read_cmi = filename => {
  let ic = open_in_bin(filename);
  try(
    switch (CmiBinarySection.load(ic)) {
    | Some(cmi) =>
      close_in(ic);
      cmi;
    | None => raise(End_of_file)
    }
  ) {
  | End_of_file
  | Failure(_) =>
    close_in(ic);
    raise(Error(Corrupted_interface(filename)));
  | Error(e) =>
    close_in(ic);
    raise(Error(e));
  };
};

let serialize_cmi = cmi =>
  /* beware: the provided signature must have been substituted for saving */
  CmiBinarySection.serialize(cmi);

/* Error report */

open Format;

let report_error = ppf =>
  fun
  | Not_an_interface(filename) =>
    fprintf(
      ppf,
      "%a@ is not a compiled interface",
      Location.print_filename,
      filename,
    )
  | [@implicit_arity] Wrong_version_interface(filename, older_newer) =>
    fprintf(
      ppf,
      "%a@ is not a compiled interface for this version of OCaml.@.It seems to be for %s version of OCaml.",
      Location.print_filename,
      filename,
      older_newer,
    )
  | Corrupted_interface(filename) =>
    fprintf(
      ppf,
      "Corrupted compiled interface@ %a",
      Location.print_filename,
      filename,
    );

let () =
  Location.register_error_of_exn(
    fun
    | Error(err) => Some(Location.error_of_printer_file(report_error, err))
    | _ => None,
  );
