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
open Grain_parsing;
open Grain_utils;

[@deriving (sexp, yojson)]
type pers_flags =
  | Opaque
  | Unsafe_string;

type error =
  | Wrong_version_interface(string, string)
  | Corrupted_interface(string)
  | Interface_file_not_found(string);

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

let sexp_of_cmi_digest = d => Sexplib.Conv.sexp_of_string(Digest.to_hex(d));
let cmi_digest_of_sexp =
  fun
  | Sexplib.Sexp.Atom(s) as d =>
    try(Digest.from_hex(s)) {
    | Invalid_argument(_) =>
      of_sexp_error("cmi_digest_of_sexp: invalid digest", d)
    }
  | d => of_sexp_error("cmi_digest_of_sexp: invalid digest", d);

[@deriving sexp]
type cmi_crcs = list((string, cmi_digest));
let rec cmi_crcs_of_yojson = [%of_yojson: list((string, cmi_digest))]
and cmi_crcs_to_yojson = [%to_yojson: list((string, cmi_digest))];

[@deriving sexp]
type cmi_crc = cmi_digest;
let rec cmi_crc_of_yojson = [%of_yojson: cmi_digest]
and cmi_crc_to_yojson = [%to_yojson: cmi_digest];

[@deriving (sexp, yojson)]
type cmi_type_metadata = {
  ctm_metadata: string,
  ctm_exceptions: string,
  ctm_offsets_tbl: list((int, int)),
};

[@deriving (sexp, yojson)]
type cmi_infos = {
  cmi_name: string,
  cmi_sign: Types.signature,
  cmi_crcs,
  cmi_crc,
  cmi_flags: list(pers_flags),
  cmi_type_metadata,
  cmi_config_sum: string,
};

type config_opt =
  | Cmi_config_opt('a): config_opt;

let magic = {
  let bytes = Bytes.create(4);
  Bytes.set_uint8(bytes, 0, 0xF0);
  Bytes.set_uint8(bytes, 1, 0x9F);
  Bytes.set_uint8(bytes, 2, 0x8C);
  Bytes.set_uint8(bytes, 3, 0xBE);
  bytes;
};

let config_sum = Config.get_root_config_digest;

let build_crc = (~name: string, sign: Types.signature) => {
  let subst_sign =
    Subst.with_reset_state(() =>
      Subst.signature(Subst.for_crc(Subst.identity), sign)
    );

  let ns_sign =
    Marshal.to_bytes(
      (name, subst_sign, Config.get_root_config_digest()),
      [],
    );
  Digest.bytes(ns_sign);
};

let read_cmi = (ic, filename): cmi_infos => {
  let read_magic = Bytes.create(4);
  really_input(ic, read_magic, 0, 4);
  if (read_magic != magic) {
    raise(Error(Corrupted_interface(filename)));
  };
  let version_length = input_binary_int(ic);
  let read_version = really_input_string(ic, version_length);
  if (read_version != Config.version) {
    raise(Error(Wrong_version_interface(filename, read_version)));
  };
  let _ = input_binary_int(ic);
  Marshal.from_channel(ic);
};
let read_cmi = filename => {
  let ic =
    try(open_in_bin(filename)) {
    | _ => raise(Error(Interface_file_not_found(filename)))
    };
  let cmi =
    try(read_cmi(ic, filename)) {
    | Error(_) as exn =>
      close_in(ic);
      raise(exn);
    | _ =>
      close_in(ic);
      raise(Error(Corrupted_interface(filename)));
    };
  close_in(ic);
  cmi;
};

/* Error report */

open Format;

let report_error = ppf =>
  fun
  | Wrong_version_interface(filename, older_newer) =>
    fprintf(
      ppf,
      "%a@ is not a compiled interface for this version of Grain.@.It seems to be for %s version of Grain.",
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
    )
  | Interface_file_not_found(filename) =>
    fprintf(
      ppf,
      "Interface file not found@ %a",
      Location.print_filename,
      filename,
    );

let () =
  Location.register_error_of_exn(
    fun
    | Error(err) => Some(Location.error_of_printer_file(report_error, err))
    | _ => None,
  );
