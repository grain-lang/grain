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

let input_cmi = ic =>
  switch (cmi_infos_of_yojson @@ Yojson.Safe.from_channel(ic)) {
  | Result.Ok(x) => x
  | Result.Error(e) => raise(Invalid_argument(e))
  };

let deserialize_cmi = (ic, size) => {
  let size = ref(size);
  let lexbuf =
    Lexing.from_function((buf, n) => {
      let n = min(n, size^);
      let read = input(ic, buf, 0, n);
      size := size^ - read;
      read;
    });
  let state = Yojson.init_lexer();
  switch (cmi_infos_of_yojson @@ Yojson.Safe.from_lexbuf(state, lexbuf)) {
  | Result.Ok(x) => x
  | Result.Error(e) => raise(Invalid_argument(e))
  };
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
  switch (CmiBinarySection.load(ic)) {
  | Some(cmi) =>
    close_in(ic);
    cmi;
  | None
  | exception End_of_file
  | exception (Invalid_argument(_))
  | exception (Failure(_)) =>
    close_in(ic);
    raise(Error(Corrupted_interface(filename)));
  | exception (Error(e)) =>
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
    );

let () =
  Location.register_error_of_exn(
    fun
    | Error(err) => Some(Location.error_of_printer_file(report_error, err))
    | _ => None,
  );
