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

type pers_flags =
  | Opaque
  | Unsafe_string;

[@deriving (sexp, yojson)]
type cmi_type_metadata = {
  ctm_metadata: string,
  ctm_exceptions: string,
  ctm_offsets_tbl: list((int, int)),
};

[@deriving (sexp, yojson)]
type cmi_infos = {
  cmi_name: string,
  cmi_sign: list(Types.signature_item),
  cmi_crcs: list((string, Digest.t)),
  cmi_crc: Digest.t,
  cmi_flags: list(pers_flags),
  cmi_type_metadata,
  cmi_config_sum: string,
};

let config_sum: unit => string;

let build_crc: (~name: string, Types.signature) => Digest.t;

/* write the magic + the cmi information */
let serialize_cmi: cmi_infos => bytes;

/* read the cmi information (the magic is supposed to have already been read) */
let input_cmi: in_channel => cmi_infos;

/* read a cmi from a filename, checking the magic */
let read_cmi: string => cmi_infos;

/* Error report */

type error =
  | Not_an_interface(string)
  | Wrong_version_interface(string, string)
  | Corrupted_interface(string);

exception Error(error);

open Format;

let report_error: (formatter, error) => unit;
