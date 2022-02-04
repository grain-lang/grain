open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;

type node_t =
  | Expression(Typedtree.expression)
  | Pattern(Typedtree.pattern)
  | NotInRange
  | Error(string);

let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

// let loc_to_range = (pos: Location.t): Rpc.range_t => {
//   let (_, startline, startchar, _) = get_raw_pos_info(pos.loc_start);
//   let (_, endline, endchar) =
//     Grain_parsing.Location.get_pos_info(pos.loc_end);

//   let range: Rpc.range_t = {
//     start_line: startline,
//     start_char: startchar,
//     end_line: endline,
//     end_char: endchar,
//   };
//   range;
// };

let is_point_inside_stmt = (loc1: Grain_parsing.Location.t, line: int) => {
  let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);

  let (_, raw1le, raw1ce, _) = get_raw_pos_info(loc1.loc_end);

  if (line == raw1l || line == raw1le) {
    true;
  } else if (line > raw1l && line < raw1le) {
    true;
  } else {
    false;
  };
};
let is_point_inside_location =
    (log, loc1: Grain_parsing.Location.t, line: int, char: int) => {
  let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);
  let (_, raw1le, raw1ce, _) = get_raw_pos_info(loc1.loc_end);

  let res =
    if (line == raw1l) {
      if (char >= raw1c) {
        if (line == raw1le) {
          if (char <= raw1ce) {
            true;
          } else {
            false;
          };
        } else {
          true;
        };
      } else {
        false;
      };
    } else if (line == raw1le) {
      if (char <= raw1ce) {
        true;
      } else {
        false;
      };
    } else if (line > raw1l && line < raw1le) {
      true;
    } else {
      false;
    };

  res;
};

let lens_sig = (t: Types.type_expr) => {
  let buf = Buffer.create(64);
  let ppf = Format.formatter_of_buffer(buf);
  Printtyp.type_expr(ppf, t);
  Format.pp_print_flush(ppf, ());
  let sigStr = Buffer.contents(buf);
  sigStr;
};
