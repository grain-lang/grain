open Grain_parsing;

let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

let get_comment_loc = (comment: Parsetree.comment) =>
  switch (comment) {
  | Line(cmt) => cmt.cmt_loc
  | Block(cmt) => cmt.cmt_loc
  | Doc(cmt) => cmt.cmt_loc
  | Shebang(cmt) => cmt.cmt_loc
  };
