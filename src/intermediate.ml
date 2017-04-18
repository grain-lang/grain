open Value_tags

(* NOTE: Currently unused *)
(* Based loosely on Tiger book's IR *)

type ir_binop =
  | PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR

type ir_label = string

type ir_relop =
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE

type 'a ir_imm =
  | IConst of int * 'a
  | IName of string * 'a
  | ITemp of string * 'a
  | ILabelImm of string * 'a
and 'a ir_arg =
  | IImm of 'a ir_imm * 'a
  | IBinop of ir_binop * 'a ir_arg * 'a ir_arg * 'a
  | IMem of 'a ir_imm * int * 'a (* loc, offset in words *)
  | ICall of 'a ir_arg * 'a ir_arg list * 'a
  | IArgSeq of 'a ir_stmt * 'a ir_arg * 'a
  | IAlloc of int * 'a
  | ITagCheck of 'a ir_arg * 'a
  | ITag of 'a ir_arg * tag_type * 'a
  | IUntag of 'a ir_arg * tag_type * 'a

and 'a ir_stmt =
  | IMove of 'a ir_arg * 'a ir_arg * 'a
  | IExp of 'a ir_arg * 'a
  | IJump of 'a ir_arg * ir_label list * 'a
  | ICJump of ir_relop * 'a ir_arg * 'a ir_arg * ir_label * ir_label * 'a
  | IStmtSeq of 'a ir_stmt * 'a ir_stmt * 'a
  | ILabel of ir_label * 'a
  | ITagAssert of 'a ir_arg * 'a

