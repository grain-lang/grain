
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b


type sourcespan = (Lexing.position * Lexing.position)
type initial_func = (string * sourcespan * bool) (* name, loc, is_pure *)
exception UnboundId of string * sourcespan (* name, where used *)
exception UnboundFun of string * sourcespan (* name of fun, where used *)
exception ShadowId of string * sourcespan * sourcespan (* name, where used, where defined *)
exception DuplicateId of string * sourcespan * sourcespan (* name, where used, where defined *)
exception DuplicateFun of string * sourcespan * sourcespan (* name, where used, where defined *)
exception Overflow of int * sourcespan (* value, where used *)
exception LetRecNonFunction of string * sourcespan (* name binding, where defined *)
exception EllipsisInNonLibrary of sourcespan (* where used *)
exception EllipsisNotInTailPosition of sourcespan (* where used *)
exception EllipsisNotInLibrary of sourcespan (* tail expression where ellipsis should be *)
exception IncludeNotAtBeginning of sourcespan (* location *)
exception IncludeNotFound of string * sourcespan (* library, location of include *)
exception MalformedString of sourcespan


type prim1 =
  | Add1
  | Sub1
  | Not
  | PrintStack
  | IsNum
  | IsBool
  | IsTuple

type prim2 =
  | Plus
  | Minus
  | Times
  | Less
  | Greater
  | LessEq
  | GreaterEq
  | Eq
  | And
  | Or

type typ =
  | TyCon of string (* things like Int or Bool *)
  | TyVar of string (* things like X or Y *)
  | TyArr of typ list * typ (* t1 t2 ... -> t_ret *)
  | TyTup of typ list (* (t1, t2, ..., tn) *)

type scheme = (string list * typ) (* Forall X, Y, ..., typ *)

type 'a bind =
  | LetBind of string * scheme option * 'a expr * 'a
  | TupDestr of ((string * 'a) list) * scheme option * 'a expr * 'a

and 'a data_branch =
  | DDataSingleton of string * 'a
  | DDataConstructor of string * typ list * 'a

and 'a expr =
  | ELet of 'a bind list * 'a expr * 'a
  | ELetRec of 'a bind list * 'a expr * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | EIf of 'a expr * 'a expr * 'a expr * 'a
  | ETuple of 'a expr list * 'a
  | EString of string * 'a
  | EGetItem of 'a expr * 'a expr * 'a
  | ESetItem of 'a expr * 'a expr * 'a expr * 'a
  | EGetItemExact of 'a expr * int * 'a
  | ESetItemExact of 'a expr * int * 'a expr * 'a
  | ENumber of int * 'a
  | EBool of bool * 'a
  | EId of string * 'a
  | EApp of 'a expr * 'a expr list * 'a
  | ELambda of (string * 'a) list * 'a expr * 'a
  | ESeq of 'a expr list * 'a
  | EEllipsis of 'a
  | EInclude of string * 'a expr * 'a
  | ENull (* Used for modules without body expressions *)

and 'a stmt =
  | SInclude of string * 'a
  | SLet of 'a bind list * 'a
  | SLetRec of 'a bind list * 'a
  | SDataDecl of string * typ list * 'a data_branch list * 'a

type 'a program = {
  statements: 'a stmt list;
  body: 'a expr
}

type 'a immexpr = (* immediate expressions *)
  | ImmNum of int * 'a
  | ImmBool of bool * 'a
  | ImmId of string * 'a
and 'a cexpr = (* compound expressions *)
  | CIf of 'a immexpr * 'a aexpr * 'a aexpr * 'a
  | CPrim1 of prim1 * 'a immexpr * 'a
  | CPrim2 of prim2 * 'a immexpr * 'a immexpr * 'a
  | CApp of 'a immexpr * 'a immexpr list * 'a
  | CTuple of 'a immexpr list * 'a
  | CString of string * 'a
  | CGetItem of 'a immexpr * 'a immexpr * 'a
  | CSetItem of 'a immexpr * 'a immexpr * 'a immexpr * 'a
  | CLambda of string list * 'a aexpr * 'a
  | CImmExpr of 'a immexpr (* for when you just need an immediate value *)
and 'a aexpr = (* anf expressions *)
  | ALet of string * 'a cexpr * 'a aexpr * 'a
  | ALetRec of (string * 'a cexpr) list * 'a aexpr * 'a
  | ASeq of 'a cexpr * 'a aexpr * 'a
  | ACExpr of 'a cexpr

and 'a aprogram = 'a aexpr

type 'a envt = (string * 'a) list
