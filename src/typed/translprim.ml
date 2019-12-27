open Grain_parsing
open Ast_helper
open Typedtree
open Parsetree

type primitive =
  | Primitive1 of prim1
  | Primitive2 of prim2

module PrimMap = Hashtbl.Make(struct 
    type t = string
    let hash x = Hashtbl.hash x
    let equal a b = (String.compare a b) == 0
  end)


let default_loc = (!default_loc_src)()

let mkident name = Exp.ident (Location.mkloc (Identifier.IdentName name) default_loc)
let mkpatvar name = {ppat_desc=PPatVar (Location.mkloc name default_loc); ppat_loc=default_loc}

let id_a = mkident "a"
let id_b = mkident "b"
let pat_a = mkpatvar "a"
let pat_b = mkpatvar "b"
  
let prim_map = PrimMap.of_seq (List.to_seq [
  ("@incr", Primitive1 Incr);
  ("@decr", Primitive1 Decr);
  ("@not", Primitive1 Not);
  ("@box", Primitive1 Box);
  ("@unbox", Primitive1 Unbox);
  ("@ignore", Primitive1 Ignore);

  ("@plus", Primitive2 Plus);
  ("@minus", Primitive2 Minus);
  ("@times", Primitive2 Times);
  ("@divides", Primitive2 Divide);
  ("@modulo", Primitive2 Mod);
  ("@less", Primitive2 Less);
  ("@greater", Primitive2 Greater);
  ("@lesseq", Primitive2 LessEq);
  ("@greatereq", Primitive2 GreaterEq);
  ("@eq", Primitive2 Eq);
  ("@and", Primitive2 And);
  ("@or", Primitive2 Or);
])

let transl_prim env desc = 
  let prim = try
    PrimMap.find prim_map (List.hd desc.tvd_prim)
  with Not_found ->
    failwith "Throw a real error here"
  in
  let value = match prim with
    | Primitive1 p ->
      Exp.lambda [pat_a] (Exp.prim1 p id_a)
    | Primitive2 p ->
      Exp.lambda [pat_a; pat_b] (Exp.prim2 p id_a id_b)
  in
  let binds = [{
    pvb_pat={ppat_desc=PPatVar desc.tvd_name; ppat_loc=desc.tvd_loc};
    pvb_expr=value;
    pvb_loc=desc.tvd_loc;
  }] in
  Typecore.type_binding env Nonrecursive binds None
