open Graph
open Types
open Anf
open Intermediate

module CFGNode = struct
  type t = unit ir_stmt list
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = []
end

module CFG = Imperative.Digraph.Abstract(CFGNode)
open CFG

type ir_frame = {
  base_pointer : unit ir_imm
}

type ir_env = {
  stack_index : int;
  bindings: (string * unit ir_arg) list;
  frame: ir_frame;
}

let rec add_aexpr_to_cfg graph aexpr env =
  match aexpr with
  | ALet(name, expr, body, _) ->
    let (expr_start, expr_end) = add_cexpr_to_cfg graph expr env in
    let new_env = {
      env with
      bindings = (name, IMem(env.frame.base_pointer, env.stack_index, ()))::env.bindings;
      stack_index = env.stack_index + 1
    } in
    (expr_start, V.create [])
  | _ -> (V.create [], V.create [])

and add_cexpr_to_cfg graph cexpr env =
  match cexpr with
  | CIf(_, _, _, _) -> (V.create [], V.create [])
  | _ -> (V.create [], V.create [])

and add_immexpr_to_cfg graph immexpr env : CFG.vertex * CFG.vertex =
  match immexpr with
  | ImmNum(_, _) -> (V.create [], V.create [])
  | _ -> (V.create [], V.create [])
