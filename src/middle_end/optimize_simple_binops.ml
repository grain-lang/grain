open Anftree
open Grain_typed
open Types

let get_int = function
  | Const_int(n) -> n
  | Const_int32(n) -> Int32.to_int n
  | Const_int64(n) -> Int64.to_int n
  | _ -> failwith "Operand was not an integer"

let get_bool = function
  | Const_bool(b) -> b
  | _ -> failwith "Operand was not a boolean"

let in_valid_int_range op x y =
  let n = op (get_int x) (get_int y) in
  (* Numbers in Grain are stored double their value, so we need to check if the representation overflows *)
  let n = n * 2 in
  n < Int32.to_int Int32.max_int && 
  n > Int32.to_int Int32.min_int

module ConstantFoldingArg : Anf_mapper.MapArgument = struct
  include Anf_mapper.DefaultMapArgument

  let leave_comp_expression ({comp_desc = desc} as c) =
    match desc with
    | CPrim2(binop, ({imm_desc=ImmConst(x)} as i), {imm_desc=ImmConst(y)}) ->
      let wrap_imm imm = {c with comp_desc=CImmExpr({ i with imm_desc=ImmConst(imm) })} in
      begin match binop with
      (* in_valid_int_range check to make sure we don't overflow. 
         If we will overflow, don't optimize and allow the error at runtime. *)
      | Plus when in_valid_int_range (+) x y -> wrap_imm @@ Const_int(get_int x + get_int y)
      | Minus when in_valid_int_range (-) x y -> wrap_imm @@ Const_int(get_int x - get_int y)
      | Times when in_valid_int_range ( * ) x y -> wrap_imm @@ Const_int(get_int x * get_int y)
      | Less -> wrap_imm @@ Const_bool(get_int x < get_int y)
      | LessEq -> wrap_imm @@ Const_bool(get_int x <= get_int y)
      | Greater -> wrap_imm @@ Const_bool(get_int x > get_int y)
      | GreaterEq -> wrap_imm @@ Const_bool(get_int x >= get_int y)
      | Eq -> wrap_imm @@ Const_bool(x = y)
      | And -> wrap_imm @@ Const_bool(get_bool x && get_bool y)
      | Or -> wrap_imm @@ Const_bool(get_bool x || get_bool y)
      | _ -> c end
    | _ -> c

end

module ConstantFoldingMapper = Anf_mapper.MakeMap(ConstantFoldingArg)

let optimize anfprog =
  ConstantFoldingMapper.map_anf_program anfprog
