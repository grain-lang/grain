open Grain_parsing;
open Grain_typed;
open Anftree;

type stack_size = {
  stack_size_ptr: int,
  stack_size_i32: int,
  stack_size_i64: int,
  stack_size_f32: int,
  stack_size_f64: int,
};

let stack_size_max = (a, b) => {
  stack_size_ptr: max(a.stack_size_ptr, b.stack_size_ptr),
  stack_size_i32: max(a.stack_size_i32, b.stack_size_i32),
  stack_size_i64: max(a.stack_size_i64, b.stack_size_i64),
  stack_size_f32: max(a.stack_size_f32, b.stack_size_f32),
  stack_size_f64: max(a.stack_size_f64, b.stack_size_f64),
};

let stack_size_add = (a, b) => {
  stack_size_ptr: a.stack_size_ptr + b.stack_size_ptr,
  stack_size_i32: a.stack_size_i32 + b.stack_size_i32,
  stack_size_i64: a.stack_size_i64 + b.stack_size_i64,
  stack_size_f32: a.stack_size_f32 + b.stack_size_f32,
  stack_size_f64: a.stack_size_f64 + b.stack_size_f64,
};

let initial_stack_size = {
  stack_size_ptr: 0,
  stack_size_i32: 0,
  stack_size_i64: 0,
  stack_size_f32: 0,
  stack_size_f64: 0,
};

let rec anf_count_vars = a =>
  switch (a.anf_desc) {
  | AELet(global, recflag, mutflag, binds, body) =>
    let max_binds =
      List.fold_left(stack_size_max, initial_stack_size) @@
      List.map(((_, c)) => comp_count_vars(c), binds);
    let rec count_binds = (ptr, i32, i64, f32, f64, binds) => {
      switch (global, binds) {
      | (Global, [_, ...rest]) => count_binds(ptr, i32, i64, f32, f64, rest)
      | (_, [(_, {comp_allocation_type: Managed}), ...rest]) =>
        count_binds(ptr + 1, i32, i64, f32, f64, rest)
      | (_, [(_, {comp_allocation_type: Unmanaged(WasmI32)}), ...rest]) =>
        count_binds(ptr, i32 + 1, i64, f32, f64, rest)
      | (_, [(_, {comp_allocation_type: Unmanaged(WasmI64)}), ...rest]) =>
        count_binds(ptr, i32, i64 + 1, f32, f64, rest)
      | (_, [(_, {comp_allocation_type: Unmanaged(WasmF32)}), ...rest]) =>
        count_binds(ptr, i32, i64, f32 + 1, f64, rest)
      | (_, [(_, {comp_allocation_type: Unmanaged(WasmF64)}), ...rest]) =>
        count_binds(ptr, i32, i64, f32, f64 + 1, rest)
      | (_, []) => {
          stack_size_ptr: ptr,
          stack_size_i32: i32,
          stack_size_i64: i64,
          stack_size_f32: f32,
          stack_size_f64: f64,
        }
      };
    };
    switch (recflag) {
    | Recursive =>
      stack_size_add(
        count_binds(0, 0, 0, 0, 0, binds),
        stack_size_max(max_binds, anf_count_vars(body)),
      )
    | Nonrecursive =>
      stack_size_max(
        max_binds,
        stack_size_add(
          count_binds(0, 0, 0, 0, 0, binds),
          anf_count_vars(body),
        ),
      )
    };
  | AESeq(hd, tl) =>
    stack_size_max(comp_count_vars(hd), anf_count_vars(tl))
  | AEComp(c) => comp_count_vars(c)
  }

and comp_count_vars = c =>
  switch (c.comp_desc) {
  | CIf(_, t, f) => stack_size_max(anf_count_vars(t), anf_count_vars(f))
  | CFor(c, inc, b) =>
    let c = Option.fold(~none=initial_stack_size, ~some=anf_count_vars, c);
    let inc =
      Option.fold(~none=initial_stack_size, ~some=anf_count_vars, inc);
    let b = anf_count_vars(b);
    stack_size_add(c, stack_size_add(inc, b));
  | CSwitch(_, bs, _) =>
    List.fold_left(stack_size_max, initial_stack_size) @@
    List.map(((_, b)) => anf_count_vars(b), bs)
  | _ => initial_stack_size
  };

module ClearLocationsArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let leave_imm_expression = i => {...i, imm_loc: Location.dummy_loc};

  let leave_comp_expression = c => {...c, comp_loc: Location.dummy_loc};

  let leave_anf_expression = a => {...a, anf_loc: Location.dummy_loc};
};

module ClearLocations = Anf_mapper.MakeMap(ClearLocationsArg);

let clear_locations = ClearLocations.map_anf_program;
