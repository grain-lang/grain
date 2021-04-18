open Types;

let rec get_allocation_type = (env, ty) => {
  switch (ty.desc) {
  | TTyConstr(path, _, _) => Env.find_type(path, env).type_allocation
  | TTySubst(linked)
  | TTyLink(linked) => get_allocation_type(env, linked)
  | TTyVar(_)
  | TTyArrow(_)
  | TTyTuple(_)
  | TTyRecord(_)
  | TTyUniVar(_)
  | TTyPoly(_) => HeapAllocated
  };
};

let rec get_fn_allocation_type = (env, ty) => {
  switch (ty.desc) {
  | TTySubst(linked)
  | TTyLink(linked) => get_fn_allocation_type(env, linked)
  | TTyArrow(args, ret, _) => (
      List.map(get_allocation_type(env), args),
      get_allocation_type(env, ret),
    )
  | TTyConstr(_)
  | TTyVar(_)
  | TTyTuple(_)
  | TTyRecord(_)
  | TTyUniVar(_)
  | TTyPoly(_) =>
    failwith("get_fn_allocation_type: function type was non-function")
  };
};

let rec is_function = ty => {
  switch (ty.desc) {
  | TTySubst(linked)
  | TTyLink(linked) => is_function(linked)
  | TTyArrow(_) => true
  | TTyConstr(_)
  | TTyVar(_)
  | TTyTuple(_)
  | TTyRecord(_)
  | TTyUniVar(_)
  | TTyPoly(_) => false
  };
};

let rec is_void = ty => {
  switch (ty.desc) {
  | TTySubst(linked)
  | TTyLink(linked) => is_void(linked)
  | TTyConstr(path, _, _) when path == Builtin_types.path_void => true
  | TTyConstr(_)
  | TTyArrow(_)
  | TTyVar(_)
  | TTyTuple(_)
  | TTyRecord(_)
  | TTyUniVar(_)
  | TTyPoly(_) => false
  };
};

let rec returns_void = ty => {
  switch (ty.desc) {
  | TTySubst(linked)
  | TTyLink(linked) => returns_void(linked)
  | TTyArrow(args, ret, _) => is_void(ret)
  | TTyConstr(_)
  | TTyVar(_)
  | TTyTuple(_)
  | TTyRecord(_)
  | TTyUniVar(_)
  | TTyPoly(_) =>
    failwith("get_fn_allocation_type: function type was non-function")
  };
};

let wasm_repr_of_allocation_type = alloc_type => {
  switch (alloc_type) {
  | StackAllocated(repr) => repr
  | HeapAllocated => WasmI32
  };
};

let repr_of_type = (env, ty) =>
  if (is_function(ty)) {
    let (args, ret) = get_fn_allocation_type(env, ty);
    let args = List.map(wasm_repr_of_allocation_type, args);
    let rets =
      if (returns_void(ty)) {
        [];
      } else {
        [wasm_repr_of_allocation_type(ret)];
      };
    ReprFunction(args, rets);
  } else {
    ReprValue(wasm_repr_of_allocation_type(get_allocation_type(env, ty)));
  };
