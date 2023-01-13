open Types;

let rec get_allocation_type = (env, ty) => {
  switch (ty.desc) {
  | TTyConstr(path, _, _) =>
    try(Env.find_type(path, env).type_allocation) {
    // Types not in the environment come from other modules and are nested in
    // types we do know about; we treat them as Managed Grain values.
    | Not_found => Managed
    }
  | TTySubst(linked)
  | TTyLink(linked) => get_allocation_type(env, linked)
  | TTyVar(_)
  | TTyArrow(_)
  | TTyTuple(_)
  | TTyRecord(_)
  | TTyUniVar(_)
  | TTyPoly(_) => Managed
  };
};

let rec get_fn_allocation_type = (env, ty) => {
  switch (ty.desc) {
  | TTySubst(linked)
  | TTyLink(linked) => get_fn_allocation_type(env, linked)
  | TTyArrow(args, ret, _) => (
      List.map(((_, arg)) => get_allocation_type(env, arg), args),
      get_allocation_type(env, ret),
    )
  | TTyConstr(path, args, _) =>
    let (ty_args, ty, _) = Env.find_type_expansion(path, env);
    get_fn_allocation_type(env, Ctype.apply(env, ty_args, ty, args));
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

let rec returns_void = (env, ty) => {
  switch (ty.desc) {
  | TTySubst(linked)
  | TTyLink(linked) => returns_void(env, linked)
  | TTyArrow(args, ret, _) => is_void(ret)
  | TTyConstr(path, args, _) =>
    let (ty_args, ty, _) = Env.find_type_expansion(path, env);
    returns_void(env, Ctype.apply(env, ty_args, ty, args));
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
  | Unmanaged(repr) => repr
  | Managed => WasmI32
  };
};

let allocation_type_of_wasm_repr = repr => {
  Unmanaged(repr);
};

let repr_of_type = (env, ty) =>
  if (is_function(Ctype.full_expand(env, ty))) {
    let (args, ret) = get_fn_allocation_type(env, ty);
    let args = List.map(wasm_repr_of_allocation_type, args);
    let rets =
      if (returns_void(env, ty)) {
        [];
      } else {
        [wasm_repr_of_allocation_type(ret)];
      };
    ReprFunction(args, rets, Unknown);
  } else {
    ReprValue(wasm_repr_of_allocation_type(get_allocation_type(env, ty)));
  };
