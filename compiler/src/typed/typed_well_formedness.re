open Grain_parsing;
open Types;
open Typedtree;
open TypedtreeIter;

/*
 * This module provides well-formedness checks which are type/value resolution-dependent.
 */

type item =
  | Value(Location.t)
  | Enum(Location.t)
  | Record(Location.t)
  | Type(Location.t)
  | Module(Location.t);

type error =
  | WasmOutsideDisableGc
  | EscapedType(item, string)
  | EscapedModuleType(item, string, string);
exception Error(Location.t, error);

let wasm_unsafe_types = [
  Builtin_types.path_wasmi32,
  Builtin_types.path_wasmi64,
  Builtin_types.path_wasmf32,
  Builtin_types.path_wasmf64,
];

let rec exp_is_wasm_unsafe = ({exp_type}) => {
  let rec type_is_wasm_unsafe = t => {
    switch (t.desc) {
    | TTyConstr(path, _, _) => List.mem(path, wasm_unsafe_types)
    | TTyLink(t) => type_is_wasm_unsafe(t)
    | _ => false
    };
  };
  type_is_wasm_unsafe(exp_type);
};

let is_marked_unsafe = attrs => {
  // Disable_gc implies Unsafe
  List.exists(
    fun
    | {txt: Disable_gc}
    | {txt: Unsafe} => true
    | _ => false,
    attrs,
  );
};

let ensure_no_escaped_types = (signature, statements) => {
  let private_idents =
    List.fold_left(
      (private_idents, stmt) => {
        switch (stmt.ttop_desc) {
        | TTopData(decls) =>
          List.fold_left(
            (private_idents, decl) => {
              switch (decl.data_provided) {
              | Provided
              | Abstract => private_idents
              | NotProvided => Ident.Set.add(decl.data_id, private_idents)
              }
            },
            private_idents,
            decls,
          )
        | TTopModule({tmod_id, tmod_provided: NotProvided}) =>
          Ident.Set.add(tmod_id, private_idents)
        | _ => private_idents
        }
      },
      Ident.Set.empty,
      statements,
    );
  let ctx_loc = ctx => {
    switch (ctx) {
    | Value(loc)
    | Enum(loc)
    | Record(loc)
    | Type(loc)
    | Module(loc) => loc
    };
  };
  let rec check_type = (ctx, ty) => {
    let check_type = check_type(ctx);
    switch (ty.desc) {
    | TTyVar(_)
    | TTyUniVar(_) => ()
    | TTyArrow(args, res, _) =>
      List.iter(((_, arg)) => check_type(arg), args);
      check_type(res);
    | TTyTuple(args) => List.iter(check_type, args)
    | TTyRecord(fields) =>
      List.iter(((_, arg)) => check_type(arg), fields)
    | TTyConstr(PIdent(id) as p, vars, _) =>
      if (Ident.Set.mem(id, private_idents)) {
        raise(Error(ctx_loc(ctx), EscapedType(ctx, Path.name(p))));
      };
      List.iter(check_type, vars);
    | TTyConstr(PExternal(_) as p, vars, _) =>
      let mod_id = Path.head(p);
      if (Ident.Set.mem(mod_id, private_idents)) {
        raise(
          Error(
            ctx_loc(ctx),
            EscapedModuleType(ctx, Path.name(p), Ident.name(mod_id)),
          ),
        );
      };
      List.iter(check_type, vars);
    | TTyPoly(ty, vars) =>
      check_type(ty);
      List.iter(check_type, vars);
    | TTyLink(ty)
    | TTySubst(ty) => check_type(ty)
    };
  };
  let rec check_signature_item = (ctx, item) => {
    let apply_ctx = ty => Option.value(~default=ty, ctx);
    switch (item) {
    | TSigValue(_, {val_type, val_loc}) =>
      check_type(apply_ctx(Value(val_loc)), val_type)
    | TSigType(_, {type_kind, type_params, type_manifest, type_loc}, _) =>
      switch (type_kind) {
      | TDataVariant(cstrs) =>
        List.iter(check_type(apply_ctx(Enum(type_loc))), type_params);
        List.iter(
          ({Types.cd_args, cd_res}) => {
            switch (cd_args) {
            | TConstrTuple(args) =>
              List.iter(check_type(apply_ctx(Enum(type_loc))), args)
            | TConstrRecord(fields) =>
              List.iter(
                ({Types.rf_type}) =>
                  check_type(apply_ctx(Enum(type_loc)), rf_type),
                fields,
              )
            | TConstrSingleton => ()
            };
            Option.iter(check_type(apply_ctx(Enum(type_loc))), cd_res);
          },
          cstrs,
        );
        Option.iter(check_type(apply_ctx(Enum(type_loc))), type_manifest);
      | TDataRecord(fields) =>
        List.iter(
          ({Types.rf_type}) => check_type(Record(type_loc), rf_type),
          fields,
        );
        Option.iter(check_type(Record(type_loc)), type_manifest);
      | TDataAbstract
      | TDataOpen =>
        List.iter(check_type(Type(type_loc)), type_params);
        Option.iter(check_type(Type(type_loc)), type_manifest);
      }
    | TSigModule(_, {md_type, md_loc}, _) =>
      switch (md_type) {
      | TModIdent(_)
      | TModAlias(_) => ()
      | TModSignature(signature) =>
        List.iter(check_signature_item(Some(Module(md_loc))), signature)
      }
    | _ => ()
    };
  };
  List.iter(check_signature_item(None), signature);
};

let make_bool_stack = () => {
  let stack = ref([]);
  let push = x => stack := [x, ...stack^];
  let pop = () => {
    switch (stack^) {
    | [] => failwith("Impossible: make_bool_stack > pop")
    | [hd, ...tl] => stack := tl
    };
  };
  let pred = () => {
    switch (stack^) {
    | [] => false
    | [hd, ..._] => hd
    };
  };
  let reset = () => stack := [];
  // For debugging:
  let dump = () =>
    "["
    ++ String.concat(
         ", ",
         List.map(x => if (x) {"true"} else {"false"}, stack^),
       )
    ++ "]";
  (push, pop, pred, reset, dump);
};

let (
  push_in_lambda,
  pop_in_lambda,
  is_in_lambda,
  reset_in_lambda,
  dump_in_lambda,
) =
  make_bool_stack();
let (push_unsafe, pop_unsafe, is_unsafe, reset_unsafe, dump_unsafe) =
  make_bool_stack();

module WellFormednessArg: TypedtreeIter.IteratorArgument = {
  include TypedtreeIter.DefaultIteratorArgument;

  let enter_expression: expression => unit =
    ({exp_desc, exp_loc, exp_attributes} as exp) => {
      // Check: Avoid using Pervasives equality ops with WasmXX types
      switch (exp_desc) {
      | TExpLet(_) when is_marked_unsafe(exp_attributes) => push_unsafe(true)
      | TExpApp(
          {
            exp_desc:
              TExpIdent(
                Path.PExternal(Path.PIdent({name: "Pervasives"}), func),
                _,
                _,
              ),
          },
          _,
          args,
        )
          when func == "==" || func == "!=" =>
        if (List.exists(((_, arg)) => exp_is_wasm_unsafe(arg), args)) {
          let warning =
            Grain_utils.Warnings.FuncWasmUnsafe(
              Printf.sprintf("Pervasives.(%s)", func),
            );
          if (Grain_utils.Warnings.is_active(warning)) {
            Grain_parsing.Location.prerr_warning(exp_loc, warning);
          };
        }
      // Check: Warn if using Int32.fromNumber(<literal>)
      | TExpApp(
          {
            exp_desc:
              TExpIdent(
                Path.PExternal(Path.PIdent({name: modname}), "fromNumber"),
                _,
                _,
              ),
          },
          _,
          [
            (
              Unlabeled,
              {
                exp_desc:
                  TExpConstant(
                    Const_number(
                      (Const_number_int(_) | Const_number_float(_)) as n,
                    ),
                  ),
              },
            ),
          ],
        )
          when
            modname == "Int32"
            || modname == "Int64"
            || modname == "Uint32"
            || modname == "Uint64"
            || modname == "Float32"
            || modname == "Float64" =>
        // NOTE: Due to type-checking, we shouldn't need to worry about ending up with a FloatXX value and a Const_number_float
        let n_str =
          switch (n) {
          | Const_number_int(nint) => Int64.to_string(nint)
          | Const_number_float(nfloat) => Float.to_string(nfloat)
          | _ => failwith("Impossible")
          };
        let warning =
          switch (modname) {
          | "Int32" => Grain_utils.Warnings.FromNumberLiteralI32(n_str)
          | "Int64" => Grain_utils.Warnings.FromNumberLiteralI64(n_str)
          | "Uint32" => Grain_utils.Warnings.FromNumberLiteralU32(n_str)
          | "Uint64" => Grain_utils.Warnings.FromNumberLiteralU64(n_str)
          | "Float32" => Grain_utils.Warnings.FromNumberLiteralF32(n_str)
          | "Float64" => Grain_utils.Warnings.FromNumberLiteralF64(n_str)
          | _ => failwith("Impossible")
          };
        if (Grain_utils.Warnings.is_active(warning)) {
          Grain_parsing.Location.prerr_warning(exp_loc, warning);
        };
      | _ => ()
      };
      // Check: Forbid usage of WasmXX types outside of disableGC context
      switch (exp_desc) {
      | TExpLambda(_) => push_in_lambda(true)
      | _ => ()
      };
      // For now, we only raise the error inside of functions.
      if (exp_is_wasm_unsafe(exp)
          && !(
               Grain_utils.Config.no_gc^
               || Grain_utils.Config.compilation_mode^ == Some("runtime")
               || is_unsafe()
             )) {
        raise(Error(exp_loc, WasmOutsideDisableGc));
      };
    };

  let enter_toplevel_stmt = ({ttop_desc, ttop_attributes}) => {
    switch (ttop_desc) {
    | TTopLet(_) => push_unsafe(is_marked_unsafe(ttop_attributes))
    | _ => ()
    };
  };

  let leave_expression = ({exp_desc, exp_attributes}) => {
    switch (exp_desc) {
    | TExpLet(_) when is_marked_unsafe(exp_attributes) => pop_unsafe()
    | TExpLambda(_) => pop_in_lambda()
    | _ => ()
    };
  };

  let leave_toplevel_stmt = ({ttop_desc, ttop_env}) => {
    switch (ttop_desc) {
    | TTopLet(_) => pop_unsafe()
    | TTopModule({
        tmod_decl: {md_type: TModSignature(signature)},
        tmod_statements,
      }) =>
      ensure_no_escaped_types(signature, tmod_statements)
    | _ => ()
    };
  };

  let leave_typed_program = ({signature, statements}) => {
    ensure_no_escaped_types(signature.cmi_sign, statements);
  };
};

module WellFormednessIterator = TypedtreeIter.MakeIterator(WellFormednessArg);

let check_well_formedness = program => {
  WellFormednessIterator.iter_typed_program(program);
};

open Format;

let print_item = ppf =>
  fun
  | Value(_) => fprintf(ppf, "value")
  | Enum(_) => fprintf(ppf, "enum")
  | Record(_) => fprintf(ppf, "record")
  | Type(_) => fprintf(ppf, "type")
  | Module(_) => fprintf(ppf, "module");

let report_error = ppf =>
  fun
  | WasmOutsideDisableGc =>
    fprintf(
      ppf,
      "Wasm types cannot be used outside of an @unsafe or @disableGC context@.",
    )
  | EscapedType(item, ty) =>
    fprintf(
      ppf,
      "This %a is provided but contains type %s, which is neither provided nor abstract.",
      print_item,
      item,
      ty,
    )
  | EscapedModuleType(item, ty, mod_) =>
    fprintf(
      ppf,
      "This %a is provided and has type %s, but module %s is not provided.",
      print_item,
      item,
      ty,
      mod_,
    );

let () =
  Location.register_error_of_exn(
    fun
    | Error(loc, err) =>
      Some(Location.error_of_printer(loc, report_error, err))
    | _ => None,
  );
