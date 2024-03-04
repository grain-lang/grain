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
let rec resolve_unsafe_type = ({exp_type}) => {
  let rec type_is_wasm_unsafe = t => {
    switch (t.desc) {
    | TTyConstr(path, _, _) =>
      switch (path) {
      | t when t == Builtin_types.path_wasmi32 => "I32"
      | t when t == Builtin_types.path_wasmi64 => "I64"
      | t when t == Builtin_types.path_wasmf32 => "F32"
      | t when t == Builtin_types.path_wasmf64 => "I64"
      | _ => failwith("Impossible: type cannot be a wasm unsafe value")
      }
    | TTyLink(t) => type_is_wasm_unsafe(t)
    | _ => failwith("Impossible: type cannot be a wasm unsafe value")
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
  // Remove types provided after initial type definition (with `provide { type X }` statement)
  let private_idents =
    List.fold_left(
      (private_idents, sig_) => {
        switch (sig_) {
        | Types.TSigType(id, _, _) =>
          switch (Ident.Set.find_first_opt(Ident.equal(id), private_idents)) {
          | Some(to_remove) => Ident.Set.remove(to_remove, private_idents)
          | None => private_idents
          }
        | _ => private_idents
        }
      },
      private_idents,
      signature,
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
          let typeName =
            switch (args) {
            | [(_, arg), _] when exp_is_wasm_unsafe(arg) =>
              "Wasm" ++ resolve_unsafe_type(arg)
            | _ => "(WasmI32 | WasmI64 | WasmF32 | WasmF64)"
            };
          let warning =
            Grain_utils.Warnings.FuncWasmUnsafe(
              Printf.sprintf("Pervasives.(%s)", func),
              Printf.sprintf("%s.(%s)", typeName, func),
              typeName,
            );
          if (Grain_utils.Warnings.is_active(warning)) {
            Grain_parsing.Location.prerr_warning(exp_loc, warning);
          };
        }
      // Check: Warn if using Pervasives print on WasmXX types
      | TExpApp(
          {
            exp_desc:
              TExpIdent(
                Path.PExternal(Path.PIdent({name: "Pervasives"}), "print"),
                _,
                _,
              ),
          },
          _,
          args,
        ) =>
        switch (List.find_opt(((_, arg)) => exp_is_wasm_unsafe(arg), args)) {
        | Some((_, arg)) =>
          let typeName = resolve_unsafe_type(arg);
          let warning = Grain_utils.Warnings.PrintUnsafe(typeName);
          if (Grain_utils.Warnings.is_active(warning)) {
            Grain_parsing.Location.prerr_warning(exp_loc, warning);
          };
        | _ => ()
        }
      // Check: Warn if using Pervasives toString on WasmXX types
      | TExpApp(
          {
            exp_desc:
              TExpIdent(
                Path.PExternal(
                  Path.PIdent({name: "Pervasives"}),
                  "toString",
                ),
                _,
                _,
              ),
          },
          _,
          args,
        ) =>
        switch (List.find_opt(((_, arg)) => exp_is_wasm_unsafe(arg), args)) {
        | Some((_, arg)) =>
          let typeName = resolve_unsafe_type(arg);
          let warning = Grain_utils.Warnings.ToStringUnsafe(typeName);
          if (Grain_utils.Warnings.is_active(warning)) {
            Grain_parsing.Location.prerr_warning(exp_loc, warning);
          };
        | _ => ()
        }
      // Check: Warn if using XXXX.fromNumber(<literal>)
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
          [(_, {exp_desc: TExpConstant(Const_number(n))})],
        )
          when
            modname == "Int8"
            || modname == "Int16"
            || modname == "Int32"
            || modname == "Int64"
            || modname == "Uint8"
            || modname == "Uint16"
            || modname == "Uint32"
            || modname == "Uint64"
            || modname == "Float32"
            || modname == "Float64"
            || modname == "Rational"
            || modname == "BigInt" =>
        // NOTE: Due to type-checking, we shouldn't need to worry about ending up with a FloatXX value and a Const_number_float
        let n_str =
          switch (n) {
          | Const_number_int(nint) => Int64.to_string(nint)
          | Const_number_float(nfloat) => Float.to_string(nfloat)
          | Const_number_rational({rational_num_rep, rational_den_rep}) =>
            Printf.sprintf("%s/%s", rational_num_rep, rational_den_rep)
          | Const_number_bigint({bigint_rep}) => bigint_rep
          };
        let mod_type =
          switch (modname) {
          | "Int8" => Grain_utils.Warnings.Int8
          | "Int16" => Grain_utils.Warnings.Int16
          | "Int32" => Grain_utils.Warnings.Int32
          | "Int64" => Grain_utils.Warnings.Int64
          | "Uint8" => Grain_utils.Warnings.Uint8
          | "Uint16" => Grain_utils.Warnings.Uint16
          | "Uint32" => Grain_utils.Warnings.Uint32
          | "Uint64" => Grain_utils.Warnings.Uint64
          | "Float32" => Grain_utils.Warnings.Float32
          | "Float64" => Grain_utils.Warnings.Float64
          | "Rational" => Grain_utils.Warnings.Rational
          | "BigInt" => Grain_utils.Warnings.BigInt
          | _ => failwith("Impossible")
          };
        let warning =
          Grain_utils.Warnings.FromNumberLiteral(mod_type, modname, n_str);
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
          && !
               Grain_utils.Config.(
                 no_gc^ || compilation_mode^ == Runtime || is_unsafe()
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
