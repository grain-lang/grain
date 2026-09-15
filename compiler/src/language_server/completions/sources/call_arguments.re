open Grain_typed;
open Grain_parsing;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let rec function_type_params = (env, ty) => {
  let ty = Ctype.expand_head(env, ty);
  switch (ty.desc) {
  | TTyArrow(params, ret, _) => params @ function_type_params(env, ret)
  | _ => []
  };
};

let label_explicitly_specified = arg =>
  switch (arg.Typedtree.arg_label, arg.arg_label_specified) {
  | (Typedtree.Labeled(_) | Typedtree.Default(_), true) => true
  | _ => false
  };

let rec consume_next_positional = params =>
  switch (params) {
  | [] => (None, [])
  | [(label, _), ...rest] when Btype.is_optional(label) =>
    consume_next_positional(rest)
  | [param, ...rest] => (Some(fst(param)), rest)
  };

let rec remaining_params_after_args = (params, args) =>
  switch (args) {
  | [] => params
  | [arg, ...rest_args] =>
    let params =
      if (label_explicitly_specified(arg)) {
        let name = Btype.label_name(arg.arg_label);
        List.filter(
          ((label, _)) => Btype.label_name(label) != name,
          params,
        );
      } else {
        let (_, params) = consume_next_positional(params);
        params;
      };
    remaining_params_after_args(params, rest_args);
  };

let remaining_named_params = (env, fun_ty, provided_args) => {
  let params = function_type_params(env, fun_ty);
  let params = remaining_params_after_args(params, provided_args);
  List.filter(((label, _)) => Btype.label_name(label) != "", params);
};

let labeled_argument_candidate = (~context, label, ty) => {
  let name = Btype.label_name(label);
  make_candidate(
    ~source=Typedtree,
    ~label=name,
    ~kind=Property,
    ~detail=
      Printf.sprintf(
        "%s: %s",
        Btype.qualified_label_name(label),
        Printtyp.string_of_type_scheme(ty),
      ),
    ~sort_group=Sort_group.Typed,
    ~filter_text=name,
    ~insert_text=name ++ "=",
    ~context,
    (),
  );
};

let argument_candidates =
    (
      ~context,
      ~env,
      ~callee_fallback,
      application:
        option((Typedtree.expression, list(Typedtree.argument_value))),
    ) => {
  let from_application =
    switch (application) {
    | None => []
    | Some((fun_expr, args)) =>
      remaining_named_params(env, fun_expr.Typedtree.exp_type, args)
      |> List.map(((label, ty)) =>
           labeled_argument_candidate(~context, label, ty)
         )
    };
  switch (from_application) {
  | [] =>
    switch (callee_fallback) {
    | None => []
    | Some(callee) =>
      try({
        let (_, desc) =
          Env.lookup_value(~mark=false, Identifier.parse(callee), env);
        function_type_params(env, desc.Types.val_type)
        |> List.filter_map(((label, ty)) => {
             let name = Btype.label_name(label);
             if (name != "" && Btype.is_optional(label)) {
               Some(labeled_argument_candidate(~context, label, ty));
             } else {
               None;
             };
           });
      }) {
      | Not_found => []
      | exn =>
        Trace.log(
          "Completion call_arguments failed for callee `"
          ++ callee
          ++ "`: "
          ++ Printexc.to_string(exn),
        );
        [];
      }
    }
  | candidates => candidates
  };
};
