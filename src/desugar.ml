open Legacy_types

let desugar_program : 'a. 'a program -> 'a program = fun p ->
  let {statements; body} = p in
  let help stmt (includes, data, acc_body) =
    match stmt with
    | SInclude(name, a) -> ((name, a)::includes, data, acc_body)
    | SLet(binds, a) -> (includes, data, ELet(binds, acc_body, a))
    | SLetRec(binds, a) -> (includes, data, ELetRec(binds, acc_body, a))
    | SDataDecl(_, _, _, _) -> (includes, stmt::data, acc_body) in
  let (includes, data, acc_body) = List.fold_right help statements ([], [], body) in
  let help_include (name, a) acc = EInclude(name, acc, a) in
  let acc_body = List.fold_right help_include includes acc_body in
  {statements=data; body=acc_body}
