let filename_to_module_name fname =
  let open BatPathGen.OfString in
  let path = of_string fname in
  try
    name_core path
  with Invalid_argument s ->
    raise (Invalid_argument (Printf.sprintf "%s (fname: '%s')" s fname))

let ensure_parent_directory_exists fname =
  let open BatPathGen.OfString in
  let path = of_string fname in
  try
    let pdir = parent path in
    try
      ignore(Sys.is_directory (to_string pdir))
    with Sys_error _ ->
      Unix.mkdir (to_string pdir) 0o755
  with Invalid_argument s ->
    (* File appears to be in CWD. Eat the exception *)
    ()

(** Converts the given path to an absolute path. Relative paths will be
    treated as relative to [base], if given; otherwise, they will be
    assumed to be relative to the current working directory. *)
let derelativize ?base fname =
  let open BatPathGen.OfString in
  let path = of_string fname in
  let path = if is_absolute path then
      path
    else begin
      let base = of_string @@ match base with
        | None -> Sys.getcwd()
        | Some(path) -> path
      in
      let open Operators in
      base //@ path
    end
  in
  to_string (normalize_filepath path)
