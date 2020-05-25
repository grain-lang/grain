let filename_to_module_name fname =
  let (path_without_ext, _ext) = Core_kernel.Filename.split_extension fname in
  try
    path_without_ext
  with Invalid_argument s ->
    raise (Invalid_argument (Printf.sprintf "%s (fname: '%s')" s fname))

let ensure_parent_directory_exists fname =
  try
    let pdir = Core_kernel.Filename.dirname fname in
    try
      ignore(Sys.is_directory (pdir))
    with Sys_error _ ->
      Unix.mkdir (pdir) 0o755
  with Invalid_argument s ->
    (* File appears to be in CWD. Eat the exception *)
    ()

(** Converts the given path to an absolute path. Relative paths will be
    treated as relative to [base], if given; otherwise, they will be
    assumed to be relative to the current working directory. *)
let derelativize ?base fname =
  let path = if Core_kernel.Filename.is_absolute fname then
      fname
    else begin
      let base = match base with
        | None -> Sys.getcwd()
        | Some(path) -> path
      in
      Core_kernel.Filename.concat base fname
    end
  in
  Core_kernel.Filename.of_parts (Core_kernel.Filename.parts path)
