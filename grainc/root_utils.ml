open Grain
open Filename
open Printf
open BatPathGen.OfString

let make_absolute d =
  if is_absolute d then
    d
  else
    normalize @@ concat (of_string (Unix.getcwd())) d

let could_be_grain_root d =
  let open Infix in
  List.for_all (fun x -> Sys.file_exists (to_string @@ (of_string d) /: x))
    ["bin"; "lib"]

let try_infer_grain_root grainc_path =
  if Sys.file_exists grainc_path then begin
    try
      let grainc_dir = parent @@ of_string grainc_path in
      let as_abs = make_absolute grainc_dir in
      let parent = to_string @@ parent as_abs in
      let could_be = could_be_grain_root parent in
      if could_be then
        Grain_utils.Config.grain_root := Some(parent);
      could_be
    with _ -> false
  end
  else
    false

let path_var_sep = if Sys.os_type = "Win32" then ';' else ':'

let infer_root_from_argv() =
  match Sys.argv.(0) with
  | x when not((BatString.ends_with x "grainc") || (BatString.ends_with x "grain-root")) -> false
  | exec_path when exec_path <> "grainc" && exec_path <> "grain-root" ->
    (* Ends with 'grainc' and isn't 'grainc'. Likely an absolute or relative path *)
    try_infer_grain_root exec_path
  | _ -> (* argv[0] is exactly 'grainc'. Look for it on $PATH *)
    let path_var = Sys.getenv_opt "PATH" in
    match path_var with
    | None ->
      prerr_string "WARNING: When locating grain root, we found that there \
                    is no PATH environment variable. This seems strange!\n";
      false
    | Some(path) ->
      (* Got the PATH variable. Check all directories *)
      let grainc_path dir =
        let open Infix in
        to_string @@ (of_string dir) /: "grainc" in
      let path_dirs = String.split_on_char path_var_sep path in
      (List.exists try_infer_grain_root (List.map grainc_path path_dirs)) || begin
        (* Last resort. Check if 'grainc' is in the cwd. *)
        let open Infix in
        let cwd = Sys.getcwd() in
        let grainc_path = to_string @@ (of_string cwd) /: "grainc" in
        try_infer_grain_root grainc_path
      end


let infer_root_from_running_grainc() =
  try
    let pid = Unix.getpid() in
    let grainc_path = Unix.readlink (sprintf "/proc/%d/exe" pid) in
    try_infer_grain_root grainc_path
  with _ ->
    false

let infer_root() =
  let found = infer_root_from_argv() || infer_root_from_running_grainc() in
  if not found then
    prerr_string "Failed to find Grain installation root. \
                  Please set the GRAIN_ROOT environment variable \
                  in order to use the standard library.\n"
  else if !Grain_utils.Config.verbose then
    prerr_string (Printf.sprintf "Grain Root: %s\n" (Option.get !Grain_utils.Config.grain_root))

let infer_root_if_needed() =
  match !Grain_utils.Config.grain_root with
  (* Prefer environment variable over inferred path *)
  | Some(_) -> ()
  | None ->
    infer_root()
