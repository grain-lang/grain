
type config_opt =
  | Opt : ('a ref * 'a) -> config_opt

type saved_config_opt =
  | SavedOpt : ('a ref * 'a) -> saved_config_opt

type config = saved_config_opt list

(* Here we model the API provided by cmdliner without introducing
   an explicit dependency on it (that's left to grainc). *)
type config_info = {
  docs  : string option; (** Title of the man page section to place arg under *)
  docv  : string option; (** Variable name for non-flags *)
  doc   : string option; (** Man page info for argument (see cmdliner documentation) *)
  env   : string option; (** Name of environment variable used to get default value. *)
  names : string list;   (** Names which the flag can be referred to by *)
}

type config_spec =
  | Spec : 'a Cmdliner.Arg.t * ('a ref) -> config_spec


let opts : config_opt list ref = ref []
let specs : config_spec list ref = ref []

let internal_opt : 'a. 'a -> 'a ref = fun v ->
  let cur = ref v in
  opts := (Opt(cur, v))::!opts;
  cur

let arg_info : ?docs:string -> ?docv:string -> ?doc:string -> ?env_docs:string -> ?env_doc:string -> ?env:string -> names:string list -> Cmdliner.Arg.info =
  fun ?docs ?docv ?doc ?env_docs ?env_doc ?env ~names ->
    let env = Option.map (fun e ->
        let doc, docs = env_doc, env_docs in
        Cmdliner.Arg.(env_var ?docs ?doc e)) env in
    Cmdliner.Arg.(info ?docs ?docv ?doc ?env names)

let opt : 'a. ?docs:string -> ?docv:string -> ?doc:string -> ?env_docs:string -> ?env_doc:string -> ?env:string -> names:string list -> conv:'a Cmdliner.Arg.conv -> 'a -> 'a ref =
  fun ?docs ?docv ?doc ?env_docs ?env_doc ?env ~names ~conv:c v ->
  let cur = internal_opt v in
  specs := (Spec(Cmdliner.Arg.(opt c v (arg_info ?docs ?docv ?doc ?env_docs ?env_doc ?env ~names)), cur))::!specs;
  cur

let flag : 'a . names:('a * Cmdliner.Arg.info) list -> default:'a -> 'a ref =
  fun ~names ~default:v ->
    let cur = internal_opt v in
    specs := (Spec(Cmdliner.Arg.(vflag v names), cur))::!specs;
    cur

let bool_flag : ?true_info:Cmdliner.Arg.info -> ?false_info:Cmdliner.Arg.info -> 'a -> 'a ref =
  fun ?true_info ?false_info default ->
    let names = ref [] in
    Option.may (fun i -> names := (true, i)::!names) true_info;
    Option.may (fun i -> names := (false, i)::!names) false_info;
    match !names with
    | [] -> failwith "Internal error: bool_flag called with no info"
    | names ->
      flag ~names ~default

let toggle_flag : ?docs:string -> ?docv:string -> ?doc:string -> ?env_docs:string -> ?env_doc:string -> ?env:string -> names:string list -> bool -> bool ref =
  fun ?docs ?docv ?doc ?env_docs ?env_doc ?env ~names default ->
    let cur = internal_opt default in
    specs := (Spec(Cmdliner.Arg.(vflag default [(not default, arg_info ?docs ?docv ?doc ?env_docs ?env_doc ?env ~names)]), cur))::!specs;
    cur

let save_config() =
  let single_save = function
    | Opt(cur, _) -> SavedOpt(cur, !cur) in
  List.map single_save !opts

let restore_config =
  let single_restore = function
    | SavedOpt(ptr, value) -> ptr := value in
  List.iter single_restore

let reset_config() =
  let single_reset = function
    | Opt(cur, default) -> cur := default in
  List.iter single_reset !opts

let with_config c thunk =
  (* Possible optimization: Only save the delta *)
  let saved = save_config() in
  try
    restore_config c;
    let r = thunk() in
    restore_config saved;
    r
  with exn -> restore_config saved; raise exn

let preserve_config thunk =
  let saved = save_config() in
  try
    let r = thunk() in
    restore_config saved;
    r
  with exn -> restore_config saved; raise exn

let with_cli_options (term : 'a) : 'a Cmdliner.Term.t =
  let open Cmdliner in
  let open Term in
  let process_option acc =
    function
    | Spec(arg, box) ->
      (const (fun a b -> box := a; b)) $ (Arg.value arg) $ acc in
  let folded = List.fold_left process_option (const term) !specs in
  folded

let option_conv (prsr, prntr) =
  (fun x -> match prsr x with
     | `Ok a -> `Ok (Some a)
     | `Error a -> `Error a),
  (fun ppf -> function
     | None -> Format.fprintf ppf "<not set>"
     | Some(x) -> prntr ppf x)

let output_enabled = internal_opt true

let optimizations_enabled = toggle_flag
    ~doc:"Disable optimizations."
    ~names:["O0"]
    true

let include_dirs = opt
    ~names:["I"]
    ~conv:Cmdliner.Arg.(list dir)
    ~doc:"Extra library include directories"
    ~docv:"DIR"
    []

let grain_root = opt
    ~names:["grain-root"]
    ~conv:(option_conv (Cmdliner.Arg.dir))
    ~doc:"Root directory for grain installation"
    ~docv:"ROOT"
    ~env:"GRAIN_ROOT"
    None

let use_stdlib = toggle_flag
    ~names:["no-stdlib"]
    ~doc:"Disable the grain standard library"
    true

let color_enabled = toggle_flag
    ~names:["no-color"]
    ~doc:"Disable colored output"
    true

let principal = toggle_flag
    ~names:["principal-types"]
    ~doc:"Enable principal types"
    false

let recursive_types = toggle_flag
    ~names:["recursive-types"]
    ~doc:"Enable recursive types"
    false

let strict_sequence = toggle_flag
    ~names:["strict-sequence"]
    ~doc:"Enable strict sequencing"
    false

let parser_debug_level = opt
    ~names:["parser-debug-level"]
    ~conv:Cmdliner.Arg.int
    ~doc:"Debugging level for parser output"
    0

let debug = toggle_flag
    ~names:["g"]
    ~doc:"Compile with debugging information"
    false

let verbose = toggle_flag
    ~names:["cdebug"]
    ~doc:"Print internal debug messages"
    false

let sexp_locs_enabled = toggle_flag
    ~names:["hide-locs"]
    ~doc:"Hide locations from intermediate trees. Only has an effect with `--cdebug'."
    true

let unsound_optimizations = toggle_flag
    ~names:["Ounsound"]
    ~doc:"Compile with optimizations which may remove runtime errors"
    false

(* To be filled in by grainc *)
let base_path = internal_opt ""

let stdlib_directory() : string option =
  let open BatPathGen.OfString in
    let open Infix in
    !grain_root
        |> Option.map (fun root ->
              to_string @@ (of_string root) /: "lib" /: "grain" /: "stdlib")

let module_search_path() =
  match (stdlib_directory()) with
  | Some(x) when !use_stdlib -> (!base_path) :: (!include_dirs) @ [x] (* stdlib goes last *)
  | Some _
  | None -> (!base_path) :: (!include_dirs)

