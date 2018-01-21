
type config_opt =
  | Opt : ('a ref * 'a) -> config_opt

type saved_config_opt =
  | SavedOpt : ('a ref * 'a) -> saved_config_opt

type config = saved_config_opt list

let opts : config_opt list ref = ref []
let opt : 'a. 'a -> 'a ref = fun v ->
  let cur = ref v in
  opts := (Opt(cur, v))::!opts;
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

let verbose = opt false
let optimizations_enabled = opt true
let include_dirs = opt []
let use_stdlib = opt true
let color_enabled = opt true
let principal = opt false
let recursive_types = opt false
