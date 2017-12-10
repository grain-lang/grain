
let grain_root = ref @@ Sys.getenv_opt "GRAIN_ROOT"

let get_grain_root() = !grain_root

let set_grain_root root = grain_root := Some(root)
