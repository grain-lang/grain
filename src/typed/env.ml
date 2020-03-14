(* This is a stripped-down version of OCaml's typing/env.ml module. The original copyright notice is reproduced below: *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Grain_parsing
open Grain_utils
open Sexplib.Conv
open Cmi_format
open Path
open Types

let add_delayed_check_forward = ref (fun _ -> assert false)

type type_descriptions = constructor_description list * label_description list
module PathMap = struct
  include Map.Make(Path)

  let sexp_of_t conv m =
    let sexp_of_path = Path.sexp_of_t in
    let open Sexplib in
    let pairs = List.map (fun (key, v) -> Sexp.List [sexp_of_path key; conv v]) (bindings m) in
    Sexp.List pairs

  let t_of_sexp conv sexp =
    let path_of_sexp = Path.t_of_sexp in
    let open Sexplib.Conv in
    let open Sexplib.Sexp in
    match sexp with
    | Atom str -> of_sexp_error "t_of_sexp: list needed" sexp
    | List sexprs ->
      let fields = List.map (function
          | List [key; value] -> (path_of_sexp key, conv value)
          | _ -> of_sexp_error "t_of_sexp: invalid field" sexp) sexprs in
      List.fold_left (fun acc (k, v) -> add k v acc) empty fields

end

let prefixed_sg = Hashtbl.create 113

type dependency_chain = (string Location.loc) list

type error =
  | Illegal_renaming of string * string * string
  | Inconsistent_import of string * string * string
  | Need_recursive_types of string * string
  | Depend_on_unsafe_string_unit of string * string
  | Missing_module of Location.t * Path.t * Path.t
  | Unbound_module of Location.t * string
  | Unbound_label of Location.t * string
  | No_module_file of string * string option
  | Value_not_found_in_module of Location.t * string * string
  | Illegal_value_name of Location.t * string
  | Cyclic_dependencies of string * dependency_chain

exception Error of error

let error err = raise (Error err)

module EnvLazy : sig
  type ('a,'b) t

  type log

  val force : ('a -> 'b) -> ('a,'b) t -> 'b
  val create : 'a -> ('a,'b) t
  val get_arg : ('a,'b) t -> 'a option

  (* [force_logged log f t] is equivalent to [force f t] but if [f] returns [None] then
     [t] is recorded in [log]. [backtrack log] will then reset all the recorded [t]s back
     to their original state. *)
  val log : unit -> log
  val force_logged : log -> ('a -> 'b option) -> ('a,'b option) t -> 'b option
  val backtrack : log -> unit

end  = struct

  type ('a,'b) t = ('a,'b) eval ref

  and ('a,'b) eval =
    | Done of 'b
    | Raise of exn
    | Thunk of 'a

  type undo =
    | Nil
    | Cons : ('a, 'b) t * 'a * undo -> undo

  type log = undo ref

  let force f x =
    match !x with
    | Done x -> x
    | Raise e -> raise e
    | Thunk e ->
        match f e with
        | y ->
          x := Done y;
          y
        | exception e ->
          x := Raise e;
          raise e

  let get_arg x =
    match !x with Thunk a -> Some a | _ -> None

  let create x =
    ref (Thunk x)

  let log () =
    ref Nil

  let force_logged log f x =
    match !x with
    | Done x -> x
    | Raise e -> raise e
    | Thunk e ->
      match f e with
      | None ->
          x := Done None;
          log := Cons(x, e, !log);
          None
      | Some _ as y ->
          x := Done y;
          y
      | exception e ->
          x := Raise e;
          raise e

  let backtrack log =
    let rec loop = function
      | Nil -> ()
      | Cons(x, e, rest) ->
          x := Thunk e;
          loop rest
    in
    loop !log

end

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_module of summary * Ident.t * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_open of summary * Path.t
  | Env_constraints of summary * type_declaration PathMap.t
  | Env_copy_types of summary * string list
[@@deriving sexp]

module TycompTbl = struct
  (** This module is used to store components of types (i.e. labels
      and constructors).  We keep a representation of each nested
      "open" and the set of local bindings between each of them. *)

  type 'a t = {
    current: 'a Ident.tbl;
    (** Local bindings since the last open. *)

    opened: 'a opened option;
    (** Symbolic representation of the last (innermost) open, if any. *)
  }

  and 'a opened = {
    components: (string, 'a list) Tbl.t;
    (** Components from the opened module. We keep a list of
        bindings for each name, as in comp_labels and
        comp_constrs. *)

    using: (string -> ('a * 'a) option -> unit) option;
    (** A callback to be applied when a component is used from this
        "open".  This is used to detect unused "opens".  The
        arguments are used to detect shadowing. *)

    next: 'a t;
    (** The table before opening the module. *)
  }

  let empty = { current = Ident.empty; opened = None }

  let add id x tbl =
    {tbl with current = Ident.add id x tbl.current}

  let add_open slot wrap components next =
    let using =
      match slot with
      | None -> None
      | Some f -> Some (fun s x -> f s (wrap x))
    in
    {
      current = Ident.empty;
      opened = Some {using; components; next};
    }

  let rec find_same id tbl =
    try Ident.find_same id tbl.current
    with Not_found as exn ->
      begin match tbl.opened with
        | Some {next; _} -> find_same id next
        | None -> raise exn
      end

  let nothing = fun () -> ()

  let mk_callback rest name desc = function
    | None -> nothing
    | Some f ->
      (fun () ->
         match rest with
         | [] -> f name None
         | (hidden, _) :: _ -> f name (Some (desc, hidden))
      )

  let rec find_all name tbl =
    List.map (fun (_id, desc) -> desc, nothing)
      (Ident.find_all name tbl.current) @
    match tbl.opened with
    | None -> []
    | Some {using; next; components} ->
      let rest = find_all name next in
      match Tbl.find name components with
      | exception Not_found -> rest
      | opened ->
        List.map
          (fun desc -> desc, mk_callback rest name desc using)
          opened
        @ rest

  let rec fold_name f tbl acc =
    let acc = Ident.fold_name (fun _id d -> f d) tbl.current acc in
    match tbl.opened with
    | Some {using = _; next; components} ->
      acc
      |> Tbl.fold
        (fun _name -> List.fold_right (fun desc -> f desc))
        components
      |> fold_name f next
    | None ->
      acc

  let rec local_keys tbl acc =
    let acc = Ident.fold_all (fun k _ accu -> k::accu) tbl.current acc in
    match tbl.opened with
    | Some o -> local_keys o.next acc
    | None -> acc

  let diff_keys is_local tbl1 tbl2 =
    let keys2 = local_keys tbl2 [] in
    List.filter
      (fun id ->
         is_local (find_same id tbl2) &&
         try ignore (find_same id tbl1); false
         with Not_found -> true)
      keys2

end


module IdTbl = struct
  (** This module is used to store all kinds of components except
      (labels and constructors) in environments.  We keep a
      representation of each nested "open" and the set of local
      bindings between each of them. *)

  type 'a t = {
    current: 'a Ident.tbl;
    (** Local bindings since the last open *)

    opened: 'a opened option;
    (** Symbolic representation of the last (innermost) open, if any. *)
  }

  and 'a opened = {
    root: Path.t;
    (** The path of the opened module, to be prefixed in front of
        its local names to produce a valid path in the current
        environment. *)

    components: (string, 'a * int) Tbl.t;
    (** Components from the opened module. *)

    using: (string -> ('a * 'a) option -> unit) option;
    (** A callback to be applied when a component is used from this
        "open".  This is used to detect unused "opens".  The
        arguments are used to detect shadowing. *)

    next: 'a t;
    (** The table before opening the module. *)
  }

  let empty = { current = Ident.empty; opened = None }

  let add id x tbl =
    {tbl with current = Ident.add id x tbl.current}

  let add_open slot wrap root components next =
    let using =
      match slot with
      | None -> None
      | Some f -> Some (fun s x -> f s (wrap x))
    in
    {
      current = Ident.empty;
      opened = Some {using; root; components; next};
    }

  let rec find_same id tbl =
    try Ident.find_same id tbl.current
    with Not_found as exn ->
      begin match tbl.opened with
        | Some {next; _} -> find_same id next
        | None -> raise exn
      end

  let rec find_name ~mark name tbl =
    try
      let (id, desc) = Ident.find_name name tbl.current in
      PIdent(id), desc
    with Not_found as exn ->
      begin match tbl.opened with
        | Some {using; root; next; components} ->
          begin try
              let (descr, pos) = Tbl.find name components in
              let res = PExternal(root, name, pos), descr in
              if mark then begin match using with
                | None -> ()
                | Some f ->
                  let tmp = (find_name ~mark:false name next) in
                  begin try f name (Some (snd tmp, snd res))
                    with Not_found -> f name None
                  end
              end;
              res
            with Not_found ->
              find_name ~mark name next
          end
        | None ->
          raise exn
      end

  let rec update name f tbl =
    try
      let (id, desc) = Ident.find_name name tbl.current in
      let new_desc = f desc in
      {tbl with current = Ident.add id new_desc tbl.current}
    with Not_found ->
      begin match tbl.opened with
        | Some {root; using; next; components} ->
          begin try
              let (desc, pos) = Tbl.find name components in
              let new_desc = f desc in
              let components = Tbl.add name (new_desc, pos) components in
              {tbl with opened = Some {root; using; next; components}}
            with Not_found ->
              let next = update name f next in
              {tbl with opened = Some {root; using; next; components}}
          end
        | None ->
          tbl
      end



  let rec find_all name tbl =
    List.map (fun (id, desc) -> PIdent(id), desc) (Ident.find_all name tbl.current) @
    match tbl.opened with
    | None -> []
    | Some {root; using = _; next; components} ->
      try
        let (desc, pos) = Tbl.find name components in
        (PExternal(root, name, pos), desc) :: find_all name next
      with Not_found ->
        find_all name next

  let rec fold_name f tbl acc =
    let acc = Ident.fold_name (fun id d -> f (Ident.name id) (PIdent(id), d)) tbl.current acc in
    match tbl.opened with
    | Some {root; using = _; next; components} ->
      acc
      |> Tbl.fold
        (fun name (desc, pos) -> f name (PExternal(root, name, pos), desc))
        components
      |> fold_name f next
    | None ->
      acc

  let rec local_keys tbl acc =
    let acc = Ident.fold_all (fun k _ accu -> k::accu) tbl.current acc in
    match tbl.opened with
    | Some o -> local_keys o.next acc
    | None -> acc


  let rec iter f tbl =
    Ident.iter (fun id desc -> f id (PIdent(id), desc)) tbl.current;
    match tbl.opened with
    | Some {root; using = _; next; components} ->
      Tbl.iter
        (fun s (x, pos) -> f (Ident.hide (Ident.create s) (* ??? *)) (PExternal(root, s, pos), x))
        components;
      iter f next
    | None -> ()

  let diff_keys tbl1 tbl2 =
    let keys2 = local_keys tbl2 [] in
    List.filter
      (fun id ->
         try ignore (find_same id tbl1); false
         with Not_found -> true)
      keys2

end

type 'a comp_tbl = (string, ('a * int)) Tbl.t

type t = {
  values: value_description IdTbl.t;
  types: (type_declaration * type_descriptions) IdTbl.t;
  constructors: constructor_description TycompTbl.t;
  labels: label_description TycompTbl.t;
  components: module_components IdTbl.t;
  modules: (Subst.t * module_declaration, module_declaration) EnvLazy.t IdTbl.t;
  modtypes: modtype_declaration IdTbl.t;
  local_constraints: type_declaration PathMap.t;
  summary: summary;
}

and module_component_components = {
  mutable comp_values: value_description comp_tbl;
  mutable comp_constrs: (string, constructor_description list) Tbl.t;
  mutable comp_labels: (string, label_description list) Tbl.t;
  mutable comp_types: (type_declaration * type_descriptions) comp_tbl;
  mutable comp_components: module_components comp_tbl;
  mutable comp_modules:
    (Subst.t * module_declaration, module_declaration) EnvLazy.t comp_tbl;
  mutable comp_modtypes: modtype_declaration comp_tbl;
}

and module_components = {
  loc: Location.t;
  comps:
  (t * Subst.t * Path.t * Types.module_type, module_component_components option) EnvLazy.t;
}

let empty = {
  values = IdTbl.empty;
  types = IdTbl.empty;
  components = IdTbl.empty;
  modules = IdTbl.empty;
  modtypes = IdTbl.empty;
  constructors = TycompTbl.empty;
  labels = TycompTbl.empty;
  local_constraints = PathMap.empty;
  summary = Env_empty;
}

let copy_local ~from env =
  { env with
    local_constraints = from.local_constraints; }

let same_constr = ref (fun _ _ _ -> assert false)

type can_load_modules =
  | Can_load_modules
  | Cannot_load_modules of EnvLazy.log

let can_load_modules = ref Can_load_modules

let without_cmis f x =
  let log = EnvLazy.log () in
  let res =
    Misc.(protect_refs
            [R (can_load_modules, Cannot_load_modules log)]
            (fun () -> f x))
  in
  EnvLazy.backtrack log;
  res

(* Forward declarations *)

let components_of_module' =
  ref ((fun ~deprecated:_ ~loc:_ _env _sub _path _mty -> assert false) :
         deprecated:string option -> loc:Location.t -> t -> Subst.t ->
       Path.t -> module_type ->
       module_components)
let components_of_module_maker' =
  ref ((fun (_env, _sub, _path, _mty) -> assert false) :
          t * Subst.t * Path.t * module_type -> module_component_components option)
let check_modtype_inclusion =
  (* to be filled with Includemod.check_modtype_inclusion *)
  ref ((fun ~loc:_ _env _mty1 _path1 _mty2 -> assert false) :
          loc:Location.t -> t -> module_type -> Path.t -> module_type -> unit)
let strengthen =
  (* to be filled with Mtype.strengthen *)
  ref ((fun ~aliasable:_ _env _mty _path -> assert false) :
         aliasable:bool -> t -> module_type -> Path.t -> module_type)

let md md_type md_filepath =
  {md_type; md_filepath; md_loc=Location.dummy_loc}

let subst_modtype_maker (subst, md) =
  if subst == Subst.identity then md
  else {md with md_type = Subst.modtype subst md.md_type}

let get_components_opt c =
  match !can_load_modules with
  | Can_load_modules ->
    EnvLazy.force !components_of_module_maker' c.comps
  | Cannot_load_modules log ->
    EnvLazy.force_logged log !components_of_module_maker' c.comps

let empty_structure =  {
  comp_values = Tbl.empty;
  comp_constrs = Tbl.empty;
  comp_labels = Tbl.empty;
  comp_types = Tbl.empty;
  comp_components = Tbl.empty;
  comp_modules = Tbl.empty;
  comp_modtypes = Tbl.empty;
}

let get_components c =
  match get_components_opt c with
  | None -> empty_structure
  | Some c -> c

let current_unit = ref ("", "")

let set_unit (name, source) =
  current_unit := (name, source)

let get_unit () =
  !current_unit


(* Persistent structure descriptions *)

type pers_flags = Cmi_format.pers_flags =
  | Rectypes
  | Opaque
  | Unsafe_string

type pers_struct =
  { ps_name: string;
    ps_sig: signature Lazy.t;
    ps_comps: module_components;
    ps_crcs: (string * Digest.t option) list;
    ps_filename: string;
    ps_flags: pers_flags list }

let persistent_structures =
  (Hashtbl.create 17 : (string, pers_struct option) Hashtbl.t)

let unit_to_file =
  (Hashtbl.create 17 : (string, string) Hashtbl.t)

let compilation_in_progress =
  (Hashtbl.create 17 : (string, string Location.loc) Hashtbl.t) (* (module, dependent) *)

(* Consistency between persistent structures *)

module Consistbl = Consistbl.Make (Misc.Stdlib.String)

let crc_units = Consistbl.create()

module StringSet =
  Set.Make(struct type t = string let compare = String.compare end)

let imported_units = ref StringSet.empty

let add_import s =
  imported_units := StringSet.add s !imported_units

let imported_opaque_units = ref StringSet.empty

let add_imported_opaque s =
  imported_opaque_units := StringSet.add s !imported_opaque_units

let clear_imports () =
  Consistbl.clear crc_units;
  imported_units := StringSet.empty;
  imported_opaque_units := StringSet.empty

let check_consistency ps =
  try
    List.iter
      (fun (name, crco) ->
         match crco with
            None -> ()
          | Some crc ->
              add_import name;
              Consistbl.check crc_units name crc ps.ps_filename)
      ps.ps_crcs;
  with Consistbl.Inconsistency(name, source, auth) ->
    error (Inconsistent_import(name, auth, source))

(* Reading persistent structures from .cmi files *)

let save_pers_struct crc ps =
  let modname = ps.ps_name in
  Hashtbl.add persistent_structures modname (Some ps);
  List.iter
    (function
        | Rectypes -> ()
        | Unsafe_string -> ()
        | Opaque -> add_imported_opaque modname)
    ps.ps_flags;
  Consistbl.set crc_units modname crc ps.ps_filename;
  add_import modname

let get_dependency_chain ~loc unit_name =
  let rec help filename =
    match Hashtbl.find_opt compilation_in_progress filename with
    | None -> []
    | Some(dep) when Hashtbl.mem unit_to_file dep.txt ->
      dep::(help (Hashtbl.find unit_to_file dep.txt))
    | Some(dep) -> [dep]
  in
  let nameloc = Location.mkloc unit_name loc in
  match Hashtbl.find_opt unit_to_file unit_name with
  | None -> [nameloc]
  | Some(fname) -> nameloc::(help fname)

let mark_in_progress ~loc unit_name sourcefile =
  if Hashtbl.mem compilation_in_progress sourcefile then
    error (Cyclic_dependencies(unit_name, get_dependency_chain ~loc unit_name));
  let stored_name, _ = get_unit() in
  Hashtbl.add compilation_in_progress sourcefile (Location.mkloc stored_name loc);
  Hashtbl.add unit_to_file unit_name sourcefile

let mark_completed unit_name sourcefile =
  Hashtbl.remove compilation_in_progress sourcefile;
  Hashtbl.remove unit_to_file unit_name


type module_location_result =
  | GrainModule of string * string option (* Grain Source file, Compiled object *)
  | WasmModule of string (* Compiled object *)

let compile_module_dependency = ref (fun filename output_file -> failwith "compile_module Should be filled in by compile.ml")

let file_older a b =
  let last_modified f =
    let open Unix in
    (stat f).st_mtime in
  (last_modified a) < (last_modified b)

let get_output_name name =
  let name = try
      Filename.chop_extension name
    with
    | Invalid_argument _ -> name in
  name ^ ".wasm"

let get_up_to_date ~loc unit_name = function
  | WasmModule(path) -> path
  | GrainModule(srcpath, Some(objpath)) when file_older srcpath objpath -> objpath
  | GrainModule(srcpath, _) ->
    let srcpath = Grain_utils.Files.derelativize srcpath in
    (* Note: This is a potential security issue? *)
    let outpath = get_output_name srcpath in
    mark_in_progress ~loc unit_name srcpath;
    let saved_unit = get_unit() in
    let saved = Ident.save_state() in
    (!compile_module_dependency) srcpath outpath;
    Ident.restore_state saved;
    set_unit saved_unit;
    mark_completed unit_name srcpath;
    outpath

let find_ext_in_dir dir name =
  let file_exists = Sys.file_exists in
  let fullname = Filename.concat dir name in
  let rec process_ext = function
    | [] -> None
    | ext :: _ when file_exists (fullname ^ ext) -> Some(fullname ^ ext, dir, name, ext)
    | _ :: tl -> process_ext tl in
  process_ext

let find_in_path_uncap ?exts:(exts=[]) path name =
  let rec try_dir = function
    | [] -> raise Not_found
    | dir::rem ->
      match find_ext_in_dir dir name exts with
      | Some(path) -> path
      | None -> try_dir rem
  in try_dir path

let locate_module path unit_name =
  let grain_src_exts = [".gr"] in
  match find_in_path_uncap ~exts:[".wasm"] path unit_name with
  | objpath, dir, basename, ext ->
    begin match find_ext_in_dir dir basename grain_src_exts with
      | Some(srcpath, _, _, _) -> GrainModule(srcpath, Some(objpath))
      | None -> WasmModule(objpath)
    end
  | exception Not_found ->
    let srcpath, _, _, _ = find_in_path_uncap ~exts:grain_src_exts path unit_name in
    GrainModule(srcpath, None)

let locate_module_file ~loc path unit_name =
  try
    get_up_to_date ~loc unit_name (locate_module path unit_name)
  with Not_found ->
    error (No_module_file (unit_name, None))

let resolutions = Hashtbl.create 12

let resolve_unit unit_name =
  try 
    Hashtbl.find resolutions unit_name
  with Not_found ->
    let path = Grain_utils.Config.module_search_path() in
    let exts = [".gr"; ".wasm"] in
    let _, dir, basename, _ = find_in_path_uncap ~exts path unit_name in
    let resolution = Grain_utils.Files.derelativize @@ Filename.concat dir basename in
    Hashtbl.add resolutions unit_name resolution;
    resolution

module Persistent_signature = struct
  type t =
    { filename : string;
      cmi : Cmi_format.cmi_infos }

  let load = ref (fun ?loc:(loc=Location.dummy_loc) ~unit_name ->
    match locate_module_file ~loc (Grain_utils.Config.module_search_path()) unit_name with
    | filename -> Some { filename; cmi = read_cmi filename }
    | exception Not_found -> None)
end

let acknowledge_pers_struct check modname
      { Persistent_signature.filename; cmi } =
  let name = cmi.cmi_name in
  let sign = cmi.cmi_sign in
  let crcs = cmi.cmi_crcs in
  let flags = cmi.cmi_flags in
  let comps =
      !components_of_module' ~deprecated:None ~loc:Location.dummy_loc
        empty Subst.identity
                             (PIdent(Ident.create_persistent name))
                             (TModSignature sign)
  in
  let ps = { ps_name = name;
             ps_sig = lazy (Subst.signature Subst.identity sign);
             ps_comps = comps;
             ps_crcs = crcs;
             ps_filename = filename;
             ps_flags = flags;
           } in

  List.iter
    (function
        | Rectypes ->
            if not !Clflags.recursive_types then
              let unit_name, _ = get_unit() in
              error (Need_recursive_types(ps.ps_name, unit_name))
        | Unsafe_string ->
            if !Config.safe_string then
              let unit_name, _ = get_unit() in
              error (Depend_on_unsafe_string_unit (ps.ps_name, unit_name));
        | Opaque -> add_imported_opaque modname)
    ps.ps_flags;
  if check then check_consistency ps;
  Hashtbl.add persistent_structures modname (Some ps);
  ps

let read_pers_struct check modname filename =
  add_import modname;
  let cmi = read_cmi filename in
  acknowledge_pers_struct check modname
    { Persistent_signature.filename; cmi }

let find_pers_struct ~loc check name filepath =
  if name = "*predef*" then raise Not_found;
  match Hashtbl.find persistent_structures name with
  | Some ps -> ps
  | None -> raise Not_found
  | exception Not_found ->
    match !can_load_modules with
    | Cannot_load_modules _ -> raise Not_found
    | Can_load_modules ->
        let ps =
          let filepath = begin try
              Option.get filepath
            with Option.No_value ->
              failwith "No file path specified"
            end in
          match !Persistent_signature.load ~loc ~unit_name:filepath with
          | Some ps -> ps
          | None ->
            Hashtbl.add persistent_structures name None;
            raise Not_found
        in
        add_import name;
        acknowledge_pers_struct check name ps


(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct ~loc name filename =
  try
    ignore (find_pers_struct ~loc false name filename)
  with
  | Not_found ->
    let err = No_module_file(name, None) in
    error err
  | Cmi_format.Error err ->
    let msg = Format.asprintf "%a" Cmi_format.report_error err in
    let err = No_module_file(name, Some msg) in
    error err
  | Error err ->
    let msg =
      match err with
      | Illegal_renaming(name, ps_name, filename) ->
        Format.asprintf
          " %a@ contains the compiled interface for @ \
           %s when %s was expected"
          Location.print_filename filename ps_name name
      | Inconsistent_import _ -> assert false
      | Need_recursive_types(name, _) ->
        Format.sprintf
          "%s uses recursive types"
          name
      | Depend_on_unsafe_string_unit (name, _) ->
        Printf.sprintf "%s uses -unsafe-string"
          name
      | Unbound_label _ -> assert false
      | Unbound_module _ -> assert false
      | Missing_module _ -> assert false
      | No_module_file _ -> assert false
      | Value_not_found_in_module _ -> assert false
      | Illegal_value_name _ -> assert false
      | Cyclic_dependencies _ -> assert false
    in
    let err = No_module_file(name, Some msg) in
    error err

let read_pers_struct modname filename =
  read_pers_struct true modname filename

let find_pers_struct name filename =
  find_pers_struct true name filename

let check_pers_struct ~loc name filename =
  if not (Hashtbl.mem persistent_structures name) then begin
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_import name;
    if (Warnings.is_active (Warnings.NoCmiFile("", None))) then
      !add_delayed_check_forward
        (fun () -> check_pers_struct ~loc name filename)
  end


let rec find_module_descr path filename env =
  match path with
  | PIdent id ->
    begin try
        IdTbl.find_same id env.components
      with Not_found ->
        let _, unit_source = get_unit() in
        if Ident.persistent id && not (Option.default (Ident.name id) filename = unit_source)
        then 
          (find_pers_struct ~loc:Location.dummy_loc (Ident.name id) filename).ps_comps
        else raise Not_found
    end
  | PExternal(m, s, pos) ->
    let c = get_components (find_module_descr m filename env) in
    let (descr, _pos) = Tbl.find s c.comp_components in
    descr

let find proj1 proj2 path env =
  match path with
  | PIdent id ->
    IdTbl.find_same id (proj1 env)
  | PExternal(m, n, _pos) ->
    let c = get_components (find_module_descr m None env) in
    let (data, _pos) = Tbl.find n (proj2 c) in
    data

let find_value =
  find (fun env -> env.values) (fun sc -> sc.comp_values)

let find_type_full =
  find (fun env -> env.types) (fun sc -> sc.comp_types)

and find_modtype =
  find (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)

let find_type_full path env =
  match path with
  | PIdent _ ->
    (try (PathMap.find path env.local_constraints, ([], []))
     with Not_found -> find_type_full path env)
  | _ -> find_type_full path env

let find_type p env =
  fst (find_type_full p env)

let find_type_descrs p env =
  snd (find_type_full p env)

let find_module ~alias path filename env =
  match path with
  | PIdent id ->
    begin try
        let data = IdTbl.find_same id env.modules in
        EnvLazy.force subst_modtype_maker data
      with Not_found ->
        let _, unit_source = get_unit() in
        if Ident.persistent id && not (Option.default (Ident.name id) filename = unit_source) then
          let ps = find_pers_struct ~loc:Location.dummy_loc (Ident.name id) filename in
          md (TModSignature(Lazy.force ps.ps_sig)) filename
        else raise Not_found
    end
  | PExternal(m, n, _pos) ->
    begin
      let c = get_components (find_module_descr m filename env) in
      let (data, _pos) = Tbl.find n c.comp_modules in
      EnvLazy.force subst_modtype_maker data
    end


let rec normalize_path lax env path =
  match path with
  | PExternal(p, s, pos) ->
    PExternal(normalize_path lax env p, s, pos)
  | _ -> path
(* No module aliases, so this is all we need*)


let normalize_path oloc env path =
  try normalize_path (oloc = None) env path
  with Not_found ->
    match oloc with None -> assert false
    | Some loc ->
        raise (Error(Missing_module(loc, path, normalize_path true env path)))

let normalize_path_prefix oloc env path =
  match path with
  | PExternal(p, s, pos) -> PExternal(normalize_path oloc env p, s, pos)
  | PIdent _ -> path
  (*| PApply _ -> assert false*)

let find_module = find_module ~alias:false

(* Find the manifest type associated to a type when appropriate:
   - the type should be public *)
let find_type_expansion path env =
  let decl = find_type path env in
  match decl.type_manifest with
  | Some body -> (decl.type_params, body, Option.map snd decl.type_newtype_level)
  (* The manifest type of Private abstract data types without
     private row are still considered unknown to the type system.
     Hence, this case is caught by the following clause that also handles
     purely abstract data types without manifest type definition. *)
  | _ -> raise Not_found

(* Find the manifest type information associated to a type, i.e.
   the necessary information for the compiler's type-based optimisations.
   In particular, the manifest type associated to a private abstract type
   is revealed for the sake of compiler's type-based optimisations. *)
let find_type_expansion_opt path env =
  let decl = find_type path env in
  match decl.type_manifest with
  (* The manifest type of Private abstract data types can still get
     an approximation using their manifest type. *)
  | Some body -> (decl.type_params, body, Option.map snd decl.type_newtype_level)
  | _ -> raise Not_found

let find_modtype_expansion path env =
  match (find_modtype path env).mtd_type with
  | None -> raise Not_found
  | Some mty -> mty

let has_local_constraints env = not (PathMap.is_empty env.local_constraints)


let copy_types l env =
  let f desc = {desc with val_type = Subst.type_expr Subst.identity desc.val_type} in
  let values = List.fold_left (fun env s -> IdTbl.update s f env) env.values l in
  {env with values; summary = Env_copy_types (env.summary, l)}

(* Currently a no-op *)
let mark_value_used env name loc = ()(*Printf.eprintf "Marking value %s used\n" name*)
let mark_type_used env name loc = ()
let mark_module_used env name loc = ()

let rec lookup_module_descr_aux ~mark id env =
  let open Identifier in
  match id with
  | IdentName s ->
    let id = Ident.create_persistent s in
    PIdent id, IdTbl.find_same id env.components
  | IdentExternal(m, n) ->
    let (p, descr) = lookup_module_descr ~mark m env in
    (* let (_, pos) = Tbl.find n (get_components descr).comp_components in *)
    (* FIXME: Should this have a proper position? *)
    (PExternal(p, n, Path.nopos), descr)

and lookup_module_descr ~mark id env =
  let (p, comps) as res = lookup_module_descr_aux ~mark id env in
  if mark then mark_module_used env (Path.last p) comps.loc;
  res

and lookup_module ?loc ~load ~mark id filename env : Path.t =
  match id with
  | Identifier.IdentName s ->
    begin try
        let p, data = IdTbl.find_name ~mark s env.modules in
        let {md_loc; md_type} = EnvLazy.force subst_modtype_maker data in
        if mark then mark_module_used env s md_loc;
        begin match md_type with
          | TModIdent (Path.PIdent id) when Ident.name id = "#recmod#" ->
            (* see #5965 *)
            failwith "NYI: lookup_module: raise Recmodule"
          | _ -> ()
        end;
        p
      with Not_found ->
        let _, unit_source = get_unit() in
        if Option.default s filename = unit_source then raise Not_found;
        let p = PIdent(Ident.create_persistent s) in
        let loc = Option.default Location.dummy_loc loc in
        if (*!Grain_utils.Config.transparent_modules &&*) not load
        then
          check_pers_struct ~loc s filename
        else begin
          ignore(find_pers_struct ~loc s filename)
        end;
        p
    end
  | Identifier.IdentExternal(l, s) ->
    let (p, descr) = lookup_module_descr ~mark l env in
    let c = get_components descr in
    let (_data, pos) = Tbl.find s c.comp_modules in
    let (comps, _) = Tbl.find s c.comp_components in
    if mark then mark_module_used env s comps.loc;
    let p = PExternal(p, s, pos) in
    p

let lookup_idtbl ~mark proj1 proj2 id env =
  let open Identifier in
  match id with
  | IdentName s -> IdTbl.find_name ~mark s (proj1 env)
  | IdentExternal(m, n) ->
    let (p, desc) = lookup_module_descr ~mark id env in
    let (data, pos) = Tbl.find n (proj2 (get_components desc)) in
    let new_path = begin match p with
      | PExternal(path, name, _) -> PExternal(path, name, pos)
      | _ -> assert false
    end in
    (new_path, data)

let lookup_tycomptbl ~mark proj1 proj2 id env =
  let open Identifier in
  match id with
  | IdentName s -> TycompTbl.find_all s (proj1 env)
  | IdentExternal(m, n) ->
    let (p, desc) = lookup_module_descr ~mark id env in
    let comps = try Tbl.find n (proj2 (get_components desc)) with Not_found -> [] in
    List.map (fun data -> (data, (fun () -> ()))) comps

let lookup_value ~mark id e =
  lookup_idtbl ~mark (fun x -> x.values) (fun x -> x.comp_values) id e

let lookup_type ~mark id e =
  lookup_idtbl ~mark (fun x -> x.types) (fun x -> x.comp_types) id e

let lookup_modtype ~mark id e =
  lookup_idtbl ~mark (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes) id e

let lookup_all_constructors ~mark id env =
  lookup_tycomptbl ~mark (fun x -> x.constructors) (fun x -> x.comp_constrs) id env

let lookup_constructor ?(mark = true) id env =
  match lookup_all_constructors ~mark id env with
  | [] -> raise Not_found
  | (desc, use)::_ ->
    if mark then begin
      use()
    end;
    desc

let lookup_all_labels ~mark lid env =
  lookup_tycomptbl ~mark (fun x -> x.labels) (fun x -> x.comp_labels) lid env

let lookup_label ~mark lid env =
  match lookup_all_labels ~mark lid env with
  | [] -> assert false
  | (desc, use) :: _ -> use (); desc


let mark_type_path env path =
  try
    let decl = find_type path env in
    mark_type_used env (Path.last path) decl
  with Not_found -> ()

let ty_path t =
  match Btype.repr t with
  | {desc=TTyConstr(path, _, _)} -> path
  | _ -> assert false


let lookup_value ?(mark = true) lid env =
  let (_, desc) as r = lookup_value ~mark lid env in
  if mark then mark_value_used env (Identifier.last lid) desc;
  r

let lookup_type ?(mark = true) lid env =
  let (path, (decl, _)) = lookup_type ~mark lid env in
  if mark then mark_type_used env (Identifier.last lid) decl;
  path

let lookup_all_constructors ?(mark = true) lid env =
  try
    let cstrs = lookup_all_constructors ~mark lid env in
    let wrap_use desc use () =
      if mark then begin
        mark_type_path env (ty_path desc.cstr_res);
        use ()
      end
    in
    List.map (fun (cstr, use) -> (cstr, wrap_use cstr use)) cstrs
  with
    Not_found when (match lid with Identifier.IdentName _ -> true | _ -> false) -> []

let lookup_label ?(mark=true) lid env =
  lookup_label ~mark lid env

let lookup_all_labels ?(mark=true) lid env =
  try
    let labels = lookup_all_labels ~mark lid env in
    let wrap_use desc use () =
      if mark then begin
        mark_type_path env (ty_path desc.lbl_res);
        use ()
      end
    in
    List.map (fun (label, use) -> (label, wrap_use label use)) labels
  with
    Not_found when (match lid with Identifier.IdentName _ -> true | _ -> false) -> []

let lookup_module ~load ?loc ?(mark = true) lid filename env =
  lookup_module ~load ?loc ~mark lid filename env

let lookup_modtype ?loc ?(mark = true) lid env =
  lookup_modtype ~mark lid env

(* Iter on an environment (ignoring the body of functors and
   not yet evaluated structures) *)

type iter_cont = unit -> unit
let iter_env_cont = ref []

let rec scrape_alias_for_visit env mty =
  match mty with
  | _ -> true

let iter_env proj1 proj2 f env () =
  IdTbl.iter (fun id x -> f (PIdent id) x) (proj1 env);
  let rec iter_components path path' mcomps =
    let cont () =
      let visit =
        match EnvLazy.get_arg mcomps.comps with
        | None -> true
        | Some (env, _sub, _path, mty) -> scrape_alias_for_visit env mty
      in
      if not visit then () else
      let comps = get_components mcomps in
      Tbl.iter
        (fun s (d, n) -> f (PExternal (path, s, n)) (PExternal (path', s, n), d))
        (proj2 comps);
      Tbl.iter
        (fun s (c, n) ->
           iter_components (PExternal (path, s, n)) (PExternal (path', s, n)) c)
        comps.comp_components
    in iter_env_cont := (path, cont) :: !iter_env_cont
  in
  Hashtbl.iter
    (fun s pso ->
      match pso with None -> ()
      | Some ps ->
          let id = PIdent (Ident.create_persistent s) in
          iter_components id id ps.ps_comps)
    persistent_structures;
  IdTbl.iter
    (fun id (path, comps) -> iter_components (PIdent id) path comps)
    env.components

let run_iter_cont l =
  iter_env_cont := [];
  List.iter (fun c -> c ()) l;
  let cont = List.rev !iter_env_cont in
  iter_env_cont := [];
  cont

let iter_types f = iter_env (fun env -> env.types) (fun sc -> sc.comp_types) f

let same_types env1 env2 =
  env1.types == env2.types && env1.components == env2.components

let used_persistent () =
  let r = ref Concr.empty in
  Hashtbl.iter (fun s pso -> if pso != None then r := Concr.add s !r)
    persistent_structures;
  !r

let find_all_comps proj s (p,mcomps) =
  let comps = get_components mcomps in
  try let (c,n) = Tbl.find s (proj comps) in [PExternal(p,s,n), c]
  with Not_found -> []

let rec find_shadowed_comps path env =
  match path with
  | PIdent id ->
      IdTbl.find_all (Ident.name id) env.components
  | PExternal(p, s, _) ->
      let l = find_shadowed_comps p env in
      let l' =
        List.map (find_all_comps (fun comps -> comps.comp_components) s) l in
      List.flatten l'

let find_shadowed proj1 proj2 path env =
  match path with
  | PIdent id ->
      IdTbl.find_all (Ident.name id) (proj1 env)
  | PExternal(p, s, _) ->
      let l = find_shadowed_comps p env in
      let l' = List.map (find_all_comps proj2 s) l in
      List.flatten l'

let find_shadowed_types path env =
  List.map fst
    (find_shadowed
       (fun env -> env.types) (fun comps -> comps.comp_types) path env)


let rec scrape_alias env ?path mty =
  match mty, path with
  | TModIdent p, _
  | TModAlias p, _ ->
      begin try
        scrape_alias env (find_modtype_expansion p env) ?path
      with Not_found ->
        mty
      end
  | mty, Some path ->
      !strengthen ~aliasable:true env mty path
  | _ -> mty

let scrape_alias env mty = scrape_alias env mty

let rec prefix_idents root pos sub = function
  | [] -> ([], sub)
  | TSigValue(id, decl) :: rem ->
      let p = PExternal(root, Ident.name id, pos) in
      let nextpos = match decl.val_kind with TValPrim _ -> pos | _ -> pos+1 in
      let (pl, final_sub) = prefix_idents root nextpos sub rem in
      (p::pl, final_sub)
  | TSigType(id, _, _) :: rem ->
      let p = PExternal(root, Ident.name id, nopos) in
      let (pl, final_sub) =
        prefix_idents root pos (Subst.add_type id p sub) rem in
      (p::pl, final_sub)
  | TSigModule(id, _, _) :: rem ->
      let p = PExternal(root, Ident.name id, pos) in
      let (pl, final_sub) =
        prefix_idents root (pos+1) (Subst.add_module id p sub) rem in
      (p::pl, final_sub)
  | TSigModType(id, _) :: rem ->
    let p = PExternal(root, Ident.name id, nopos) in
    let (pl, final_sub) =
      prefix_idents root pos
        (Subst.add_modtype id (TModIdent p) sub) rem in
    (p::pl, final_sub)



let prefix_idents root sub sg =
  if sub = Subst.identity then
    let sgs =
      try
        Hashtbl.find prefixed_sg root
      with Not_found ->
        let sgs = ref [] in
        Hashtbl.add prefixed_sg root sgs;
        sgs
    in
    try
      List.assq sg !sgs
    with Not_found ->
      let r = prefix_idents root 0 sub sg in
      sgs := (sg, r) :: !sgs;
      r
  else
    prefix_idents root 0 sub sg

let check_value_name name loc =
  (* Note: we could also check here general validity of the
     identifier, to protect against bad identifiers forged other
     steps of the pipeline. *)
  if String.length name > 0 && (name.[0] = '#') then
    for i = 1 to String.length name - 1 do
      if name.[i] = '#' then
        raise (Error(Illegal_value_name(loc, name)))
    done

(* Compute structure descriptions *)

let add_to_tbl id decl tbl =
  let decls =
    try Tbl.find id tbl with Not_found -> [] in
  Tbl.add id (decl :: decls) tbl

let rec components_of_module ~(deprecated : string option) ~loc env sub path mty = {
  loc;
  comps = EnvLazy.create (env, sub, path, mty)
}

and components_of_module_maker (env, sub, path, mty) =
  match scrape_alias env mty with
  | TModSignature sg ->
    let c =
      { comp_values = Tbl.empty;
        comp_constrs = Tbl.empty;
        comp_labels = Tbl.empty;
        comp_types = Tbl.empty;
        comp_modules = Tbl.empty;
        comp_components = Tbl.empty;
        comp_modtypes = Tbl.empty;
      } in
    let pl, sub = begin match mty, path with 
      | TModAlias path, _
      | TModIdent path, _
      | TModSignature _, path -> 
        prefix_idents path sub sg end in
    let env = ref env in
    let pos = ref 0 in
    List.iter2 (fun item path ->
        match item with
        | TSigValue(id, decl) ->
          let decl' = Subst.value_description sub decl in
          let decl' = {decl' with val_fullpath = path} in
          c.comp_values <-
            Tbl.add (Ident.name id) (decl', !pos) c.comp_values;
          begin match decl.val_kind with
              TValPrim _ -> () | _ -> incr pos
          end
        | TSigType(id, decl, _) ->
          let decl' = Subst.type_declaration sub decl in
          let constructors = Datarepr.constructors_of_type path decl' in
          let cstrs =
            List.map snd (Datarepr.constructors_of_type path decl') in
          let labels =
            List.map snd (Datarepr.labels_of_type path decl') in
          List.iter (fun (id, desc) ->
              let val_type = match desc.cstr_args with
                | [] -> desc.cstr_res
                | args -> (Btype.newgenty (TTyArrow(args, desc.cstr_res, TComOk))) in
              let val_type = match desc.cstr_existentials with
                | [] -> val_type
                | existentials -> (Btype.newgenty (TTyPoly(val_type, existentials))) in
              let get_path name =
                begin match path with
                  | PIdent _ -> PIdent (Ident.create name)
                  | PExternal(PIdent mod_, _, level) -> PExternal(PIdent mod_, name, level)
                  | PExternal(PExternal _, _, _) -> failwith "NYI: Multiple PExternal"
                end in
              let val_desc = {
                val_type;
                val_fullpath = get_path desc.cstr_name;
                val_kind = TValConstructor desc;
                val_loc = desc.cstr_loc;
              } in
              c.comp_values <- Tbl.add (Ident.name id) (val_desc, nopos) c.comp_values
            ) constructors;
          c.comp_types <-
            Tbl.add (Ident.name id)
              ((decl', (cstrs, labels)), nopos)
              c.comp_types;
          List.iter
            (fun descr ->
               c.comp_constrs <-
                 add_to_tbl descr.cstr_name descr c.comp_constrs)
            cstrs;
          List.iter
            (fun descr ->
               c.comp_labels <-
                 add_to_tbl descr.lbl_name descr c.comp_labels)
            labels;
          env := store_type_infos id decl !env
        | TSigModule(id, md, _) ->
          let md' = EnvLazy.create (sub, md) in
          c.comp_modules <-
            Tbl.add (Ident.name id) (md', !pos) c.comp_modules;
          let comps =
            components_of_module ~deprecated:None ~loc:md.md_loc !env sub path
              md.md_type
          in
          c.comp_components <-
            Tbl.add (Ident.name id) (comps, !pos) c.comp_components;
          env := store_module ~check:false id md !env;
          incr pos
        | TSigModType(id, decl) ->
          let decl' = Subst.modtype_declaration sub decl in
          c.comp_modtypes <-
            Tbl.add (Ident.name id) (decl', nopos) c.comp_modtypes;
          env := store_modtype id decl !env)
      sg pl;
    Some c
  | TModIdent _ -> None
  | TModAlias _ -> None

and store_type ~check id info env =
  (*let loc = info.type_loc in*)
  (*if check then
    check_usage loc id (fun s -> Warnings.Unused_type_declaration s)
      type_declarations;*)
  let path = PIdent id in
  let constructors = Datarepr.constructors_of_type path info in
  let labels = Datarepr.labels_of_type path info in
  let descrs = (List.map snd constructors, List.map snd labels) in

  let val_descrs = List.map (fun (id, desc) ->
      let val_type = match desc.cstr_args with
        | [] -> desc.cstr_res
        | args -> (Btype.newgenty (TTyArrow(args, desc.cstr_res, TComOk))) in
      let val_type = match desc.cstr_existentials with
        | [] -> val_type
        | existentials -> (Btype.newgenty (TTyPoly(val_type, existentials))) in
      let val_desc = {
        val_type;
        val_fullpath = PIdent (Ident.create desc.cstr_name);
        val_kind = TValConstructor desc;
        val_loc = desc.cstr_loc;
      } in
      id, val_desc
    ) constructors in

  (*if check && not loc.Location.loc_ghost &&
    Warnings.is_active (Warnings.Unused_constructor ("", false, false))
  then begin
    let ty = Ident.name id in
    List.iter
      begin fun (_, {cstr_name = c; _}) ->
        let k = (ty, loc, c) in
        if not (Hashtbl.mem used_constructors k) then
          let used = constructor_usages () in
          Hashtbl.add used_constructors k (add_constructor_usage used)
          if not (ty = "" || ty.[0] = '_')
          then !add_delayed_check_forward
              (fun () ->
                if not (is_in_signature env) && not used.cu_positive then
                  Location.prerr_warning loc
                    (Warnings.Unused_constructor
                       (c, used.cu_pattern, used.cu_privatize)))
      end
      constructors
    end;*)
  { env with
    constructors =
      List.fold_right
        (fun (id, descr) constrs -> TycompTbl.add id descr constrs)
        constructors
        env.constructors;
    labels =
      List.fold_right
        (fun (id, descr) labels -> TycompTbl.add id descr labels)
        labels
        env.labels;
    types =
      IdTbl.add id (info, descrs) env.types;
    values =
      List.fold_left (fun acc (id, val_desc) -> IdTbl.add id val_desc acc) env.values val_descrs;
    summary = List.fold_left (fun acc (id, val_desc) -> Env_value(acc, id, val_desc)) (Env_type(env.summary, id, info)) val_descrs;
  }
  

and store_type_infos id info env =
  (* Simplified version of store_type that doesn't compute and store
     constructor and label infos, but simply record the arity and
     manifest-ness of the type.  Used in components_of_module to
     keep track of type abbreviations (e.g. type t = float) in the
     computation of label representations. *)
  { env with
    types = IdTbl.add id (info, ([], [])) env.types;
    summary = Env_type(env.summary, id, info); }

and store_module ~check id md env =
  (*let loc = md.md_loc in
  if check then
    check_usage loc id (fun s -> Warnings.Unused_module s)
      module_declarations;***)

  { env with
    modules = IdTbl.add id (EnvLazy.create (Subst.identity, md)) env.modules;
    components =
      IdTbl.add id
        (components_of_module ~deprecated:None ~loc:md.md_loc
           env Subst.identity (PIdent id) md.md_type)
        env.components;
    summary = Env_module(env.summary, id, md); }

and store_modtype id info env =
  { env with
    modtypes = IdTbl.add id info env.modtypes;
    summary = Env_modtype(env.summary, id, info); }


let store_value id decl env =
  { env with
    values = IdTbl.add id decl env.values;
    summary = Env_value(env.summary, id, decl) }

let _ =
  components_of_module' := components_of_module;
  components_of_module_maker' := components_of_module_maker

let add_value ?check id desc env =
  store_value id desc env

let add_type ~check id info env =
  store_type ~check id info env

let add_module_declaration ?(arg=false) ~check id md env =
  let env = store_module ~check id md env in
  env

and add_modtype id info env =
  store_modtype id info env

let add_module ?arg id mty mf env =
  add_module_declaration ~check:false ?arg id (md mty mf) env

let add_constructor id desc ({constructors; _} as e) =
  {e with constructors = TycompTbl.add id desc constructors}

let add_local_type path info env =
  { env with
    local_constraints = PathMap.add path info env.local_constraints }

let add_local_constraint path info elv env =
  match info with
    {type_manifest = Some _; type_newtype_level = Some (lv, _)} ->
      (* elv is the expansion level, lv is the definition level *)
      let info = {info with type_newtype_level = Some (lv, elv)} in
      add_local_type path info env
  | _ -> assert false


(* Insertion of bindings by name *)

let enter store_fun name data env =
  let id = Ident.create name in (id, store_fun id data env)

let enter_value =
  let store_fun id data env =
    store_value id {data with val_fullpath = Path.PIdent id} env
  in
  enter store_fun
and enter_type = enter (store_type ~check:true)
and enter_module_declaration ?arg id md env =
  add_module_declaration ?arg ~check:true id md env
  (* let (id, env) = enter store_module name md env in
  (id, add_functor_arg ?arg id env) *)
and enter_modtype = enter store_modtype

let enter_module ?arg s mty env =
  let id = Ident.create s in
  (id, enter_module_declaration ?arg id (md mty None) env)

(* Insertion of all components of a signature *)

let add_item comp env =
  match comp with
  | TSigValue(id, decl)     -> add_value id decl env
  | TSigType(id, decl, _)   -> add_type ~check:false id decl env
  | TSigModule(id, md, _)   -> add_module_declaration ~check:false id md env
  | TSigModType(id, decl)   -> add_modtype id decl env

let rec add_signature sg env =
  match sg with
    [] -> env
  | comp :: rem -> add_signature rem (add_item comp env)

(* Open a signature path *)

let add_components ?filter_modules ?filter_components slot root env0 comps =
  let add_l w comps env0 =
    TycompTbl.add_open slot w comps env0
  in

  let add w comps env0 = IdTbl.add_open slot w root comps env0 in

  let skipped_modules = ref StringSet.empty in
  let filter tbl env0_tbl =
    match filter_modules with
    | None -> tbl
    | Some f ->
      Tbl.fold (fun m x acc ->
          if f m then
            Tbl.add m x acc
          else begin
            assert
              (match IdTbl.find_name m env0_tbl ~mark:false with
               | (_ : _ * _) -> false
               | exception _ -> true);
            skipped_modules := StringSet.add m !skipped_modules;
            acc
          end)
        tbl Tbl.empty
  in

  let filter_and_add w comps env0 =
    let comps = filter comps env0 in
    add w comps env0
  in

  let filtered_components =
    match filter_components with
    | Some f ->
      let filter tbl =
        let new_tbl = ref Tbl.empty in
        Tbl.iter (fun name value ->
          match f name with
          | Some new_name -> new_tbl := Tbl.add new_name value !new_tbl
          | None -> ()
        ) tbl;
        !new_tbl
      in
      {
        comps with
        comp_constrs=filter comps.comp_constrs;
        comp_values=filter comps.comp_values;
        comp_types=filter comps.comp_types;
        comp_modtypes=filter comps.comp_modtypes;
        comp_components=filter comps.comp_components;
      }
    | None -> comps
  in

  let constructors =
    add_l (fun x -> `Constructor x) filtered_components.comp_constrs env0.constructors
  in

  let values =
    add (fun x -> `Value x) filtered_components.comp_values env0.values
  in
  let types =
    add (fun x -> `Type x) filtered_components.comp_types env0.types
  in
  let modtypes =
    add (fun x -> `Module_type x) filtered_components.comp_modtypes env0.modtypes
  in
  let components =
    filter_and_add (fun x -> `Component x) filtered_components.comp_components env0.components
  in

  let modules =
    filter_and_add (fun x -> `Module x) filtered_components.comp_modules env0.modules
  in

  { env0 with
    summary = Env_open(env0.summary, root);
    constructors;
    values;
    types;
    modtypes;
    components;
    modules;
  }

let same_filepath unit1 unit2 =
  (resolve_unit unit1) = (resolve_unit unit2)

let check_opened (mod_ : Parsetree.import_declaration) env =
  let rec find_open summary =
    match summary with
      | Env_empty -> None
      | Env_module(summary, ({name} as id), {md_filepath=Some(filepath)})
        when same_filepath filepath mod_.pimp_path.txt -> 
          Some(PIdent(id))
      | Env_module(summary, _, _)
      | Env_value(summary, _, _)
      | Env_type(summary, _, _)
      | Env_modtype(summary, _, _)
      | Env_constraints(summary, _)
      | Env_copy_types(summary, _)
      | Env_open(summary, _) -> find_open summary
  in
  find_open env.summary

let add_module_signature ?(internal=false) mod_name (mod_ : Parsetree.import_declaration) env0 =
  let name = match mod_name with
    | Identifier.IdentName name -> name
    | Identifier.IdentExternal _ -> failwith "NYI mod identifer is external"
  in
  let mod_alias = match (Option.default (Location.mknoloc mod_name) mod_.pimp_mod_alias).txt with
    | Identifier.IdentName name -> name
    | Identifier.IdentExternal _ -> failwith "NYI mod alias identifer is external" in
  let mod_ident = if internal then (Ident.create name) else (Ident.create_persistent mod_alias) in
  let filename = (Some mod_.pimp_path.txt) in
  match check_opened mod_ env0 with
  | Some(path) when not internal ->
      let mod_type = (TModAlias path) in
      add_modtype mod_ident {mtd_type=(Some mod_type); mtd_loc=mod_.pimp_loc} env0 |>
      add_module mod_ident mod_type filename
  | Some _ -> env0
  | None ->
    let {ps_sig} = find_pers_struct name filename mod_.pimp_loc in
    let sign = Lazy.force ps_sig in
    let sign = Translsig.translate_signature sign in
    let mod_type = (TModSignature sign) in
    add_modtype mod_ident {mtd_type=(Some mod_type); mtd_loc=mod_.pimp_loc} env0 |>
    add_module mod_ident mod_type filename

let open_signature ?filter_modules ?filter_components slot root filepath env0 =
  let comps = get_components (find_module_descr root filepath env0) in
  Some (add_components ?filter_modules ?filter_components slot root env0 comps)

let open_pers_signature name filepath env =
  match open_signature None (PIdent(Ident.create_persistent name)) filepath env with
  | Some env -> env
  | None -> assert false (* Invalid compilation unit *)

let open_signature_of_initially_opened_module ?loc:(loc=Location.dummy_loc) root env =
  let load_path = !Grain_utils.Config.include_dirs in
  let filter_modules m =
    match locate_module_file ~loc load_path m with
    | (_ : string) -> false
    | exception Not_found -> true
  in
  open_signature None root env ~filter_modules

let check_imports found all where =
  List.iter (fun comp ->
    if List.mem comp found
    then ()
    else 
      let {loc; txt} = comp in
      error (Value_not_found_in_module (loc, Identifier.string_of_ident txt, where))
  ) all

let open_signature
    ?(used_slot = ref false)
    ?(toplevel = false)
    root mod_name (mod_ : Parsetree.import_declaration) env =
  let env = add_module_signature ~internal:true mod_name mod_ env in
  match mod_.pimp_val with
  | PImportModule -> Some(add_module_signature mod_name mod_ env)
  | PImportValues values ->
    let imported = ref [] in
    let filter_components name =
      let value = List.find_opt (fun (val_name, _) ->
        match (val_name : Identifier.t Parsetree.loc).txt with
        | Identifier.IdentName id_name -> id_name = name
        | Identifier.IdentExternal _ -> failwith "NYI"
      ) values in
      begin match value with
        | Some (val_name, val_alias) ->
          let new_name = Option.default val_name val_alias in
          begin match new_name.txt with
            | Identifier.IdentName id_name -> 
              imported := val_name :: !imported;
              Some(id_name)
            | Identifier.IdentExternal _ -> failwith "NYI" 
          end
        | None -> None
      end
    in
    let root = match check_opened mod_ env with
      | Some path -> path
      | None -> assert false
    in
    let new_env = open_signature ~filter_components None root (Some mod_.pimp_path.txt) env in
    check_imports !imported (List.map (fun (value, _) -> value) values) mod_.pimp_path.txt;
    new_env
  | PImportAllExcept exceptions ->
    let rejected = ref [] in
    let filter_components name =
      if List.exists (fun id ->
        match id.txt with
        | Identifier.IdentName id_name -> 
          if id_name = name
          then begin 
            rejected := id :: !rejected; 
            true 
          end
          else false
        | Identifier.IdentExternal _ -> failwith "NYI"
      ) exceptions
      then None
      else Some(name) in
    let root = match check_opened mod_ env with
      | Some path -> path
      | None -> assert false
    in
    let new_env = open_signature ~filter_components None root (Some mod_.pimp_path.txt) env in
    check_imports !rejected exceptions mod_.pimp_path.txt;
    new_env

(* Read a signature from a file *)
let read_signature modname filename =
  let ps = read_pers_struct modname filename in
  Lazy.force ps.ps_sig

(* Return the CRC of the given compilation unit *)
let crc_of_unit name filename =
  let ps = find_pers_struct ~loc:Location.dummy_loc name filename in
  let crco =
    try
      List.assoc name ps.ps_crcs
    with Not_found ->
      assert false
  in
  match crco with
  | None -> assert false
  | Some crc -> crc

(* Return the list of imported interfaces with their CRCs *)

let imports() =
  Consistbl.extract (StringSet.elements !imported_units) crc_units

(* Returns true if [s] is an imported opaque module *)
let is_imported_opaque s =
  StringSet.mem s !imported_opaque_units


(* Save a signature to a file *)
(*
let save_signature_with_imports ~deprecated sg modname filename imports =
  (*prerr_endline filename;
  List.iter (fun (name, crc) -> prerr_endline name) imports;*)
  Btype.cleanup_abbrev ();
  Subst.reset_for_saving ();
  let sg = Subst.signature (Subst.for_saving Subst.identity) sg in
  let flags =
    List.concat [
      if !Grain_utils.Config.recursive_types then [Cmi_format.Rectypes] else [];
      (*if !Grain_utils.Config.opaque then [Cmi_format.Opaque] else [];*)
    ]
  in
  try
    let cmi = {
      cmi_name = modname;
      cmi_sign = sg;
      cmi_crcs = imports;
      cmi_flags = flags;
    } in
    let crc =
      output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
         ~mode: [Open_binary] filename
         (fun temp_filename oc -> output_cmi temp_filename oc cmi) in
    (* Enter signature in persistent table so that imported_unit()
       will also return its crc *)
    let comps =
      components_of_module ~deprecated ~loc:Location.dummy_loc
        empty Subst.identity
        (PIdent(Ident.create_persistent modname)) (TModSignature sg) in
    let ps =
      { ps_name = modname;
        ps_sig = lazy (Subst.signature Subst.identity sg);
        ps_comps = comps;
        ps_crcs = (cmi.cmi_name, Some crc) :: imports;
        ps_filename = filename;
        ps_flags = cmi.cmi_flags;
      } in
    save_pers_struct crc ps;
    cmi
  with exn ->
    remove_file filename;
    raise exn

let save_signature ~deprecated sg modname filename =
  save_signature_with_imports ~deprecated sg modname filename (imports())
*)

(* Build a module signature *)
let build_signature_with_imports ?deprecated sg modname filename imports =
  (*prerr_endline filename;
  List.iter (fun (name, crc) -> prerr_endline name) imports;*)
  Btype.cleanup_abbrev ();
  Subst.reset_for_saving ();
  let sg = Subst.signature (Subst.for_saving Subst.identity) sg in
  let flags =
    List.concat [
      if !Grain_utils.Config.recursive_types then [Cmi_format.Rectypes] else [];
      (*if !Grain_utils.Config.opaque then [Cmi_format.Opaque] else [];*)
    ]
  in
  try
    let cmi = {
      cmi_name = modname;
      cmi_sign = sg;
      cmi_crcs = imports;
      cmi_flags = flags;
    } in
    let full_cmi = Cmi_format.build_full_cmi ~name:modname ~sign:sg ~crcs:imports ~flags in
    let crc = match full_cmi.cmi_crcs with
      | (_, Some crc)::_ -> crc
      | _ -> failwith "Impossible"
    in
    (* Enter signature in persistent table so that imported_unit()
       will also return its crc *)
    let comps =
      components_of_module ~deprecated ~loc:Location.dummy_loc
        empty Subst.identity
        (PIdent(Ident.create_persistent modname)) (TModSignature sg) in
    let ps =
      { ps_name = modname;
        ps_sig = lazy (Subst.signature Subst.identity sg);
        ps_comps = comps;
        ps_crcs = full_cmi.cmi_crcs;
        ps_filename = filename;
        ps_flags = cmi.cmi_flags;
      } in
    save_pers_struct crc ps;
    cmi
  with exn ->
    raise exn

let build_signature ?deprecated sg modname filename =
  build_signature_with_imports ?deprecated sg modname filename (imports())


(* Folding on environments *)

let find_all proj1 proj2 f lid env acc =
  match lid with
    | None ->
      IdTbl.fold_name
        (fun name (p, data) acc -> f name p data acc)
        (proj1 env) acc
    | Some l ->
      let p, desc = lookup_module_descr ~mark:true l env in
      let c = get_components desc in
      Tbl.fold
        (fun s (data, pos) acc -> f s (PExternal(p, s, pos)) data acc)
        (proj2 c) acc


let find_all_simple_list proj1 proj2 f lid env acc =
  match lid with
    | None ->
      TycompTbl.fold_name
        (fun data acc -> f data acc)
        (proj1 env) acc
    | Some l ->
      let (_p, desc) = lookup_module_descr ~mark:true l env in
      let c = get_components desc in
      Tbl.fold
        (fun _s comps acc ->
           match comps with
             [] -> acc
           | data :: _ ->
             f data acc)
        (proj2 c) acc

let fold_modules f lid env acc =
  match lid with
    | None ->
      let acc =
        IdTbl.fold_name
          (fun name (p, data) acc ->
             let data = EnvLazy.force subst_modtype_maker data in
             f name p data acc
          )
          env.modules
          acc
      in
      (* Hashtbl.fold
        (fun name ps acc ->
          match ps with
              None -> acc
            | Some ps ->
              f name (PIdent(Ident.create_persistent name))
                     (md (TModSignature(Lazy.force ps.ps_sig)) None) acc)
        persistent_structures *)
        acc
    | Some l ->
      let p, desc = lookup_module_descr ~mark:true l env in
      let c = get_components desc in
      Tbl.fold
        (fun s (data, pos) acc ->
           f s (PExternal(p, s, pos))
             (EnvLazy.force subst_modtype_maker data) acc)
        c.comp_modules
        acc

let fold_values f =
  find_all (fun env -> env.values) (fun sc -> sc.comp_values) f
and fold_constructors f =
  find_all_simple_list (fun env -> env.constructors) (fun sc -> sc.comp_constrs) f
and fold_types f =
  find_all (fun env -> env.types) (fun sc -> sc.comp_types) f
and fold_modtypes f =
  find_all (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes) f

let (initial_safe_string, initial_unsafe_string) =
  Builtin_types.build_initial_env
    (add_type ~check:false)
    empty

(* Return the environment summary *)

let summary env =
  if PathMap.is_empty env.local_constraints then env.summary
  else Env_constraints (env.summary, env.local_constraints)

let last_env = ref empty
let last_reduced_env = ref empty

(* Error report *)

open Format

let format_dependency_chain ppf (depchain : dependency_chain) =
  let print_single {txt; loc} =
    fprintf ppf "@,@[<2> %s at " txt;
    if loc = Location.dummy_loc then
      fprintf ppf "<unknown>"
    else
      fprintf ppf "%a" Location.print_compact loc;
    fprintf ppf "@]"
  in
  fprintf ppf "@[<v>Dependency Chain:";
  List.iter print_single depchain;
  fprintf ppf "@]"

let report_error ppf = function
  | Illegal_renaming(modname, ps_name, filename) -> fprintf ppf
      "Wrong file naming: %a@ contains the compiled interface for @ \
       %s when %s was expected"
      Location.print_filename filename ps_name modname
  | Inconsistent_import(name, source1, source2) -> fprintf ppf
      "@[<hov>The files %a@ and %a@ \
              make inconsistent assumptions@ over interface %s@]"
      Location.print_filename source1 Location.print_filename source2 name
  | Need_recursive_types(import, export) ->
      fprintf ppf
        "@[<hov>Unit %s imports from %s, which uses recursive types.@ %s@]"
        export import "The compilation flag -rectypes is required"
  | Depend_on_unsafe_string_unit(import, export) ->
      fprintf ppf
        "@[<hov>Unit %s imports from %s, compiled with -unsafe-string.@ %s@]"
        export import "This compiler has been configured in strict \
                       safe-string mode (-force-safe-string)"
  | Missing_module(_, path1, path2) ->
      fprintf ppf "@[@[<hov>";
      if Path.same path1 path2 then
        fprintf ppf "Internal path@ %s@ is dangling." (Path.name path1)
      else
        fprintf ppf "Internal path@ %s@ expands to@ %s@ which is dangling."
          (Path.name path1) (Path.name path2);
      fprintf ppf "@]@ @[%s@ %s@ %s.@]@]"
        "The compiled interface for module" (Ident.name (Path.head path2))
        "was not found"
  | Illegal_value_name(_loc, name) ->
      fprintf ppf "'%s' is not a valid value identifier."
        name
  | Unbound_label(_, label) -> fprintf ppf "Unbound record label %s. Perhaps you need to import its type or write a type definition?" label
  | Unbound_module(_, modname) -> fprintf ppf "Unbound module %s" modname
  | No_module_file(m, None) -> fprintf ppf "Missing file for module %s" m
  | No_module_file(m, Some(msg)) -> fprintf ppf "Missing file for module %s: %s" m msg
  | Value_not_found_in_module(_, name, path) -> fprintf ppf "Export '%s' was not found in '%s'" name path
  | Cyclic_dependencies(dep, chain) ->
    fprintf ppf "@[<v>@[Found cyclic dependency: %s@]@,%a@]"
      dep format_dependency_chain chain

let () =
  Location.register_error_of_exn
    (function
      | Error (Missing_module (loc, _, _)
              | Illegal_value_name (loc, _)
              | Value_not_found_in_module (loc, _, _)
               as err) when loc <> Location.dummy_loc ->
          Some (Location.error_of_printer loc report_error err)
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
