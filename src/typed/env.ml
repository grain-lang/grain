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
open Cmi_format
open Path
open Types

let add_delayed_check_forward = ref (fun _ -> assert false)

type type_descriptions = constructor_description list
module PathMap = Map.Make(Path)

let prefixed_sg = Hashtbl.create 113

type error =
  | Illegal_renaming of string * string * string
  | Inconsistent_import of string * string * string
  | Need_recursive_types of string * string
  | Depend_on_unsafe_string_unit of string * string
  | Missing_module of Location.t * Path.t * Path.t
  | No_module_file of string * string option
  | Illegal_value_name of Location.t * string

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

type module_component_components = {
  mutable comp_values: value_description comp_tbl;
  mutable comp_constrs: (string, constructor_description list) Tbl.t;
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

and t = {
  values: value_description IdTbl.t;
  types: (type_declaration * type_descriptions) IdTbl.t;
  constructors: constructor_description TycompTbl.t;
  components: module_components IdTbl.t;
  modules: (Subst.t * module_declaration, module_declaration) EnvLazy.t IdTbl.t;
  modtypes: modtype_declaration IdTbl.t;
  local_constraints: type_declaration PathMap.t;
}

let empty = {
  values = IdTbl.empty;
  types = IdTbl.empty;
  components = IdTbl.empty;
  modules = IdTbl.empty;
  modtypes = IdTbl.empty;
  constructors = TycompTbl.empty;
  local_constraints = PathMap.empty;
}

let copy_local ~from env =
  { env with
    local_constraints = from.local_constraints; }


type can_load_modules =
  | Can_load_modules
  | Cannot_load_modules of EnvLazy.log

let can_load_modules = ref Can_load_modules


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

let md md_type =
  {md_type; md_loc=Location.dummy_loc}

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
  comp_types = Tbl.empty;
  comp_components = Tbl.empty;
  comp_modules = Tbl.empty;
  comp_modtypes = Tbl.empty;
}

let get_components c =
  match get_components_opt c with
  | None -> empty_structure
  | Some c -> c

let current_unit = ref ""

let set_unit_name name =
  current_unit := name

let get_unit_name () =
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

(* Consistency between persistent structures *)

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

let find_in_path_uncap path name =
  let uname = String.uncapitalize_ascii name in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in try_dir path

module Persistent_signature = struct
  type t =
    { filename : string;
      cmi : Cmi_format.cmi_infos }

  let load = ref (fun ~unit_name ->
    match find_in_path_uncap !(Config.load_path) (unit_name ^ ".cmi") with
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
  if ps.ps_name <> modname then
    error (Illegal_renaming(modname, ps.ps_name, filename));

  List.iter
    (function
        | Rectypes ->
            if not !Clflags.recursive_types then
              error (Need_recursive_types(ps.ps_name, !current_unit))
        | Unsafe_string ->
            if Config.safe_string then
              error (Depend_on_unsafe_string_unit (ps.ps_name, !current_unit));
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

let find_pers_struct check name =
  if name = "*predef*" then raise Not_found;
  match Hashtbl.find persistent_structures name with
  | Some ps -> ps
  | None -> raise Not_found
  | exception Not_found ->
    match !can_load_modules with
    | Cannot_load_modules _ -> raise Not_found
    | Can_load_modules ->
        let ps =
          match !Persistent_signature.load ~unit_name:name with
          | Some ps -> ps
          | None ->
            Hashtbl.add persistent_structures name None;
            raise Not_found
        in
        add_import name;
        acknowledge_pers_struct check name ps


(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct ~loc name =
  try
    ignore (find_pers_struct false name)
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
      | Missing_module _ -> assert false
      | No_module_file _ -> assert false
      | Illegal_value_name _ -> assert false
    in
    let err = No_module_file(name, Some msg) in
    error err

let read_pers_struct modname filename =
  read_pers_struct true modname filename

let find_pers_struct name =
  find_pers_struct true name

let check_pers_struct ~loc name =
  if not (Hashtbl.mem persistent_structures name) then begin
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_import name;
    if (Warnings.is_active (Warnings.No_cmi_file("", None))) then
      !add_delayed_check_forward
        (fun () -> check_pers_struct ~loc name)
  end


let rec find_module_descr path env =
  match path with
  | PIdent id ->
    begin try
        IdTbl.find_same id env.components
      with Not_found ->
        if Ident.persistent id && not (Ident.name id = !current_unit)
        then (find_pers_struct (Ident.name id)).ps_comps
        else raise Not_found
    end
  | PExternal(m, s, pos) ->
    let c = get_components (find_module_descr m env) in
    let (descr, _pos) = Tbl.find s c.comp_components in
    descr

let find proj1 proj2 path env =
  match path with
  | PIdent id ->
    IdTbl.find_same id (proj1 env)
  | PExternal(m, n, _pos) ->
    let c = get_components (find_module_descr m env) in
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
    (try (PathMap.find path env.local_constraints, [])
     with Not_found -> find_type_full path env)
  | _ -> find_type_full path env

let find_type p env =
  fst (find_type_full p env)

let find_type_descrs p env =
  snd (find_type_full p env)

let find_module ~alias path env =
  match path with
  | PIdent id ->
    begin try
        let data = IdTbl.find_same id env.modules in
        EnvLazy.force subst_modtype_maker data
      with Not_found ->
        if Ident.persistent id && not (Ident.name id = !current_unit) then
          let ps = find_pers_struct (Ident.name id) in
          md (TModSignature(Lazy.force ps.ps_sig))
        else raise Not_found
    end
  | PExternal(m, n, _pos) ->
    begin
      let c = get_components (find_module_descr m env) in
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

(* Currently a no-op *)
let mark_module_used env name loc = ()

let rec lookup_module_descr_aux ~mark id env =
  let open Identifier in
  match id with
  | IdentName s ->
    IdTbl.find_name ~mark s env.components
  | IdentExternal(m, n) ->
    let (p, descr) = lookup_module_descr ~mark m env in
    let (descr, pos) = Tbl.find n (get_components descr).comp_components in
    (PExternal(p, n, pos), descr)

and lookup_module_descr ~mark id env =
  let (p, comps) as res = lookup_module_descr_aux ~mark id env in
  if mark then mark_module_used env (Path.last p) comps.loc;
  res

let lookup_idtbl ~mark proj1 proj2 id env =
  let open Identifier in
  match id with
  | IdentName s -> IdTbl.find_name ~mark s (proj1 env)
  | IdentExternal(m, n) ->
    let (p, desc) = lookup_module_descr ~mark id env in
    let (data, pos) = Tbl.find n (proj2 (get_components desc)) in
    (PExternal(p, n, pos), data)

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
  lookup_idtbl ~mark (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)

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

let rec scrape_alias env ?path mty =
  match mty, path with
  | TModIdent p, _ ->
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
        comp_types = Tbl.empty;
        comp_modules = Tbl.empty;
        comp_components = Tbl.empty;
        comp_modtypes = Tbl.empty;
      } in
    let pl, sub = prefix_idents path sub sg in
    let env = ref env in
    let pos = ref 0 in
    List.iter2 (fun item path ->
        match item with
        | TSigValue(id, decl) ->
          let decl' = Subst.value_description sub decl in
          c.comp_values <-
            Tbl.add (Ident.name id) (decl', !pos) c.comp_values;
          begin match decl.val_kind with
              TValPrim _ -> () | _ -> incr pos
          end
        | TSigType(id, decl, _) ->
          let decl' = Subst.type_declaration sub decl in
          let constructors =
            List.map snd (Datarepr.constructors_of_type path decl') in
          c.comp_types <-
            Tbl.add (Ident.name id)
              ((decl', constructors), nopos)
              c.comp_types;
          List.iter
            (fun descr ->
               c.comp_constrs <-
                 add_to_tbl descr.cstr_name descr c.comp_constrs)
            constructors;
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

and store_type_infos id info env =
  (* Simplified version of store_type that doesn't compute and store
     constructor and label infos, but simply record the arity and
     manifest-ness of the type.  Used in components_of_module to
     keep track of type abbreviations (e.g. type t = float) in the
     computation of label representations. *)
  { env with
    types = IdTbl.add id (info,[]) env.types; }

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
        env.components; }

and store_modtype id info env =
  { env with
    modtypes = IdTbl.add id info env.modtypes; }


let store_value id decl env =
  { env with
    values = IdTbl.add id decl env.values }

let store_type id info env =
  let path = PIdent id in
  let constructors = Datarepr.constructors_of_type path info in
  let descrs = List.map snd constructors in
  { env with
    constructors = List.fold_right (fun (id, descr) cstrs -> TycompTbl.add id descr cstrs)
        constructors
        env.constructors;
    types = IdTbl.add id (info, descrs) env.types; }

let _ =
  components_of_module' := components_of_module;
  components_of_module_maker' := components_of_module_maker

let add_value ?check id desc env =
  store_value id desc env

let add_type ~check id info env =
  store_type id info env

let add_module_declaration ?(arg=false) ~check id md env =
  let env = store_module ~check id md env in
  env

and add_modtype id info env =
  store_modtype id info env

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
      Hashtbl.fold
        (fun name ps acc ->
          match ps with
              None -> acc
            | Some ps ->
              f name (PIdent(Ident.create_persistent name))
                     (md (TModSignature(Lazy.force ps.ps_sig))) acc)
        persistent_structures
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
