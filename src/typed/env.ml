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
open Path
open Types

type type_descriptions = constructor_description list
module PathMap = Map.Make(Path)

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
}

let empty = {
  values = IdTbl.empty;
  types = IdTbl.empty;
  components = IdTbl.empty;
  constructors = TycompTbl.empty;
}

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

let get_components _ = failwith "NYI: get_components"

let rec find_module_descr path env =
  match path with
  | PIdent id ->
    begin try
        IdTbl.find_same id env.components
      with Not_found ->
        raise Not_found
        (*if Ident.persistent id && not (Ident.name id = !current_unit)
        then (find_pers_struct (Ident.name id)).ps_comps
        else raise Not_found*)
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
    failwith "NYI: find external"

let find_value =
  find (fun env -> env.values) (fun sc -> sc.comp_values)

let find_type_full =
  find (fun env -> env.types) (fun sc -> sc.comp_types)

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
          failwith "NYI: find_module > PIdent > Not_found"
        else raise Not_found
    end
  | PExternal(m, n, _pos) ->
    failwith "NYI: find_module > PExternal"

(* Currently a no-op *)
let mark_module_used env name loc = ()

let rec lookup_module_descr_aux ~mark id env =
  let open Identifier in
  match id with
  | IdentName s ->
    IdTbl.find_name ~mark s env.components
  | IdentExternal(m, n) ->
    let (p, descr) = lookup_module_descr ~mark m env in
    let (descr, pos) = Tbl.find n descr.comp_components in
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
    let (data, pos) = Tbl.find n (proj2 desc) in
    (PExternal(p, n, pos), data)

let lookup_tycomptbl ~mark proj1 proj2 id env =
  let open Identifier in
  match id with
  | IdentName s -> TycompTbl.find_all s (proj1 env)
  | IdentExternal(m, n) ->
    let (p, desc) = lookup_module_descr ~mark id env in
    let comps = try Tbl.find n (proj2 desc) with Not_found -> [] in
    List.map (fun data -> (data, (fun () -> ()))) comps

let lookup_value ~mark id e =
  lookup_idtbl ~mark (fun x -> x.values) (fun x -> x.comp_values) id e

let lookup_type ~mark id e =
  lookup_idtbl ~mark (fun x -> x.types) (fun x -> x.comp_types) id e

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

let add_value id desc ({values; _} as e) =
  {e with values = IdTbl.add id desc values}

let add_type id desc ({types; _} as e) =
  {e with types = IdTbl.add id desc types}

let add_constructor id desc ({constructors; _} as e) =
  {e with constructors = TycompTbl.add id desc constructors}

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


(*let find_all proj1 proj2 f lid env acc =
  match lid with
  | None ->
    IdTbl.fold_name
      (fun name (p, data) acc -> f name p data acc)
      (proj1 env) acc
  | Some l ->
    let p, desc = lookup_module_descr ~mark:true l env in
    begin match get_components desc with
        Structure_comps c ->
        Tbl.fold
          (fun s (data, pos) acc -> f s (Pdot (p, s, pos)) data acc)
          (proj2 c) acc
      | Functor_comps _ ->
        acc
    end
*)
