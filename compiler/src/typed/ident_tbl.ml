include Hashtbl.Make(
  struct
    type t = Ident.t
    let equal i j = Ident.same i j
    let hash i = Hashtbl.hash i
  end
)
