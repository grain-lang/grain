open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;
open Sourcetree;

// We need to use the "grain-type" markdown syntax to have correct coloring on hover items
let grain_type_code_block = Markdown.code_block(~syntax="grain-type");
// Used for module hovers
let grain_code_block = Markdown.code_block(~syntax="grain");

let markdown_join = (a, b) => {
  // Horizonal rules between code blocks render a little funky
  // so we manually add linebreaks
  Printf.sprintf(
    "%s\n---\n<br><br>\n%s",
    a,
    b,
  );
};

let supressed_types = [Builtin_types.path_void, Builtin_types.path_bool];

let print_type = (env, ty) => {
  let instance = grain_type_code_block(Printtyp.string_of_type_scheme(ty));
  try({
    let (path, _, decl) = Ctype.extract_concrete_typedecl(env, ty);
    // Avoid showing the declaration for supressed types
    if (List.exists(
          supressed_type => Path.same(path, supressed_type),
          supressed_types,
        )) {
      raise(Not_found);
    };
    markdown_join(
      grain_code_block(
        Printtyp.string_of_type_declaration(
          ~ident=Ident.create(Path.last(path)),
          decl,
        ),
      ),
      instance,
    );
  }) {
  | Not_found => instance
  };
};

let print_type_raw = ty => Printtyp.string_of_type_scheme(ty);

let print_mod_type = (decl: Types.module_declaration) => {
  let vals = Modules.get_provides(decl);
  let signatures =
    List.map(
      (v: Modules.provide) =>
        switch (v.kind) {
        | Function
        | Value => Format.sprintf("let %s", v.signature)
        | Record
        | Enum
        | Abstract
        | Exception => v.signature
        | Module => Format.sprintf("module %s", v.name)
        },
      vals,
    );
  String.concat("\n", signatures);
};
