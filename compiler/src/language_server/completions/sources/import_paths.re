open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Completion_types;
open Completion_types.ResponseResult;
open Builder;

let has_gr_extension = path => Filename.check_suffix(path, ".gr");

let without_gr_extension = path =>
  has_gr_extension(path) ? Filename.chop_suffix(path, ".gr") : path;

let is_relative_import = path =>
  String.starts_with(~prefix="./", path)
  || String.starts_with(~prefix="../", path);

let display_prefix_of = prefix =>
  switch (String.rindex_opt(prefix, '/')) {
  | None => ""
  | Some(index) => String.sub(prefix, 0, index + 1)
  };

let candidate_from_entry =
    (~context, ~base_dir, ~display_prefix, ~entry, ~strip_extension) => {
  let full_path = Filepath.String.concat(base_dir, entry);
  if (Utils.safe_is_directory(full_path)) {
    Some(
      make_candidate(
        ~source=Filesystem,
        ~label=display_prefix ++ entry ++ "/",
        ~kind=Folder,
        ~sort_group=Sort_group.Import,
        ~context,
        (),
      ),
    );
  } else if (has_gr_extension(entry)) {
    let label_entry = strip_extension ? without_gr_extension(entry) : entry;
    Some(
      make_candidate(
        ~source=Filesystem,
        ~label=display_prefix ++ label_entry,
        ~kind=File,
        ~sort_group=Sort_group.Import,
        ~context,
        (),
      ),
    );
  } else {
    None;
  };
};

let candidates_from_dir =
    (~context, ~base_dir, ~display_prefix, ~strip_extension) =>
  Utils.safe_readdir(base_dir)
  |> List.fold_left(
       (acc, entry) =>
         // skip hidden entries
         if (String.length(entry) > 0 && entry.[0] == '.') {
           acc;
         } else {
           switch (
             candidate_from_entry(
               ~context,
               ~base_dir,
               ~display_prefix,
               ~entry,
               ~strip_extension,
             )
           ) {
           | None => acc
           | Some(candidate) => [candidate, ...acc]
           };
         },
       [],
     );

let local_path_candidates = (~context: Syntax.Types.t, ~base_dir) => {
  let prefix = context.prefix;
  let display_prefix =
    is_relative_import(prefix) ? display_prefix_of(prefix) : "./";
  let candidate_dir = Filepath.String.concat(base_dir, display_prefix);
  candidates_from_dir(
    ~context,
    ~base_dir=candidate_dir,
    ~display_prefix,
    ~strip_extension=false,
  );
};

let search_path_candidates = (~context: Syntax.Types.t) => {
  let prefix = context.prefix;
  if (is_relative_import(prefix)) {
    [];
  } else {
    let display_prefix = display_prefix_of(prefix);
    Config.module_search_path()
    |> List.fold_left(
         (acc, base_dir) => {
           let candidate_dir =
             Filepath.String.concat(base_dir, display_prefix);
           candidates_from_dir(
             ~context,
             ~base_dir=candidate_dir,
             ~display_prefix,
             ~strip_extension=true,
           )
           @ acc;
         },
         [],
       );
  };
};

let file_path_candidates = (~context: Syntax.Types.t, ~uri) => {
  let filename = Utils.uri_to_filename(uri);
  let base_dir = Filepath.String.dirname(filename);
  local_path_candidates(~context, ~base_dir)
  @ search_path_candidates(~context);
};

let first_module_name_from_source = filename => {
  let read_line = ic =>
    try(Some(input_line(ic))) {
    | End_of_file => None
    };
  let module_prefix = "module ";
  let rec module_name_end = (line, idx) =>
    if (idx >= String.length(line)) {
      idx;
    } else {
      let c = line.[idx];
      if (Syntax.Text.is_ident_char(c)) {
        module_name_end(line, idx + 1);
      } else {
        idx;
      };
    };
  let rec loop = ic =>
    switch (read_line(ic)) {
    | None => None
    | Some(line) =>
      let line = String.trim(line);
      if (String.starts_with(~prefix=module_prefix, line)) {
        let start = String.length(module_prefix);
        let end_ = module_name_end(line, start);
        end_ > start
          ? Some(String.sub(line, start, end_ - start)) : loop(ic);
      } else {
        loop(ic);
      };
    };
  try({
    let ic = open_in(filename);
    let result = loop(ic);
    close_in(ic);
    result;
  }) {
  | _ => None
  };
};

let source_file_for_import = (~base_dir, import_path) => {
  let possible_paths =
    if (is_relative_import(import_path) || !Filename.is_relative(import_path)) {
      [import_path];
    } else {
      List.map(
        search_dir => Filepath.String.concat(search_dir, import_path),
        Config.module_search_path(),
      );
    };
  let with_extension =
    List.fold_left(
      (acc, path) =>
        has_gr_extension(path)
          ? [path, ...acc] : [path ++ ".gr", path, ...acc],
      [],
      possible_paths,
    )
    |> List.rev;
  List.find_opt(
    path => {
      let path =
        Filename.is_relative(path)
          ? Filepath.String.concat(base_dir, path) : path;
      Sys.file_exists(path) && !Utils.safe_is_directory(path);
    },
    with_extension,
  )
  |> Option.map(path =>
       Filename.is_relative(path)
         ? Filepath.String.concat(base_dir, path) : path
     );
};

let module_name_from_env = (~current_file, import_path) => {
  let previous_unit = Env.get_unit();
  let restore = () => Env.set_unit(previous_unit);
  try(
    {
      Env.set_unit(("__completion__", current_file));
      let module_name =
        Env.load_pers_struct(~loc=Location.dummy_loc, import_path);
      restore();
      Some(module_name);
    }
  ) {
  | exn =>
    restore();
    Trace.log(
      "Completion import include load failed for `"
      ++ import_path
      ++ "`: "
      ++ Printexc.to_string(exn),
    );
    None;
  };
};

let include_module_candidates = (~context: Syntax.Types.t, ~uri, ~import_path) => {
  let filename = Utils.uri_to_filename(uri);
  let base_dir = Filepath.String.dirname(filename);
  let module_name =
    switch (module_name_from_env(~current_file=filename, import_path)) {
    | Some(module_name) => Some(module_name)
    | None =>
      switch (source_file_for_import(~base_dir, import_path)) {
      | None => None
      | Some(source_file) => first_module_name_from_source(source_file)
      }
    };
  switch (module_name) {
  | None => []
  | Some(module_name) => [
      make_candidate(
        ~source=Typedtree,
        ~label=module_name,
        ~kind=Module,
        ~sort_group=Sort_group.Import,
        ~context,
        (),
      ),
    ]
  };
};
