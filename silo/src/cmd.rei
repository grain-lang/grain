type t;
type opt_spec;
type arg_spec;

type action =
  | Help
  | Thunk(unit => unit)
  | NoAction;

let opt:
  (
    ~names: list(string),
    ~doc: string,
    ~usage: string,
    ~value: ref('a),
    ~conv: string => 'a
  ) =>
  opt_spec;
let flag:
  (~names: list(string), ~doc: string, ~value: ref('a), ~conv: unit => 'a) =>
  opt_spec;
let enum_opt:
  (
    ~names: list(string),
    ~doc: string,
    ~usage: string,
    ~value: ref('a),
    ~enum: list(string),
    ~conv: string => 'a
  ) =>
  opt_spec;

let arg:
  (~name: string, ~doc: string, ~value: ref('a), ~conv: string => 'a) =>
  arg_spec;

let make:
  (
    ~name: string,
    ~doc: string,
    ~usage: string,
    ~options: list(opt_spec)=?,
    ~args: list(arg_spec)=?,
    ~commands: list(t)=?,
    action
  ) =>
  t;

let parse: t => unit;
