type t;
type group_type =
  | Auto
  | FitGroups
  | FitAll;

let empty: t;
let group_breaker: t;
let string: string => t;
let blank: int => t;
let space: t;
let breakable_space: t;
let break: t;
let hardline: t;
let phantom_hardline: t;
let if_broken: (t, t) => t;
let indent: (~count: int=?, t) => t;
let group: (~print_width: int=?, ~kind: group_type=?, t) => t;
let concat: (t, t) => t;
let (++): (t, t) => t;

let concat_map:
  (
    ~sep: ('a, 'a) => t,
    ~lead: 'a => t,
    ~trail: 'a => t,
    ~f: (~final: bool, 'a) => t,
    list('a)
  ) =>
  t;

let comma: t;
let comma_breakable_space: t;
let trailing_comma: t;
let parens: (~lead: t=?, ~trail: t=?, t) => t;
let braces: t => t;
let block_braces: (~lead: t, ~trail: t, t) => t;
let array_brackets: t => t;
let list_brackets: t => t;
let angle_brackets: t => t;
let double_quotes: t => t;

module Engine: {
  let print:
    (
      ~write: string => 'a,
      ~eol: Grain_utils.Fs_access.eol,
      ~line_width: int,
      t
    ) =>
    unit;
  let to_string:
    (~eol: Grain_utils.Fs_access.eol, ~line_width: int, t) => string;
};
