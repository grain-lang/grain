open Parser;

type positioned('a) = ('a, Lexing.position, Lexing.position);

type fn_ctx =
  | DiscoverFunctions
  | IgnoreFunctions;

type t = {
  lexbuf: Sedlexing.lexbuf,
  mutable queued_tokens: list(positioned(token)),
  mutable queued_exn: option(exn),
  mutable fn_ctx_stack: list(fn_ctx),
};

let init: Sedlexing.lexbuf => t;
let token: t => (token, Lexing.position, Lexing.position);
