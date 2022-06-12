/**
  Good reading for a somewhat similar problem that occurs in C.
  https://en.wikipedia.org/wiki/Lexer_hack

  This module was inspired by ReasonML's lexer, as they too experience this
  problem: Grain's true grammar is not LR(1), but Menhir is an LR(1) parser
  generator. Thus it may seem that using Menhir is counter-intuitive, but the
  grammar only has one issue that can be solved at the lexer level. The parsers
  produced by Menhir are very fast and Menhir offers an excellent development
  experience, so we accept that we must have this lexer hack. The syntax that
  makes the grammar not LR(1) is the arrow function:

  LR grammars can't determine if the expression
      (foo, bar, baz)
  is a tuple or the beginning of the function definition
      (foo, bar, baz) => { ... }
  as the former is a comma-separated list of expressions and the latter is a
  comma-separated list of patterns. After reading `(foo`, should the parser
  reduce `expr -> foo` or `pattern -> foo`? The answer to that question lies
  many tokens ahead, at the `=>` token. Unfortunately, the LR(1) parsers only
  look one token ahead, so it doesn't know, creating an unavoidable
  reduce/reduce conflict. (You could instead reduce `id -> foo` and in your
  semantic action for the function or tuple retroactively decide whether it's
  an expression or pattern, but this only works for very simple expressions or
  patterns.)

  We handle this case by allowing the lexer to do a lookahead. It if encounters
  an open paren (or a single identifier) it will scan ahead and check if an
  arrow appears on the other side. If this is the case, the lexer will first
  report a phantom `FUN` token. This signals to the parser that the upcoming
  thing is a function, so it can treat the parameters as patterns rather than
  expressions. This lookahead is cached, so tokens are only ever read once.
 */
open Parser;

type positioned('a) = ('a, Lexing.position, Lexing.position);

type fn_ctx =
  | DiscoverFunctions
  | IgnoreFunctions;

type t = {
  lexbuf: Lexing.lexbuf,
  mutable lexbuf_p: (Lexing.position, Lexing.position),
  mutable queued_tokens: list(positioned(token)),
  mutable queued_exn: option(exn),
  mutable fn_ctx_stack: list(fn_ctx),
};

let init = lexbuf => {
  {
    lexbuf,
    lexbuf_p: (lexbuf.lex_start_p, lexbuf.lex_curr_p),
    queued_tokens: [],
    queued_exn: None,
    fn_ctx_stack: [],
  };
};

let lexbuf = state => state.lexbuf;

let token = state => {
  Lexer.token(state.lexbuf);
};

let save_triple = (lexbuf, tok) => (
  tok,
  lexbuf.Lexing.lex_start_p,
  lexbuf.Lexing.lex_curr_p,
);

let fake_triple = (t, (_, pos, _)) => (t, pos, pos);

exception Lex_balanced_failed(list(positioned(token)), option(exn));
exception Lex_fast_forward_failed(list(positioned(token)), option(exn));

let inject_fun =
  fun
  | [tok, ...acc] => [tok, fake_triple(FUN, tok), ...acc]
  | _ => assert(false);

let is_triggering_token =
  fun
  | THICKARROW
  | ARROW => true
  | _ => false;

let rec lex_fast_forward_step = (state, stop, acc, tok) => {
  let lexbuf = state.lexbuf;
  let acc = [save_triple(lexbuf, tok), ...acc];
  switch (tok, stop) {
  | _ when tok == stop => acc
  | (EOF, _) => raise(Lex_fast_forward_failed(acc, None))
  | _ => lex_fast_forward(state, stop, acc)
  };
}

and lex_fast_forward = (state, stop, acc) =>
  switch (token(state)) {
  | exception exn => raise(Lex_fast_forward_failed(acc, Some(exn)))
  | tok => lex_fast_forward_step(state, stop, acc, tok)
  };

let rec next_non_eol_token = (state, acc) => {
  switch (token(state)) {
  | exception exn => raise(Lex_balanced_failed(acc, Some(exn)))
  | EOL as tok =>
    let lexbuf = state.lexbuf;
    let acc = [save_triple(lexbuf, tok), ...acc];
    next_non_eol_token(state, acc);
  | tok =>
    let lexbuf = state.lexbuf;
    (save_triple(lexbuf, tok), acc);
  };
};

let push_fn_ctx = (state, ctx) => {
  state.fn_ctx_stack = [ctx, ...state.fn_ctx_stack];
};

let pop_fn_ctx = state => {
  switch (state.fn_ctx_stack) {
  | [hd, ...tl] => state.fn_ctx_stack = tl
  | _ => assert(false)
  };
};

let ignore_fns = state => {
  switch (state.fn_ctx_stack) {
  | [IgnoreFunctions, ...tl] => true
  | _ => false
  };
};

let rec check_lparen_fn = (state, closing, acc) => {
  let rparen =
    try(lex_balanced(~push=DiscoverFunctions, state, RPAREN, [])) {
    | Lex_balanced_failed(rparen, None) =>
      raise(Lex_balanced_failed(rparen @ acc, None))
    };

  switch (token(state)) {
  | exception exn => raise(Lex_balanced_failed(rparen @ acc, Some(exn)))
  | tok' =>
    let acc =
      if (is_triggering_token(tok')) {
        inject_fun(acc);
      } else {
        acc;
      };
    lex_balanced_step(state, closing, rparen @ acc, tok');
  };
}

and check_id_fn = (state, closing, acc) => {
  let ((tok, _, _), acc') = next_non_eol_token(state, []);
  let acc =
    if (is_triggering_token(tok)) {
      acc' @ inject_fun(acc);
    } else {
      acc' @ acc;
    };
  lex_balanced_step(state, closing, acc, tok);
}

and lex_balanced_step = (state, closing, acc, tok) => {
  let lexbuf = state.lexbuf;
  let acc = [save_triple(lexbuf, tok), ...acc];
  switch (tok, closing) {
  | (RPAREN, RPAREN)
  | (RBRACE, RBRACE)
  | (RBRACK, RBRACK) =>
    pop_fn_ctx(state);
    acc;
  | (RPAREN | RBRACE | RBRACK | EOF, _) =>
    raise(Lex_balanced_failed(acc, None))
  | (LBRACK | LBRACKRCARET, _) =>
    lex_balanced(
      state,
      closing,
      lex_balanced(~push=DiscoverFunctions, state, RBRACK, acc),
    )
  | (LBRACE, _) =>
    lex_balanced(
      state,
      closing,
      lex_balanced(~push=DiscoverFunctions, state, RBRACE, acc),
    )
  | (LPAREN, _) when ignore_fns(state) =>
    lex_balanced(
      state,
      closing,
      lex_balanced(~push=DiscoverFunctions, state, RPAREN, acc),
    )
  | (LPAREN, _) => check_lparen_fn(state, closing, acc)
  | (THICKARROW, _) when ignore_fns(state) =>
    // When in a context where we're not looking for toplevel functions,
    // the thing that appears immediately after an arrow could be a
    // function, so we need to check for that
    let ((tok', _, _) as triple', tokens) = next_non_eol_token(state, []);
    switch (tok') {
    | LPAREN => check_lparen_fn(state, closing, [triple', ...tokens @ acc])
    | ID(_)
    | UNDERSCORE => check_id_fn(state, closing, [triple', ...tokens @ acc])
    | _ =>
      // Recurse normally
      lex_balanced_step(state, closing, tokens @ acc, tok')
    };
  | (MATCH, _) =>
    lex_balanced(
      state,
      closing,
      lex_balanced(
        ~push=IgnoreFunctions,
        state,
        RBRACE,
        lex_fast_forward(
          state,
          LBRACE,
          lex_balanced(
            ~push=DiscoverFunctions,
            state,
            RPAREN,
            lex_fast_forward(state, LPAREN, acc),
          ),
        ),
      ),
    )
  | (ID(_) | UNDERSCORE, _) when !ignore_fns(state) =>
    check_id_fn(state, closing, acc)
  | _ => lex_balanced(state, closing, acc)
  };
}

and lex_balanced = (~push=?, state, closing, acc) => {
  Option.iter(push_fn_ctx(state), push);
  switch (token(state)) {
  | exception exn => raise(Lex_balanced_failed(acc, Some(exn)))
  | tok => lex_balanced_step(state, closing, acc, tok)
  };
}

and lookahead_fun = (state, (tok, _, _) as lparen) =>
  switch (lex_balanced(~push=DiscoverFunctions, state, RPAREN, [])) {
  | exception (Lex_balanced_failed(tokens, exn)) =>
    state.queued_tokens = List.rev(tokens);
    state.queued_exn = exn;
    lparen;
  | tokens =>
    switch (token(state)) {
    | exception exn =>
      state.queued_tokens = List.rev(tokens);
      state.queued_exn = Some(exn);
      lparen;
    | token =>
      let tokens = [save_triple(state.lexbuf, token), ...tokens];
      if (is_triggering_token(token)) {
        state.queued_tokens = [lparen, ...List.rev(tokens)];
        fake_triple(FUN, lparen);
      } else {
        state.queued_tokens = List.rev(tokens);
        lparen;
      };
    }
  }

and lookahead_match = state => {
  switch (lex_fast_forward(state, LPAREN, [])) {
  | exception (Lex_fast_forward_failed(tokens, exn)) =>
    state.queued_tokens = List.rev(tokens);
    state.queued_exn = exn;
  | first_ff_tokens =>
    switch (lex_balanced(~push=DiscoverFunctions, state, RPAREN, [])) {
    | exception (Lex_balanced_failed(tokens, exn)) =>
      state.queued_tokens = List.rev(tokens @ first_ff_tokens);
      state.queued_exn = exn;
    | tokens =>
      switch (lex_fast_forward(state, LBRACE, [])) {
      | exception exn =>
        state.queued_tokens = List.rev(tokens @ first_ff_tokens);
        state.queued_exn = Some(exn);
      | second_ff_tokens =>
        switch (lex_balanced(~push=IgnoreFunctions, state, RBRACE, [])) {
        | exception (Lex_balanced_failed(more_tokens, exn)) =>
          state.queued_tokens =
            List.rev(
              more_tokens @ second_ff_tokens @ tokens @ first_ff_tokens,
            );
          state.queued_exn = exn;
        | more_tokens =>
          state.queued_tokens =
            List.rev(
              more_tokens @ second_ff_tokens @ tokens @ first_ff_tokens,
            )
        }
      }
    }
  };
};

let token = state => {
  let lexbuf = state.lexbuf;
  switch (state.queued_tokens, state.queued_exn) {
  | ([], Some(exn)) =>
    state.queued_exn = None;
    raise(exn);
  | ([(MATCH, _, _) as match], None) =>
    lookahead_match(state);
    match;
  | ([(LPAREN, _, _) as lparen], None) => lookahead_fun(state, lparen)
  | ([], None) =>
    switch (token(state)) {
    | MATCH as tok =>
      let tok = save_triple(state.lexbuf, tok);
      lookahead_match(state);
      tok;
    | LPAREN as tok => lookahead_fun(state, save_triple(state.lexbuf, tok))
    | (ID(_) | UNDERSCORE) as tok =>
      let tok = save_triple(lexbuf, tok);
      switch (token(state)) {
      | exception exn =>
        state.queued_exn = Some(exn);
        tok;
      | tok' =>
        if (is_triggering_token(tok')) {
          state.queued_tokens = [tok, save_triple(lexbuf, tok')];
          fake_triple(FUN, tok);
        } else {
          state.queued_tokens = [save_triple(lexbuf, tok')];
          tok;
        }
      };
    | token => save_triple(lexbuf, token)
    }
  | ([x, ...xs], _) =>
    state.queued_tokens = xs;
    x;
  };
};

let save_lexer_positions = state => {
  state.lexbuf_p = (state.lexbuf.lex_start_p, state.lexbuf.lex_curr_p);
};
let set_lexer_positions = (state, lex_start_p, lex_curr_p) => {
  state.lexbuf.lex_start_p = lex_start_p;
  state.lexbuf.lex_curr_p = lex_curr_p;
};
let restore_lexer_positions = state => {
  let (lex_start_p, lex_curr_p) = state.lexbuf_p;
  set_lexer_positions(state, lex_start_p, lex_curr_p);
};

let token = state => {
  // if-else and or-patterns hack
  let (tok, _, _) as triple = token(state);
  if (tok == EOL) {
    let fst = ((a, _, _)) => a;
    let next_triple = ref(triple);
    let acc = ref([]);
    while (fst(next_triple^) == EOL) {
      acc := [next_triple^, ...acc^];
      next_triple := token(state);
    };

    switch (fst(next_triple^)) {
    | ELSE
    | PIPE => next_triple^
    | _ =>
      state.queued_tokens =
        List.tl(
          List.rev_append([next_triple^, ...acc^], state.queued_tokens),
        );
      triple;
    };
  } else {
    triple;
  };
};

let token = state => {
  // Menhir will read lexer positions to determine token locations
  // We spoof these locations and reset when we need to lex the next token
  restore_lexer_positions(state);
  let (token, start_p, curr_p) = token(state);
  save_lexer_positions(state);
  set_lexer_positions(state, start_p, curr_p);
  token;
};
