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

  We handle this case by allowing the lexer to do a lookahead. If it encounters
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
  lexbuf: Sedlexing.lexbuf,
  mutable queued_tokens: list(positioned(token)),
  mutable queued_exn: option(exn),
  mutable fn_ctx_stack: list(fn_ctx),
};

let init = lexbuf => {
  {lexbuf, queued_tokens: [], queued_exn: None, fn_ctx_stack: []};
};

let token = state => {
  Lexer.token(state.lexbuf);
};

let fake_triple = (t, (_, pos, _)) => (t, pos, pos);

exception Lex_balanced_failed(list(positioned(token)), option(exn));
exception Lex_fast_forward_failed(list(positioned(token)), option(exn));
exception Lex_data_typ_failed(list(positioned(token)), option(exn));

let inject_fun =
  fun
  | [tok, ...acc] => [tok, fake_triple(FUN, tok), ...acc]
  | _ => assert(false);

let is_triggering_token =
  fun
  | (THICKARROW, _, _)
  | (ARROW, _, _) => true
  | _ => false;

let rec lex_fast_forward_step = (state, stop, acc, tok) => {
  let acc = [tok, ...acc];
  switch (tok, stop) {
  | ((tok, _, _), _) when tok == stop => acc
  | ((EOF, _, _), _) => raise(Lex_fast_forward_failed(acc, None))
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
  | (EOL, _, _) as tok =>
    let acc = [tok, ...acc];
    next_non_eol_token(state, acc);
  | tok => (tok, acc)
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
  let (tok, acc') = next_non_eol_token(state, []);
  let acc =
    if (is_triggering_token(tok)) {
      acc' @ inject_fun(acc);
    } else {
      acc' @ acc;
    };
  lex_balanced_step(state, closing, acc, tok);
}

and check_data_typ = (state, closing, acc) => {
  switch (lex_data_typ(state)) {
  | exception (Lex_data_typ_failed([token, ...rest], exn)) =>
    lex_balanced_step(state, closing, rest @ acc, token)
  | [token, ...rest] =>
    let acc =
      if (is_triggering_token(token)) {
        inject_fun(acc);
      } else {
        acc;
      };
    lex_balanced_step(state, closing, rest @ acc, token);
  | _ => failwith("Impossible: wrapped_lexer check_data_typ not matched")
  };
}

and lex_balanced_step = (state, closing, acc, tok) => {
  let acc = [tok, ...acc];
  switch (tok, closing) {
  | ((RPAREN, _, _), RPAREN)
  | ((RBRACE, _, _), RBRACE)
  | ((RBRACK, _, _), RBRACK)
  | ((RCARET, _, _), RCARET) =>
    pop_fn_ctx(state);
    acc;
  | ((RPAREN | RBRACE | RBRACK | EOF, _, _), _) =>
    raise(Lex_balanced_failed(acc, None))
  | ((LBRACK | LBRACKRCARET, _, _), _) =>
    lex_balanced(
      state,
      closing,
      lex_balanced(~push=DiscoverFunctions, state, RBRACK, acc),
    )
  | ((LBRACE, _, _), _) =>
    lex_balanced(
      state,
      closing,
      lex_balanced(~push=DiscoverFunctions, state, RBRACE, acc),
    )
  | ((LPAREN, _, _), _) when ignore_fns(state) =>
    lex_balanced(
      state,
      closing,
      lex_balanced(~push=DiscoverFunctions, state, RPAREN, acc),
    )
  | ((LPAREN, _, _), _) => check_lparen_fn(state, closing, acc)
  | ((THICKARROW, _, _), _) when ignore_fns(state) =>
    // When in a context where we're not looking for toplevel functions,
    // the thing that appears immediately after an arrow could be a
    // function, so we need to check for that
    let ((tok', _, _) as triple', tokens) = next_non_eol_token(state, []);
    switch (tok') {
    | LPAREN => check_lparen_fn(state, closing, [triple', ...tokens @ acc])
    | LIDENT(_)
    | UNDERSCORE => check_id_fn(state, closing, [triple', ...tokens @ acc])
    | UIDENT(_) => check_data_typ(state, closing, [triple', ...tokens @ acc])
    | _ =>
      // Recurse normally
      lex_balanced_step(state, closing, tokens @ acc, triple')
    };
  | ((MATCH, _, _), _) =>
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
  | ((LIDENT(_) | UNDERSCORE, _, _), _) when !ignore_fns(state) =>
    check_id_fn(state, closing, acc)
  | ((UIDENT(_), p, _), _) when !ignore_fns(state) =>
    check_data_typ(state, closing, acc)
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
      let tokens = [token, ...tokens];
      if (is_triggering_token(token)) {
        state.queued_tokens = [lparen, ...List.rev(tokens)];
        fake_triple(FUN, lparen);
      } else {
        state.queued_tokens = List.rev(tokens);
        lparen;
      };
    }
  }

and lex_qualified_uid = (state, acc) => {
  switch (token(state)) {
  | exception exn => raise(Lex_data_typ_failed(acc, Some(exn)))
  | (DOT, _, _) as dot =>
    switch (token(state)) {
    | exception exn => raise(Lex_data_typ_failed(acc, Some(exn)))
    | (UIDENT(_), _, _) as uident =>
      lex_qualified_uid(state, [uident, dot, ...acc])
    | tok => raise(Lex_data_typ_failed([tok, dot, ...acc], None))
    }
  | token => [token, ...acc]
  };
}

and lex_data_typ = state => {
  switch (lex_qualified_uid(state, [])) {
  | [(LCARET, _, _), ..._] as quid_tokens =>
    switch (lex_balanced(~push=DiscoverFunctions, state, RCARET, [])) {
    | exception (Lex_balanced_failed(tokens, exn)) =>
      raise(Lex_data_typ_failed(tokens, exn))
    | brack_tokens =>
      let tokens = brack_tokens @ quid_tokens;
      switch (token(state)) {
      | exception exn => raise(Lex_data_typ_failed(tokens, Some(exn)))
      | tok => [tok, ...tokens]
      };
    }
  | tokens => tokens
  };
}

and lookahead_fun_data_typ = (state, uident) => {
  switch (lex_data_typ(state)) {
  | exception (Lex_data_typ_failed(tokens, exn)) =>
    state.queued_tokens = List.rev(tokens);
    state.queued_exn = exn;
    uident;
  | [token, ..._] as tokens when is_triggering_token(token) =>
    state.queued_tokens = [uident, ...List.rev(tokens)];
    fake_triple(FUN, uident);
  | tokens =>
    state.queued_tokens = List.rev(tokens);
    uident;
  };
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
    | (MATCH, _, _) as tok =>
      lookahead_match(state);
      tok;
    | (LPAREN, _, _) as tok => lookahead_fun(state, tok)
    | (LIDENT(_) | UNDERSCORE, _, _) as tok =>
      switch (token(state)) {
      | exception exn =>
        state.queued_exn = Some(exn);
        tok;
      | tok' =>
        if (is_triggering_token(tok')) {
          state.queued_tokens = [tok, tok'];
          fake_triple(FUN, tok);
        } else {
          state.queued_tokens = [tok'];
          tok;
        }
      }
    | (UIDENT(_), _, _) as tok => lookahead_fun_data_typ(state, tok)
    | token => token
    }
  | ([x, ...xs], _) =>
    state.queued_tokens = xs;
    x;
  };
};

let token = state => {
  // if-else, or-patterns, and `and` hack
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
    | AND
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
