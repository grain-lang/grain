open Parsetree;

let infixop = op => {
  switch (op.[0]) {
  | '+'
  | '-'
  | '*'
  | '/'
  | '%'
  | '='
  | '^'
  | '<'
  | '>'
  | '&'
  | '|'
  | '?' => true
  | _ when op == "is" => true
  | _ when op == "isnt" => true
  | _ when String.starts_with(~prefix="!=", op) => true
  | _
  | exception _ => false
  };
};

let is_infix_op = expr => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) => infixop(op)
  | _ => false
  };
};

let prefixop = op =>
  switch (op.[0]) {
  | '!' => true
  | _
  | exception _ => false
  };

let is_prefix_op = expr => {
  switch (expr.pexp_desc) {
  | PExpId({txt: Identifier.IdentName({txt: op})}) => prefixop(op)
  | _ => false
  };
};

let rec starts_with_negative_value = (~bypass_parens, expr) =>
  if (bypass_parens && expr.pexp_meta.pexpmd_in_parens) {
    false;
  } else {
    switch (expr.pexp_desc) {
    | PExpConstant(const) =>
      switch (const) {
      | PConstNumber(
          PConstNumberInt({txt}) | PConstNumberFloat({txt}) |
          PConstNumberRational({numerator: {txt}}),
        )
      | PConstInt8({txt})
      | PConstInt16({txt})
      | PConstInt32({txt})
      | PConstInt64({txt})
      | PConstUint8({txt})
      | PConstUint16({txt})
      | PConstUint32({txt})
      | PConstUint64({txt})
      | PConstFloat32({txt})
      | PConstFloat64({txt})
      | PConstWasmI32({txt})
      | PConstWasmI64({txt})
      | PConstWasmF32({txt})
      | PConstWasmF64({txt})
      | PConstBigInt({txt})
      | PConstRational({txt}) => String.starts_with(~prefix="-", txt)
      | PConstBool(_)
      | PConstVoid
      | PConstBytes(_)
      | PConstString(_)
      | PConstChar(_) => false
      }
    | PExpApp(
        func_expr,
        [{paa_label: Unlabeled, paa_expr: arg1}, {paa_label: Unlabeled}],
      )
        when is_infix_op(func_expr) =>
      starts_with_negative_value(~bypass_parens, arg1)
    | _ => false
    };
  };
