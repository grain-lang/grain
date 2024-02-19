open Parsetree;

type hooks = {
  enter_location: Location.t => unit,
  leave_location: Location.t => unit,
  enter_attribute: attribute => unit,
  leave_attribute: attribute => unit,
  enter_parsed_program: parsed_program => unit,
  leave_parsed_program: parsed_program => unit,
  enter_include: include_declaration => unit,
  leave_include: include_declaration => unit,
  enter_provide: list(provide_item) => unit,
  leave_provide: list(provide_item) => unit,
  enter_foreign: (provide_flag, value_description) => unit,
  leave_foreign: (provide_flag, value_description) => unit,
  enter_primitive: (provide_flag, primitive_description) => unit,
  leave_primitive: (provide_flag, primitive_description) => unit,
  enter_top_let:
    (provide_flag, rec_flag, mut_flag, list(value_binding)) => unit,
  leave_top_let:
    (provide_flag, rec_flag, mut_flag, list(value_binding)) => unit,
  enter_module: (provide_flag, module_declaration) => unit,
  leave_module: (provide_flag, module_declaration) => unit,
  enter_pattern: pattern => unit,
  leave_pattern: pattern => unit,
  enter_expression: expression => unit,
  leave_expression: expression => unit,
  enter_type: parsed_type => unit,
  leave_type: parsed_type => unit,
  enter_toplevel_stmt: toplevel_stmt => unit,
  leave_toplevel_stmt: toplevel_stmt => unit,
  enter_constant: constant => unit,
  leave_constant: constant => unit,
  enter_let: (rec_flag, mut_flag, list(value_binding)) => unit,
  leave_let: (rec_flag, mut_flag, list(value_binding)) => unit,
  enter_value_binding: value_binding => unit,
  leave_value_binding: value_binding => unit,
  enter_data_declarations:
    list((provide_flag, data_declaration, Location.t)) => unit,
  leave_data_declarations:
    list((provide_flag, data_declaration, Location.t)) => unit,
  enter_data_declaration: data_declaration => unit,
  leave_data_declaration: data_declaration => unit,
};

let iter_parsed_program: (hooks, parsed_program) => unit;

let default_hooks: hooks;
