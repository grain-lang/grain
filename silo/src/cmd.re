open Ansi;
open Arg;

type spec =
  | Spec({
      names: list(string),
      doc: string,
      usage: string,
      value: ref('a),
      conv: conv('a),
    })
    : spec

and conv('a) =
  | UnitConv(unit => 'a)
  | StringConv(string => 'a)
  | SymbolConv(list(string), string => 'a);

type opt_spec = spec;
type arg_spec = spec;

type t = {
  options: list(opt_spec),
  args: list(arg_spec),
  name: string,
  doc: string,
  usage: string,
  action,
  commands: list(t),
  mutable parent: option(t),
}
and action =
  | Help
  | Thunk(unit => unit)
  | NoAction;

let opt = (~names, ~doc, ~usage, ~value: ref('a), ~conv: string => 'a) => {
  Spec({names, doc, usage, value, conv: StringConv(conv)});
};

let flag = (~names, ~doc, ~value: ref('a), ~conv: unit => 'a) => {
  Spec({names, doc, usage: "", value, conv: UnitConv(conv)});
};

let enum_opt =
    (~names, ~doc, ~usage, ~value: ref('a), ~enum, ~conv: string => 'a) => {
  Spec({names, doc, usage, value, conv: SymbolConv(enum, conv)});
};

let arg = (~name, ~doc, ~value: ref('a), ~conv: string => 'a) => {
  Spec({names: [name], doc, usage: "", value, conv: StringConv(conv)});
};

let create_parse_spec = (cmd: t) => {
  List.concat_map(
    spec => {
      switch (spec) {
      | Spec({names, doc, value, conv}) =>
        let arg_spec =
          switch (conv) {
          | UnitConv(conv) => Unit(() => {value := conv()})
          | StringConv(conv) => String(v => {value := conv(v)})
          | SymbolConv(enum, conv) => Symbol(enum, v => {value := conv(v)})
          };
        List.map(name => (name, arg_spec, ""), names);
      }
    },
    cmd.options,
  );
};

let bold = text => ansi(~bold=true, text);

let section_header = header => ansi(~bold=true, ~color=YellowBright, header);

let usage_text = usage => ansi(~color=BlueBright, usage);

let rec print_list = (pr, sep, ppf) =>
  fun
  | [] => ()
  | [a] => pr(ppf, a)
  | [a, ...l] => {
      pr(ppf, a);
      sep(ppf);
      print_list(pr, sep, ppf, l);
    };

let print_options =
  print_list(
    (ppf, name) =>
      Printf.bprintf(ppf, "%s", ansi(~bold=true, ~color=BlueBright, name)),
    ppf => Printf.bprintf(ppf, ", "),
  );

let print_command = (ppf, name) =>
  Printf.bprintf(ppf, "%s", ansi(~bold=true, ~color=BlueBright, name));

let rec command_name = cmd => {
  switch (cmd.parent) {
  | Some(parent) => Printf.sprintf("%s %s", command_name(parent), cmd.name)
  | None => cmd.name
  };
};

let opt_width = (Spec({names, usage})) =>
  (List.length(names) - 1)
  * 2
  + List.fold_left((acc, name) => acc + String.length(name), 0, names)
  + (usage == "" ? 0 : 1 + String.length(usage));

let max_opt_width =
  List.fold_left((acc, opt) => max(acc, opt_width(opt)), 0);

let max_command_width =
  List.fold_left((acc, {name}) => max(acc, String.length(name)), 0);

let make_help = (~buf=Buffer.create(1024), cmd) => {
  open Printf;

  bprintf(buf, "%s\n\n", cmd.doc);
  bprintf(
    buf,
    "%s %s %s\n",
    section_header("Usage:"),
    usage_text(bold(command_name(cmd))),
    usage_text(cmd.usage),
  );

  if (List.length(cmd.options) > 0) {
    bprintf(buf, "\n%s\n", section_header("Options:"));
  };
  let max_opt_width = max_opt_width(cmd.options);
  List.iter(
    (Spec({names, doc, usage}) as opt) => {
      bprintf(buf, "    %a ", print_options, names);
      if (usage != "") {
        bprintf(buf, "%s  ", usage_text(usage));
      } else {
        bprintf(buf, " ");
      };
      bprintf(buf, "%s", String.make(max_opt_width - opt_width(opt), ' '));
      bprintf(buf, "%s\n", doc);
    },
    cmd.options,
  );

  if (cmd.commands != []) {
    bprintf(buf, "\n%s\n", section_header("Commands:"));
  };
  let max_command_width = max_command_width(cmd.commands);
  List.iter(
    ({name, doc}) => {
      bprintf(buf, "    %a  ", print_command, name);
      bprintf(
        buf,
        "%s",
        String.make(max_command_width - String.length(name), ' '),
      );
      bprintf(buf, "%s\n", doc);
    },
    cmd.commands,
  );

  bprintf(
    buf,
    "\nRun %s for help with individual commands.\n",
    usage_text(bold("silo") ++ " <command> --help"),
  );

  Buffer.contents(buf);
};

let help_on_error = cmd => {
  let buf = Buffer.create(1024);
  Buffer.add_string(buf, "\n");
  make_help(~buf, cmd);
};

let make = (~name, ~doc, ~usage, ~options=[], ~args=[], ~commands=[], action) => {
  let help_action = ref(() => ());
  let help =
    flag(
      ~names=["-h", "-help", "--help"],
      ~doc="Display help",
      ~value=ref(),
      ~conv=() =>
      help_action^()
    );

  let options = [help, ...options];

  let cmd = {name, doc, usage, options, args, commands, action, parent: None};

  List.iter(subcmd => subcmd.parent = Some(cmd), commands);

  help_action :=
    (
      () => {
        print_string(make_help(cmd));
        exit(0);
      }
    );

  cmd;
};

let parse = (cmd: t) => {
  let current = ref(cmd);
  let speclist = ref(create_parse_spec(cmd));
  let positional_arguments = ref(cmd.args);

  let process_positional = positional => {
    switch (List.find_opt(({name}) => name == positional, cmd.commands)) {
    | Some(subcommand) =>
      current := subcommand;
      speclist := create_parse_spec(subcommand);
      positional_arguments := subcommand.args;
    | None =>
      switch (positional_arguments^) {
      | [] =>
        raise(
          Arg.Bad("unknown argument or subcommand '" ++ positional ++ "'"),
        )
      | [Spec({value, conv}), ...rest] =>
        positional_arguments := rest;
        switch (conv) {
        | UnitConv(conv) => value := conv()
        | StringConv(conv) => value := conv(positional)
        | SymbolConv(enum, conv) =>
          if (List.mem(positional, enum)) {
            value := conv(positional);
          } else {
            raise(
              Arg.Bad(
                Printf.sprintf(
                  "wrong argument '%s'; expected one of: %s.",
                  positional,
                  String.concat(" ", enum),
                ),
              ),
            );
          }
        };
      }
    };
  };

  let run = () => {
    Arg.parse_argv_dynamic(Sys.argv, speclist, process_positional, "");
    switch (positional_arguments^) {
    | [Spec({names}), ..._] =>
      raise(
        Arg.Bad(
          Printf.sprintf("missing required argument <%s>", List.hd(names)),
        ),
      )
    | [] => ()
    };
  };

  switch (run()) {
  | exception (Arg.Help(_)) =>
    prerr_string(make_help(current^));
    exit(0);
  | _ =>
    switch (current^.action) {
    | NoAction => ()
    | Thunk(f) => f()
    | Help => print_string(make_help(current^))
    }
  };
};

let _ =
  Printexc.register_printer(exn => {
    switch (exn) {
    | Arg.Bad(msg) => Some(String.trim(msg))
    | _ => None
    }
  });
