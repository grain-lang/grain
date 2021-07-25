open Grain_parsing;

let allLocations: ref(list(Grain_parsing.Location.t)) = ref([]);

// iterate the tree to make an ordered list of locations
// we can then find expressions between locations

//

let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

let print_loc = (msg: string, loc: Grain_parsing.Location.t) => {
  let (file, line, startchar, _) = get_raw_pos_info(loc.loc_start);
  let (_, endline, endchar, _) = get_raw_pos_info(loc.loc_end);
  /*let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in*/

  if (startchar >= 0) {
    if (line == endline) {
      print_endline(
        msg
        ++ " "
        ++ string_of_int(line)
        ++ ":"
        ++ string_of_int(startchar)
        ++ ","
        ++ string_of_int(endchar),
      );
    } else {
      print_endline(
        msg
        ++ " "
        ++ string_of_int(line)
        ++ ":"
        ++ string_of_int(startchar)
        ++ " - "
        ++ string_of_int(endline)
        ++ ":"
        ++ string_of_int(endchar),
      );
    };
  };
};

let walktree = (statements: list(Grain_parsing.Parsetree.toplevel_stmt)) => {
  //let iterator = {...Ast_iterator.default_iterator,};

  let iter_location = (self, location) => {
    // print_loc("walked location:", location);
    allLocations := List.append(allLocations^, [location]);
  };

  let iterator = {...Ast_iterator.default_iterator, location: iter_location};

  List.iter(iterator.toplevel(iterator), statements);

  allLocations :=
    List.sort(
      (loc1: Grain_parsing.Location.t, loc2: Grain_parsing.Location.t) => {
        let (_, raw1l, raw1c, _) = get_raw_pos_info(loc1.loc_start);
        let (_, raw2l, raw2c, _) = get_raw_pos_info(loc2.loc_start);

        if (raw1l < raw2l) {
          (-1);
        } else if (raw1l == raw2l) {
          if (raw1c < raw2c) {
            (-1);
          } else {
            1;
          };
        } else {
          1;
        };
      },
      allLocations^,
    );
  // List.iter(l => print_loc("Sorted loc:", l), allLocations^);
};

let get_location_after = (loc: Grain_parsing.Location.t) => {
  print_loc("looking for a node afte", loc);
  let (_, raw1l, raw1c, _) = get_raw_pos_info(loc.loc_start);

  let afterloc: option(Grain_parsing.Location.t) =
    List.fold_left(
      (acc, l: Grain_parsing.Location.t) => {
        switch (acc) {
        | Some(_) => acc // nothing more to do
        | None =>
          let (_, raw2l, raw2c, _) = get_raw_pos_info(l.loc_start);

          if (raw2l > raw1l) {
            print_loc("A match", l);
            Some(l);
          } else if (raw2l == raw1l) {
            if (raw2c > raw1c) {
              print_loc("B match", l);
              Some(l);
            } else {
              None;
            };
          } else {
            None;
          };
        }
      },
      None,
      allLocations^,
    );

  switch (afterloc) {
  | None => print_endline("no afterloc")
  | Some(l) => print_loc("afterlocl", l)
  };
  afterloc;
};
