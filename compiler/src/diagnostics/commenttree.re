open Grain_parsing;

type t = list(Parsetree.comment);

let from_comments = x => x;

let query =
    (
      tree,
      {Location.loc_start: {pos_cnum: start}, loc_end: {pos_cnum: finish}},
    ) => {
  Location.(
    List.fold_left(
      (acc, comment) => {
        switch (comment) {
        | Parsetree.Line({cmt_loc})
        | Shebang({cmt_loc})
        | Block({cmt_loc})
        | Doc({cmt_loc}) =>
          let pos = cmt_loc.loc_start.pos_cnum;
          if (start <= pos && pos <= finish) {
            [comment, ...acc];
          } else {
            acc;
          };
        }
      },
      [],
      tree,
    )
  )
  |> List.rev;
};
