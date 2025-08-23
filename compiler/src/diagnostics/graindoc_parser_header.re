open Grain_parsing.Location;

let to_loc = ((loc_start, loc_end)) => {
  {
    loc_start,
    loc_end,
    loc_ghost: false,
  };
};
