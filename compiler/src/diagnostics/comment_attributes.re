open Grain_parsing;

type param_id =
  | LabeledParam(string, Location.t)
  | PositionalParam(int, Location.t);

type t = {
  attr,
  attr_loc: Location.t,
}

and attr =
  | Param({
      attr_id: param_id,
      attr_desc: string,
    })
  | Returns({attr_desc: string})
  | Example({attr_desc: string})
  | Deprecated({attr_desc: string})
  | Since({attr_version: string})
  | History({
      attr_version: string,
      attr_desc: string,
    })
  | Throws({
      attr_type: string,
      attr_desc: string,
    });

type parsed_graindoc = (option(string), list(t));
