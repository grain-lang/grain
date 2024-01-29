exception InvalidAttribute(string);
exception MalformedAttribute(string, string);

type param_id =
  | LabeledParam(string)
  | PositionalParam(int);

type t =
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
